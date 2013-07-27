-- | Some functions on directed graphs.
module Language.Coatl.Graph where
-- base
import Data.Maybe
-- containers
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
-- mtl
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS

newtype Graph k = Graph
  (Map k (Set k))
  deriving
  ( Eq
  , Show
  )

-- | Create a graph from a list of connections.
connections :: Ord k => [(k, [k])] -> Graph k
connections = Graph . M.fromList . map (fmap S.fromList)

-- | Check if there exists a path between two nodes in the graph.
-- 
--  Note that this does not consider a path to exist between a
--  node and itself unless there is an explicit connection between them.
path :: Ord k => Graph k -> k -> k -> Bool
path (Graph g) s e = evalState (loop g e s) (S.fromList . M.keys $ g)
  where
    anyM :: Monad m => [m Bool] -> m Bool
    anyM [] = return False
    anyM (a : as) = a >>= \x -> if x then return x else anyM as
    loop :: Ord k => Map k (Set k) -> k -> k -> State (Set k) Bool
    loop m t k = get >>= \unvisited -> do
      -- Remove this from "unvisited".
      put $ S.delete k unvisited
      -- Get all the neighbors.
      let neighbors = fromMaybe S.empty $ M.lookup k m
      -- If the target is in here, great!
      if S.member t neighbors then return True else do
        -- Figure out which neighbors have not been checked yet.
        let toCheck = S.intersection neighbors unvisited
        -- If this is empty, there is no path here. If not,
        -- check all the unchecked neighbors.
        if S.null toCheck then return False else
          anyM . map (loop m t) $ S.toList toCheck

-- | Find all the cycles in a 'Graph k'. This is a modification
--   of Tarjan's algorithm for finding strongly-connected components.
cycles :: Ord k => Graph k -> [[k]]
cycles (Graph g) = snd $ execRWS
  (mapM_ (each' g) (M.keys g)) [] S.empty where
    each' m k = get >>= \visited ->  do
      -- This thing has been visited.
      modify $ S.insert k
      -- Check whether this thing is in the stack already.
      stack <- ask
      case span (/= k) stack of
        -- If it isn't...
        (_, []) -> if S.member k visited
          -- ...and this is not the first time visiting this, return.
          then return ()
          -- ...and this is the first time visiting this, push it to
          -- the stack and try each of its neighbors.
          else local (k :) $ mapM_ (each' m)
            (maybe [] S.toList . M.lookup k $ m)
        -- Write the cycle we found.
        (as, _) -> tell [k : reverse as]

-- | Topologically sort a 'Graph k'. This may fail with a list of
--   cycles if the graph is cyclic.
sort :: Ord k => Graph k -> Either [[k]] [Set k]
sort g = let cs = cycles g in
  if not $ null cs then Left cs
    else Right . snd $ execRWS new g S.empty
  where
    -- Find the nodes only depending on the given set of nodes.
    only :: Ord k => Graph k -> Set k -> Set k
    only (Graph n) s = S.fromList . map fst
      . filter ((`S.isSubsetOf` s) . snd) $ M.toList n
    -- Find the nodes that only depend on the nodes already
    -- checked and that have not been already checked.
    new = get >>= \already -> ask >>= \g'->
      let as = only g' already `S.difference` already in
        if S.null as then return ()
          else put (as `S.union` already) >> tell [as] >> new

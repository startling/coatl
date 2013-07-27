-- | Some functions on directed graphs.
module Language.Coatl.Graph where
-- base
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Traversable as T
-- containers
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
-- mtl
import Control.Monad.State
import Control.Monad.Writer
-- lens
import Control.Lens

import Debug.Trace

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
path (Graph m) k t = evalState (loop m t k) (S.fromList . M.keys $ m)
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
cycles :: Show k => Ord k => Graph k -> [[k]]
cycles (Graph m) = execWriter $ runStateT
  (mapM_ (each' m) (M.keys m)) ([], S.empty) where
    each' m k = get >>= \(stack, visited) ->  do
      -- This thing has been visited.
      _2 %= S.insert k
      -- Check whether this thing is in the stack already.
      case span (/= k) stack of
        -- If it isn't...
        (as, []) -> if S.member k visited
          -- ...and this is not the first time visiting this, return.
          then return ()
          -- ...and this is the first time visiting this, push it to
          -- the stack and try each of its neighbors.
          else _1 %= (k :) >> mapM_ (each' m)
            (maybe [] S.toList . M.lookup k $ m)
            -- If the thing we pushed is still on top of the stack,
            -- take it off.
            >> _1 %= (\s -> case s of
              [] -> []
              (b : bs) -> if b == k then bs else b : bs)
        -- Shorten the stack and write the cycle.
        (as, (_:bs)) -> _1 .= bs >> tell [k : reverse as]

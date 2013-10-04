{-# Language Rank2Types #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
-- | Some functions on directed graphs.
module Language.Coatl.Extra.Graph where
-- base
import Data.Foldable
import Control.Applicative
import Control.Arrow
-- containers
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
-- mtl
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS
-- lens
import Control.Lens
-- monad-loops
import Control.Monad.Loops

-- | A graph is represented by a 'Map' of identifiers to some
--   node type and by a fold on the node type over the identifiers
--   with that are pointed to from that node.
data Graph k n = Graph
  { arcs  :: Fold n k
  , nodes :: Map k n
  }

instance Foldable (Graph k) where
  foldMap f (Graph _ n) = foldMap f n

instance FoldableWithIndex k (Graph k) where
  ifoldMap f (Graph _ n) = ifoldMap f n

instance (Show k, Show n) => Show (Graph k n) where
  show g = "connections ["
    ++ iconcatMap (\k v -> show (k, v)) g
    ++ "]"

-- | Create a 'Graph' from some list of nodes along with a
--   function getting an identifier from a node and a fold over
--   the identifiers of other nodes pointing from a node.
connections :: Ord k => (n -> k) -> Fold n k -> [n] -> Graph k n
connections c f = Graph f . M.fromList . map (c &&& id)

-- | Create a graph from an association list of identifiers to
--   some 'Foldable' of connected identifiers.
associations :: (Ord k, Foldable f) => [(k, f k)] -> Graph k (k, f k)
associations = connections fst (_2 . folded)

-- | Fold over the IDs of the nodes pointed to by the node at some ID.
next :: Ord k => Graph k n -> Fold k k
next (Graph a n) f k = maybe (pure k) (k <$)
  . fmap (a f) . M.lookup k $ n

-- | Check if there exists a path between two nodes in the graph.
-- 
--  Note that this does not consider a path to exist between a
--  node and itself unless there is an explicit connection between them.
path :: Ord k => Graph k a -> k -> k -> Bool
path g s e = evalState (looping s) . S.fromList
  . toListOf (ifolded . asIndex) $ g where
    -- Look through all the next of a node to tell whether
    -- there exists a path from the node to the target.
    looping k = get >>= \unvisited -> do
      -- Remove this from the set of unvisited nodes.
      put $ S.delete k unvisited
      -- If the target is in here, then great!
      if elemOf (next g) e k then return True else do
        -- Otherwise, check if there exists a path from any
        -- neighbor we have not checked before.
        anyM looping . (`toListOf` k)
          $ filtered (`S.member` unvisited) . next g

-- | Find all the cycles in a 'Graph k'. This is a modification
--   of Tarjan's algorithm for finding strongly-connected components.
cycles :: Ord k => Graph k a -> [[k]]
cycles g  = snd $ execRWS
  (mapMOf_ (ifolded . asIndex) each' g) [] S.empty where
    each' k = get >>= \visited ->  do
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
          -- the stack and try each of its next.
          else local (k :) $ mapMOf_ (next g) each' k
        -- Write the cycle we found.
        (as, (top : _)) -> tell [top : reverse as]

-- | Topologically sort a 'Graph k'. This may fail with a list of
--   cycles if the graph is cyclic.
sort :: Ord k => Graph k a -> Either [[k]] [[a]]
sort g = let cs = cycles g in
  if not $ null cs then Left cs
    else Right . snd $ execRWS new g S.empty
  where
    -- Fold over the identifiers whose nodes only point to
    -- the given identifiers and that are not one of them.
    only' :: Ord k => Set k -> IndexedFold k (Graph k n) n
    only' s f gr@(Graph a ns) = (gr <$) . flip ifolded gr . Indexed
      $ \i n -> if i `S.notMember` s && allOf a
        (\x -> x `S.member` s || x `M.notMember` ns) n
          then indexed f i n
          else pure n
    -- Find the nodes that only depend on the nodes already
    -- checked and that have not been already checked.
    new = get >>= \already -> let
      as = itoListOf (only' already) g in
          if null as then return () else
            put (S.fromList (map fst as) `S.union` already)
              >> tell [map snd as] >> new


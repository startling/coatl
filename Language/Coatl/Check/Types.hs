{-# Language FlexibleContexts #-}
module Language.Coatl.Check.Types where
-- base
import Text.Printf
-- containers
import Data.Map (Map)
import qualified Data.Map as M
-- transformers
import Control.Monad.Error
-- mtl
import Control.Monad.Reader
-- lens
import Control.Lens
-- coatl
import Language.Coatl.Abstract
import Language.Coatl.Check.Environment
import Language.Coatl.Check (Canonical(..))

-- | Infer the type of an 'Inferrable' term.
infer ::
  ( Ord v, Show v
  , MonadError [String] m )
  => Inferrable b v -> ReaderT (Environment a v) m (Checkable a v)
infer (IReference _ v) = view (types . at v)
  >>= maybe report return where
    report :: MonadError [String] m => m a
    report = throwError
      [printf "Symbol not in scope: \"%s\"" (show v)]
infer (IApplication f ar) = view named >>= \nd -> infer f >>= \ft ->
    case preview (_CInferrable . binary Function nd) ft of
      Just (_, a, b) -> check ar a >> return b
      Nothing -> case preview
        (_CInferrable . binary Dependent nd) ft of
          Just (_, _, _) -> throwError
            [printf "(~) unimplemented as of yet"]
          Nothing -> report
  where
    report :: MonadError [String] m => m a
    report = throwError
      [printf "Expected a function type."]

-- | Check the type of some 'Checkable' term.
check ::
  ( Ord v, Show v
  , MonadError [String] m )
  => Checkable a v -> Checkable b v -> ReaderT (Environment b v) m ()
check (CLambda _ l) t = view named
  >>= \nd -> case preview (_CInferrable . binary Function nd) t of
    Just (_, a, b) -> withReaderT (add a . lower) $ check l (fmap Just b)
    Nothing -> case preview (_CInferrable . binary Dependent nd) t of
      Nothing -> throwError ["Expected a function type"]
      Just (_, a, b) -> throwError
        [printf "(~) unimplemented as of yet"]
  where
    lower :: Ord v => Environment a v -> Environment a (Maybe v)
    lower (Environment n ts) = Environment (_Just . n)
      . M.mapKeys Just . M.map (fmap Just) $ ts
    add :: Ord v => Checkable a v -> Environment a (Maybe v)
      -> Environment a (Maybe v)
    add c = set (types . at Nothing) (Just $ fmap Just c)
check (CInferrable i) t = infer i >>= \it -> if it `equivalent` t
  then return () else throwError ["Type mismatch."] where

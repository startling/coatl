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
import Language.Coatl.Parse.Syntax
import Language.Coatl.Check.Abstract
import Language.Coatl.Check.Environment

-- | Infer the type of an 'Infer' term.
infer ::
  ( Ord v, Show v
  , MonadError [String] m )
  => Infer b v -> ReaderT (Environment a v) m (Check a v)
infer (IReference _ v) = view (types . at v)
  >>= maybe report return where
    report :: MonadError [String] m => m a
    report = throwError
      [printf "Symbol not in scope: \"%s\"" (show v)]
infer (IApplication f ar) = view named >>= \nd -> infer f >>= \ft ->
    case preview (_CInfer . binary nd) ft of
      Just (Function, _, a, b) -> check ar a >> return b
      Just (Dependent, _, _, _) -> throwError
        [printf "(~) unimplemented as of yet"]
      _ -> report
  where
    report :: MonadError [String] m => m a
    report = throwError
      [printf "Expected a function type."]

-- | Check the type of some 'Check' term.
check ::
  ( Ord v, Show v
  , MonadError [String] m )
  => Check a v -> Check b v -> ReaderT (Environment b v) m ()
check (CLambda _ l) t = view named
  >>= \nd -> case preview (_CInfer . binary nd) t of
    Just (Function, _, a, b) -> with a $ check l (fmap Just b)
    Just (Dependent, _, _, _) -> throwError
      [printf "(~) unimplemented as of yet"]
    _ -> throwError ["Expected a function type"]
check (CInfer i) t = infer i >>= \it -> if it `equivalent` t
  then return () else throwError ["Type mismatch."] where

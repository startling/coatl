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
import Language.Coatl.Syntax
import Language.Coatl.Evaluate
import Language.Coatl.Check.Environment

-- | Infer the type of an 'Infer' term.
infer ::
  ( Ord v, Show v
  , MonadError [String] m )
  => Infer b v -> ReaderT (Checking a v) m (Value v)
infer (IReference _ v) = view (environment . types . at v)
  >>= maybe report return where
    report :: MonadError [String] m => m a
    report = throwError
      [printf "Symbol not in scope: \"%s\"" (show v)]
infer (IApplication f ar) = view named >>= \nd ->
  infer f >>= \ft ->
    case preview (binary nd) ft of
      Just (Function, a, b) -> check ar a >> return b
      Just (Dependent, a, b) -> check ar a
        >> (reduce . Applied b) `liftM`
          magnify (environment . definitions) (evaluate ar)
      _ -> throwError
        [printf "Expected a function type: %s" (show ft)]

-- | Check the type of some 'Check' term.
check ::
  ( Ord v, Show v
  , MonadError [String] m )
  => Check a v -> Value v -> ReaderT (Checking b v) m ()
check (CLambda _ l) t = view named
  >>= \nd -> case preview (binary nd) t of
    -- If we're checking this lambda form as a function,
    -- check that the function body type-checks as the result type
    -- with a parameter of the argument type.
    Just (Function, a, b) -> with a $ check l (fmap Just b)
    Just (Dependent, a, b) -> with a $ check l
      (reduce $ Applied (fmap Just b) (Construct Nothing))
    a -> throwError
      [printf "Expected a function type: %s" (show t)]
check (CInfer i) t = infer i >>= \it -> if it == t
  then return () else throwError ["Type mismatch."] where

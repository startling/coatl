{-# Language FlexibleContexts #-}
module Language.Coatl.Check.Types
  ( Checking (..)
  , named
  , environment
  , infer
  , check
  ) where
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
-- ansi-wl-pprint
import Text.PrettyPrint.ANSI.Leijen
-- coatl
import Language.Coatl.Abstract
import Language.Coatl.Evaluate

data Checking a v = Checking
  { _named       :: APrism' v Canonical
    -- ^ A prism into the 'Canonical' in the symbol type.
  , _environment :: Environment a v
    -- ^ The environment.
  }
makeLenses ''Checking

-- | Run a checking action in an environment with something new
--   as 'Nothing'.
with :: Ord v
  => Term () v
  -> ReaderT (Checking a (Maybe v)) m b
  -> ReaderT (Checking a v) m b
with a = withReaderT (set (environment . types . at Nothing)
  (Just . fmap Just $ a) . lower) where
    lower :: Ord v => Checking a v -> Checking a (Maybe v)
    lower (Checking n (Environment ts ds)) = Checking (_Just . n)
      $ Environment
        (M.mapKeys Just . M.map (fmap Just) $ ts)
        (M.mapKeys Just . M.map (fmap Just) $ ds)

-- | Infer the type of an 'Infer' term.
infer ::
  ( Ord v, Show v
  , MonadError Doc m )
  => Infer b v -> ReaderT (Checking a v) m (Term () v)
infer (IReference _ v) = view (environment . types . at v)
  >>= maybe report return where
    report :: MonadError Doc m => m a
    report = throwError . text $
      printf "Symbol not in scope: \"%s\"" (show v)
infer (IApplication f ar) = view named >>= \nd ->
  infer f >>= \ft ->
    case preview (binary nd) ft of
      Just (Function, a, b) -> check ar a >> return b
      Just (Dependent, a, b) -> check ar a
        >> (reduce . Applied b) `liftM`
          magnify (environment . definitions) (evaluate ar)
      _ -> throwError . text
        $ printf "Expected a function type: %s" (show ft)

-- | Check the type of some 'Check' term.
check ::
  ( Ord v, Show v
  , MonadError Doc m )
  => Check a v -> Term () v -> ReaderT (Checking b v) m ()
check (CLambda _ l) t = view named
  >>= \nd -> case preview (binary nd) t of
    -- If we're checking this lambda form as a function,
    -- check that the function body type-checks as the result type
    -- with a parameter of the argument type.
    Just (Function, a, b) -> with a $ check l (fmap Just b)
    Just (Dependent, a, b) -> with a $ check l
      (reduce $ Applied (fmap Just b) (Reference () Nothing))
    a -> throwError . text
      $ printf "Expected a function type: %s" (show t)
check (CInfer i) t = infer i >>= \it -> if it == t
  then return () else throwError . text $ "Type mismatch."

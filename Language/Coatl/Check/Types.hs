{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell #-}
-- | Check and infer' coatl types.
module Language.Coatl.Check.Types
  ( infer
  , check
  ) where
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

-- | Run a check'ing action in an environment with something new
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

-- | Infer the type of an inferrable term given a 'Checking'.
infer' ::
  ( Ord v, Pretty v
  , MonadError Doc m )
  => Term b v -> ReaderT (Checking a v) m (Term () v)
infer' (Reference _ v) = view (environment . types . at v)
  >>= maybe report return where
    report :: MonadError Doc m => m a
    report = throwError $
      text "Symbol not in scope:" <+> pretty v
infer' (Applied _ f ar) = view named >>= \nd ->
  infer' f >>= \ft ->
    case preview (binary nd) ft of
      Just (Function, a, b) -> check' ar a >> return b
      Just (Dependent, a, b) -> check' ar a
        >> (reduce . Applied () b) `liftM`
          magnify (environment . definitions) (evaluate ar)
      _ -> throwError . text
        $ "Expected a function type."
infer' otherwise = throwError $ text "Type is not infer'rable."

-- | Check the type of some checkable term given a 'Checking'.
check' ::
  ( Ord v, Pretty v
  , MonadError Doc m )
  => Term a v -> Term () v -> ReaderT (Checking b v) m ()
check' (Lambda _ l) t = view named
  >>= \nd -> case preview (binary nd) t of
    -- If we're check'ing this lambda form as a function,
    -- check' that the function body type-check's as the result type
    -- with a parameter of the argument type.
    Just (Function, a, b) -> with a $ check' l (fmap Just b)
    Just (Dependent, a, b) -> with a $ check' l
      (reduce $ Applied () (fmap Just b) (Reference () Nothing))
    a -> throwError . text
      $ "Expected a function type."
check' other t = infer' other >>= \it -> if it == t
  then return () else throwError . text $ "Type mismatch."

-- | Infer the type of an inferrable term.
infer :: MonadError Doc m => Environment a Canonical
  -> Term b Canonical -> m (Term () Canonical)
infer e t = runReaderT (infer' t) (Checking id e)

-- | Check the type of a checkable term.
check :: MonadError Doc m => Environment a Canonical
  -> Term b Canonical -> Term () Canonical -> m ()
check e t ty = runReaderT (check' t ty) (Checking id e)


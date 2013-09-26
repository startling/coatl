{-# Language FlexibleContexts #-}
{-# Language DeriveFunctor #-}
-- | Evaluate coatl terms to normal form.
module Language.Coatl.Evaluate where
-- base
import Control.Monad
import Data.Monoid
import Data.Traversable as T
-- transformers
import Control.Monad.Error
-- mtl
import Control.Monad.Reader
-- containers
import Data.Map (Map)
import qualified Data.Map as M
-- bifunctors
import Data.Bifunctor
-- lens
import Control.Lens
-- ansi-wl-pprint
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
-- coatl
import Language.Coatl.Abstract

-- | Evaluate some checkable term to normal form, given
--   a 'Map' of the (normal) values of things it might reference.
evaluate ::
  ( MonadReader (Map s (Term () v)) m
  , MonadError Doc m
  , Pretty s
  , Ord s
  ) => Term a s -> m (Term () v)
evaluate = liftM (reduce . join) . T.mapM find . first (const ()) where
  find v = M.lookup v `liftM` ask >>= \c -> case c of
    Nothing -> throwError
      $ text "Symbol not in scope during evaluation:"
      <+> pretty v
    Just v' -> return v'

-- | Evaluate some term to normal form.
reduce :: Term () n -> Term () n
reduce (Reference _ n) = Reference () n
reduce (Lambda _ e) = Lambda () $ reduce e
reduce (Applied _ a b) = let b' = reduce b in case reduce a of
  Lambda () e -> reduce $ e >>= maybe b' (Reference ())
  elsewise -> Applied () elsewise $ b'


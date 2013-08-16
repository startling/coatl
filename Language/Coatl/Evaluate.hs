{-# Language FlexibleContexts #-}
{-# Language DeriveFunctor #-}
-- | Evaluate coatl terms to normal form.
module Language.Coatl.Evaluate where
-- base
import Control.Applicative
import Control.Monad
import Data.Maybe
import Text.Printf
-- transformers
import Control.Monad.Error
-- mtl
import Control.Monad.Reader
-- containers
import Data.Map (Map)
import qualified Data.Map as M
-- lens
import Control.Lens
-- ansi-wl-pprint
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
-- coatl
import Language.Coatl.Abstract

-- | Evaluate some checkable term to normal form, given
--   a 'Map' of the (normal) values of things it might reference.
evaluate ::
  ( Ord v, Pretty v
  , MonadError Doc m )
  => Term a v -> ReaderT (Map v (Term () n)) m (Term () n)
evaluate (Lambda _ n) = Lambda () `liftM` withReaderT
  (set (at Nothing) (Just $ Reference () Nothing)
  . M.mapKeys Just . fmap (fmap Just)) (evaluate n)
evaluate (Reference _ v) = view (at v) >>= flip maybe return
  (throwError $
    text "Symbol not in scope (during evaluation):" <+> pretty v)
evaluate (Applied _ f a) = evaluate f
  >>= \f' -> evaluate a >>= \a' -> return $ case f' of
    Lambda () n    -> reduce $ n >>= maybe a' (Reference ())
    Reference () c -> Applied () (Reference () c) a'
    Applied () a b -> Applied () (Applied () a b) a'

-- | Evaluate some term to normal form.
reduce :: Term () n -> Term () n
reduce (Reference () n) = Reference () n
reduce (Lambda () e) = Lambda () $ reduce e
reduce (Applied () a b) = let b' = reduce b in
  case reduce a of
    Lambda () e -> e >>= maybe b' (Reference ())
    elsewise -> Applied () elsewise b'


{-# Language FlexibleContexts #-}
{-# Language DeriveFunctor #-}
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
import Text.PrettyPrint.ANSI.Leijen (Doc, text)
-- coatl
import Language.Coatl.Abstract

evaluate ::
  ( Ord v, Show v
  , MonadError Doc m )
  => Check a v -> ReaderT (Map v (Term () n)) m (Term () n)
evaluate (CLambda _ n) = Lambda () `liftM` withReaderT
  (set (at Nothing) (Just $ Reference () Nothing)
  . M.mapKeys Just . fmap (fmap Just)) (evaluate n)
evaluate (CInfer (IReference _ v)) = view (at v) >>= flip maybe return
  (throwError . text $
    printf "Symbol not in scope during evaluation: \"%s\"." (show v))
evaluate (CInfer (IApplication f a)) = evaluate (CInfer f)
  >>= \f' -> evaluate a >>= \a' -> case f' of
    Lambda () n -> return $ substitute a' n
    Reference () c -> return $ Applied (Reference () c) a'
    Applied a b -> return $ Applied (Applied a b) a'

substitute :: Term () n -> Term () (Maybe n) -> Term () n
substitute a = flip runReader (maybe a $ Reference ()) . sub where
  sub :: Term () a -> Reader (a -> Term () n) (Term () n)
  sub (Reference () c) = ($ c) `liftM` ask
  sub (Applied a b) = reduce <$> (Applied <$> sub a <*> sub b)
  sub (Lambda () e) = Lambda () `liftM` withReader
    (maybe (Reference () Nothing) . (fmap Just .)) (sub e)

reduce :: Term () n -> Term () n
reduce (Reference () n) = Reference () n
reduce (Lambda () e) = Lambda () e
reduce (Applied a b) = let b' = reduce b in
  case reduce a of
    Lambda () e -> substitute b' e
    elsewise -> Applied elsewise b'


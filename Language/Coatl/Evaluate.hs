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
  => Check a v -> ReaderT (Map v (Value n)) m (Value n)
evaluate (CLambda _ n) = Lambda `liftM` withReaderT
  (set (at Nothing) (Just $ Construct Nothing)
  . M.mapKeys Just . fmap (fmap Just)) (evaluate n)
evaluate (CInfer (IReference _ v)) = view (at v) >>= flip maybe return
  (throwError . text $
    printf "Symbol not in scope during evaluation: \"%s\"." (show v))
evaluate (CInfer (IApplication f a)) = evaluate (CInfer f)
  >>= \f' -> evaluate a >>= \a' -> case f' of
    Lambda n -> return $ substitute a' n
    Construct c -> return $ Applied (Construct c) a'
    Applied a b -> return $ Applied (Applied a b) a'

substitute :: Value n -> Value (Maybe n) -> Value n
substitute a = flip runReader (maybe a Construct) . sub where
  sub :: Value a -> Reader (a -> Value n) (Value n)
  sub (Construct c) = ($ c) `liftM` ask
  sub (Applied a b) = reduce <$> (Applied <$> sub a <*> sub b)
  sub (Lambda e) = Lambda `liftM` withReader
    (maybe (Construct Nothing) . (fmap Just .)) (sub e)

reduce :: Value n -> Value n
reduce (Construct n) = Construct n
reduce (Lambda e) = Lambda e
reduce (Applied a b) = let b' = reduce b in
  case reduce a of
    Lambda e -> substitute b' e
    elsewise -> Applied elsewise b'


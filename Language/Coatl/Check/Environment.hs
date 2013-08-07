{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language TemplateHaskell #-}
{-# Language Rank2Types #-}
{-# Language FlexibleContexts #-}
module Language.Coatl.Check.Environment where
-- base
import Data.Foldable (Foldable)
-- transformers
import Control.Monad.Error
-- containers
import Data.Map (Map)
import qualified Data.Map as M
-- mtl
import Control.Monad.Reader
-- lens
import Control.Lens
-- coatl
import Language.Coatl.Parse.Syntax
import Language.Coatl.Check.Abstract

-- | Change an identifier into its canonical representation.
canonicalize :: Identifier -> Canonical
canonicalize (Operator "->") = Function
canonicalize (Operator "~") = Dependent
canonicalize (Name "Type") = Type
canonicalize o = Simple o


-- | Check that two 'Check' are equal but for annotations.
equivalent :: Eq v => Check a v -> Check b v -> Bool
equivalent (CLambda _ l) (CLambda _ m) = l `equivalent` m
equivalent (CInfer r) (CInfer s) = r `eq'` s where
  eq' :: Eq v => Infer a v -> Infer b v -> Bool
  eq' (IReference _ v) (IReference _ w) = v == w
  eq' (IApplication a b) (IApplication c d) = a `eq'` c
    && b `equivalent` d
  eq' _ _ = False
equivalent _ _ = False

-- | Represent some 'Expression' as a 'Check'.
represent :: (MonadError [String] m) => Syntax a v
  -> m (Check a v)
represent (SReference a v) = return . CInfer $ IReference a v
represent (SLambda a e) = CLambda a `liftM` represent e
represent (SApplication a b) = (,) `liftM` represent a `ap` represent b
  >>= \(a', b') -> case preview _CInfer a' of
    Just a'' -> return . CInfer $ IApplication a'' b'
    Nothing -> throwError ["Term is not inferrable"]

data Environment a v = Environment
  { _named   :: APrism' v Canonical
    -- ^ A prism into the 'Canonical' in the symbol type.
  , _types   :: Map v (Check a v)
    -- ^ The types of things that have already been checked
    --   in the environment. They should be in normal form.
  }
makeLenses ''Environment

-- | A Prism on binary application.
binary :: APrism' v Canonical -> Simple Prism (Infer a v)
  (Canonical, a, Check a v, Check a v)
binary nd = prism create decompose where
  create (c, s, a, b) = IApplication
    (IApplication (IReference s
      (view (re $ clonePrism nd) c)) a) b
  decompose ck = case ck of
    i@(IApplication (IApplication (IReference s n) a) b) ->
      case preview (clonePrism nd) n of
        Nothing -> Left i
        Just c -> Right (c, s, a, b)
    elsewise -> Left elsewise

-- | Run a checking action in an environment with something new
--   as 'Nothing'.
with :: Ord v
  => Check a v
  -> ReaderT (Environment a (Maybe v)) m b
  -> ReaderT (Environment a v) m b
with a = withReaderT (set (types . at Nothing)
  (Just . fmap Just $ a) . lower) where
    lower :: Ord v => Environment a v -> Environment a (Maybe v)
    lower (Environment n ts) = Environment (_Just . n)
      . M.mapKeys Just . M.map (fmap Just) $ ts

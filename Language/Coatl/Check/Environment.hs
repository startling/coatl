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
-- transformers
-- lens
import Control.Lens
-- coatl
import Language.Coatl.Abstract

-- | A canonical identifier is either a reference to another part of the
--   program or one of the internal values: (~), (->), or Type. 
-- 
--  In the future I will probably extend this to take module names into
--  account.
data Canonical
  = Simple Identifier
  | Dependent
  | Function
  | Type
  deriving
  ( Eq
  , Ord
  , Show
  )

-- | Change an identifier into its canonical representation.
canonicalize :: Identifier -> Canonical
canonicalize (Operator "->") = Function
canonicalize (Operator "~") = Dependent
canonicalize (Name "Type") = Type
canonicalize o = Simple o

data Inferrable a v
  = IReference a v
  | IApplication (Inferrable a v) (Checkable a v)
  deriving
  ( Eq
  , Ord
  , Show
  , Functor
  , Foldable
  , Traversable
  )

data Checkable a v 
  = CLambda a (Checkable a (Maybe v))
  | CInferrable (Inferrable a v)
  deriving
  ( Eq
  , Ord
  , Show
  , Functor
  , Foldable
  , Traversable
  )
makePrisms ''Checkable

-- | Check that two 'Checkable' are equal but for annotations.
equivalent :: Eq v => Checkable a v -> Checkable b v -> Bool
equivalent (CLambda _ l) (CLambda _ m) = l `equivalent` m
equivalent (CInferrable r) (CInferrable s) = r `eq'` s where
  eq' :: Eq v => Inferrable a v -> Inferrable b v -> Bool
  eq' (IReference _ v) (IReference _ w) = v == w
  eq' (IApplication a b) (IApplication c d) = a `eq'` c
    && b `equivalent` d
  eq' _ _ = False
equivalent _ _ = False

-- | Represent some 'Expression' as a 'Checkable'.
represent :: (MonadError [String] m) => Expression a v
  -> m (Checkable a v)
represent (Reference a v) = return . CInferrable $ IReference a v
represent (Lambda a e) = CLambda a `liftM` represent e
represent (Application a b) = (,) `liftM` represent a `ap` represent b
  >>= \(a', b') -> case preview _CInferrable a' of
    Just a'' -> return . CInferrable $ IApplication a'' b'
    Nothing -> throwError ["Term is not inferrable"]

data Environment a v = Environment
  { _named   :: APrism' v Canonical
    -- ^ A prism into the 'Canonical' in the symbol type.
  , _types   :: Map v (Checkable a v)
    -- ^ The types of things that have already been checked
    --   in the environment. They should be in normal form.
  }
makeLenses ''Environment

-- | A Prism on binary application.
binary :: Canonical -> APrism' v Canonical
  -> Simple Prism (Inferrable a v)
     (a, Checkable a v, Checkable a v)
binary c nd = prism create decompose where
  create (s, a, b) = IApplication
    (IApplication (IReference s
      (view (re $ clonePrism nd) Function)) a) b
  decompose ck = case ck of
    i@(IApplication (IApplication (IReference s n) a) b) ->
      if preview (clonePrism nd) n == Just c
        then Right (s, a, b) else Left i
    elsewise -> Left elsewise


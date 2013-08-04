{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language TemplateHaskell #-}
{-# Language FlexibleContexts #-}
{-# Language Rank2Types #-}
module Language.Coatl.Check.Types where
-- base
import Data.Foldable (Foldable)
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
import Language.Coatl.Check (Canonical(..))

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

-- | Infer the type of an 'Inferrable' term.
infer ::
  ( Ord v, Show v
  , MonadError [String] m )
  => Inferrable b v -> ReaderT (Environment a v) m (Checkable a v)
infer (IReference _ v) = view (types . at v)
  >>= maybe report return where
    report :: MonadError [String] m => m a
    report = throwError
      [printf "Symbol not in scope: \"%s\"" (show v)]
infer (IApplication f ar) = view named >>= \nd -> infer f >>= \ft ->
    case preview (_CInferrable . binary Function nd) ft of
      Just (_, a, b) -> check ar a >> return b
      Nothing -> case preview
        (_CInferrable . binary Dependent nd) ft of
          Just (_, _, _) -> throwError
            [printf "(~) unimplemented as of yet"]
          Nothing -> report
  where
    report :: MonadError [String] m => m a
    report = throwError
      [printf "Expected a function type."]

-- | Check the type of some 'Checkable' term.
check ::
  ( Ord v, Show v
  , MonadError [String] m )
  => Checkable a v -> Checkable b v -> ReaderT (Environment b v) m ()
check (CLambda _ l) t = view named
  >>= \nd -> case preview (_CInferrable . binary Function nd) t of
    Just (_, a, b) -> withReaderT (add a . lower) $ check l (fmap Just b)
    Nothing -> case preview (_CInferrable . binary Dependent nd) t of
      Nothing -> throwError ["Expected a function type"]
      Just (_, a, b) -> throwError
        [printf "(~) unimplemented as of yet"]
  where
    lower :: Ord v => Environment a v -> Environment a (Maybe v)
    lower (Environment n ts) = Environment (_Just . n)
      . M.mapKeys Just . M.map (fmap Just) $ ts
    add :: Ord v => Checkable a v -> Environment a (Maybe v)
      -> Environment a (Maybe v)
    add c = set (types . at Nothing) (Just $ fmap Just c)
check (CInferrable i) t = infer i >>= \it -> if it `equivalent` t
  then return () else throwError ["Type mismatch."] where

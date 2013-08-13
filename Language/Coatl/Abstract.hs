{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language FlexibleContexts #-}
{-# Language Rank2Types #-}
module Language.Coatl.Abstract where
-- base
import Control.Applicative
import Control.Monad
import Data.Foldable (Foldable)
-- transformers
import Control.Monad.Error
-- containers
import Data.Map (Map)
import qualified Data.Map as M
-- bifunctors
import Data.Bitraversable
import Data.Bifoldable
import Data.Bifunctor
-- lens
import Control.Lens
-- ansi-wl-pprint
import Text.PrettyPrint.ANSI.Leijen
-- coatl
import Language.Coatl.Syntax

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

data Term n
  = Lambda (Term (Maybe n))
  | Applied (Term n) (Term n)
  | Construct n
  deriving
  ( Eq
  , Ord
  , Show
  , Functor
  )

-- | A Prism on binary application of constructors.
binary :: APrism' v Canonical -> Simple Prism (Term v)
  (Canonical, Term v, Term v)
binary nd = prism create decompose where
  create (c, a, b) = Applied
    (Applied (Construct (view (re $ clonePrism nd) c)) a) b
  decompose ck = case ck of
    i@(Applied (Applied (Construct c) a) b) ->
      case preview (clonePrism nd) c of
        Nothing -> Left i
        Just c -> Right (c, a, b)
    elsewise -> Left elsewise

data Infer a v
  = IReference a v
  | IApplication (Infer a v) (Check a v)
  deriving
  ( Eq
  , Ord
  , Show
  , Functor
  , Foldable
  , Traversable
  )

data Check a v 
  = CLambda a (Check a (Maybe v))
  | CInfer (Infer a v)
  deriving
  ( Eq
  , Ord
  , Show
  , Functor
  , Foldable
  , Traversable
  )
makePrisms ''Check

-- | Represent some 'Syntax' as a 'Check'.
represent :: MonadError Doc m => Syntax a v -> m (Check a v)
represent (SReference a v) = return . CInfer $ IReference a v
represent (SLambda a e) = CLambda a `liftM` represent e
represent (SApplication a b) = (,) `liftM` represent a `ap` represent b
  >>= \(a', b') -> case preview _CInfer a' of
    Just a'' -> return . CInfer $ IApplication a'' b'
    Nothing -> throwError . text $ "Term is not inferrable"

-- | Change an identifier into its canonical representation.
canonicalize :: Identifier -> Canonical
canonicalize (Operator "->") = Function
canonicalize (Operator "~") = Dependent
canonicalize (Name "Type") = Type
canonicalize o = Simple o

data Environment a v = Environment
  { _types       :: Map v (Term v)
    -- ^ The types of things that have already been checked
    --   in the environment. They should be in normal form.
  , _definitions :: Map v (Term v)
    -- ^ The values already defined.
  }
makeLenses ''Environment

-- | The types and definitions in the standard environment.
--   We have the following types:
--
--  @
--    Type : Type
--    (->) : Type -> Type -> Type
--    (~)  : Type ~ { a => (a -> Type) -> Type }
--  @
--
--   The definitions just map 'Type', 'Dependent', and 'Function'
--   to corresponding constructors.
standard :: Environment a Canonical
standard = Environment types defs where
  type_ = Construct Type
  function f a b = (Function, a, b) ^. (re $ binary f)
  dependent a b = (Dependent, a, b) ^. (re $ binary id)
  types = M.fromList
    [ (Type, type_)
    , (Function, function id type_ $ function id type_ type_)
    , (Dependent, dependent type_ . Lambda
      $ function _Just
          (function _Just
            (Construct Nothing) (Construct $ Just Type))
          (Construct $ Just Type))
    ]
  defs = M.fromList
    [ (Type, Construct Type)
    , (Dependent, Construct Dependent)
    , (Function, Construct Function)
    ]

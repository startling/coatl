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
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

-- | We have two semantically distinct types of identifiers in coatl,
--   names and operators. This type distinguishes them.
data Identifier
  = Name String
  | Operator String
  deriving
  ( Eq
  , Ord
  , Show
  )

instance Pretty Identifier where
  pretty (Name s) = text s
  pretty (Operator o) = parens $ text o

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

instance Pretty Canonical where
  pretty (Simple i) = pretty i
  pretty Dependent = parens $ text "~"
  pretty Function = parens $ text "->"
  pretty Type = text "Type"

data Term a n
  = Lambda a (Term a (Maybe n))
  | Applied (Term a n) (Term a n)
  | Reference a n
  deriving
  ( Eq
  , Ord
  , Show
  , Functor
  , Foldable
  , Traversable
  )

instance Bitraversable Term where
  bitraverse f g (Reference a n) = Reference <$> f a <*> g n
  bitraverse f g (Applied a b) = Applied
    <$> bitraverse f g a <*> bitraverse f g b
  bitraverse f g (Lambda a e) = Lambda <$> f a
    <*> bitraverse f (traverse g) e

instance Bifoldable Term where
  bifoldMap = bifoldMapDefault

instance Bifunctor Term where
  bimap = bimapDefault

instance Pretty n => Pretty (Term a n) where
  pretty (Reference _ n) = pretty n
  pretty (Applied a b) = parens (pretty a) <> parens (pretty b)
  pretty (Lambda _ e) = braces $ text "unimplemented"

-- | A Prism on binary application of constructors.
binary :: APrism' v Canonical -> Simple Prism (Term () v)
  (Canonical, Term () v, Term () v)
binary nd = prism create decompose where
  create (c, a, b) = Applied
    (Applied (Reference () (view (re $ clonePrism nd) c)) a) b
  decompose ck = case ck of
    i@(Applied (Applied (Reference () c) a) b) ->
      case preview (clonePrism nd) c of
        Nothing -> Left i
        Just c -> Right (c, a, b)
    elsewise -> Left elsewise

-- | A type for declarations.
data Declaration a v
  = Definition
  { _label :: a
  , _lhs   :: v
  , _rhs   :: Term a v
  }
  | Signature
  { _label :: a
  , _lhs   :: v
  , _rhs   :: Term a v
  } deriving
  ( Eq
  , Show
  , Functor
  , Foldable
  , Traversable
  )
makeLenses ''Declaration
makePrisms ''Declaration

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
represent :: MonadError Doc m => Term a v -> m (Check a v)
represent (Reference a v) = return . CInfer $ IReference a v
represent (Lambda a e) = CLambda a `liftM` represent e
represent (Applied a b) = (,) `liftM` represent a `ap` represent b
  >>= \(a', b') -> case preview _CInfer a' of
    Just a'' -> return . CInfer $ IApplication a'' b'
    Nothing -> throwError . text $ "Term () is not inferrable"

-- | Change an identifier into its canonical representation.
canonicalize :: Identifier -> Canonical
canonicalize (Operator "->") = Function
canonicalize (Operator "~") = Dependent
canonicalize (Name "Type") = Type
canonicalize o = Simple o

data Environment a v = Environment
  { _types       :: Map v (Term () v)
    -- ^ The types of things that have already been checked
    --   in the environment. They should be in normal form.
  , _definitions :: Map v (Term () v)
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
  type_ = Reference () Type
  function f a b = (Function, a, b) ^. (re $ binary f)
  dependent a b = (Dependent, a, b) ^. (re $ binary id)
  types = M.fromList
    [ (Type, type_)
    , (Function, function id type_ $ function id type_ type_)
    , (Dependent, dependent type_ . Lambda ()
      $ function _Just
          (function _Just
            (Reference () Nothing) (Reference () $ Just Type))
          (Reference () $ Just Type))
    ]
  defs = M.fromList
    [ (Type, Reference () Type)
    , (Dependent, Reference () Dependent)
    , (Function, Reference () Function)
    ]

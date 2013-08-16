{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language FlexibleContexts #-}
{-# Language Rank2Types #-}
-- | This  module has in it the abstract representations of
--   coatl programs and program environments (and some
--    more-syntax-level representations) as well as typeclass
--   instances and a number of small functions on them.
--
--   The very first thing we obtain, after parsing the source,
--   is a @'Term Span Identifier'@. These namess are then
--   resolved (currently with 'canonicalize', but probably
--   something more powerful in the future) into 'Canonical'.
--
--   For type-checking and inference we convert the @'Term'a v@s into 
--   one of two types: either a @'Check' a v@ or @'Infer' a v@. These
--   correspond (naturally) to the checkable and inferable terms
--   in coatl. Notice that every inferable term is also
--   checkable (and we can create a @'Check' a v@ from it with
--   'CInfer') but that then we need to supply a type in order to
--   check for well-formedness.
--
--   Finally, after type-checking, we evaluate any checkable term
--   back into a @'Term' a v@ that is supposed to be in normal
--   form -- that is, application of functions is carried out and 
--   the 'Reference' constructor is taken to represent
--   constructor names.
module Language.Coatl.Abstract
  ( Term(..)
  , Identifier(..)
  , Canonical(..)
  , annotation
  , binary
  , module Language.Coatl.Abstract
  ) where
-- base
import Control.Applicative
import Control.Monad
import Data.Foldable (Foldable)
-- transformers
import Control.Monad.Error
-- containers
import Data.Map (Map)
import qualified Data.Map as M
-- lens
import Control.Lens
-- ansi-wl-pprint
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
-- monad-loops
import Control.Monad.Loops
-- coatl
import Language.Coatl.Abstract.Term

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
  | IApplication a (Infer a v) (Check a v)
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

-- | Represent some @'Syntax' a v@ as a @'Check' a v@..
represent :: MonadError Doc m => Term a v -> m (Check a v)
represent (Reference a v) = return . CInfer $ IReference a v
represent (Lambda a e) = CLambda a `liftM` represent e
represent (Applied a x y) = (,) `liftM` represent x `ap` represent y
  >>= \(x', y') -> case preview _CInfer x' of
    Just x'' -> return . CInfer $ IApplication a x'' y'
    Nothing -> throwError . text $ "Term () is not inferrable"

-- | Change an identifier into its canonical representation.
canonicalize :: Identifier -> Canonical
canonicalize (Operator "->") = Function
canonicalize (Operator "~") = Dependent
canonicalize (Name "Type") = Type
canonicalize o = Simple o

-- | When we type-check and evaluate terms, we want some
--   information about things that have already been checked.
--   An @'Environment' a v@ stores that information about terms
--   annotated with type @a@ and with names of type @v@.
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

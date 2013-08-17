{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language FlexibleContexts #-}
{-# Language Rank2Types #-}
{-# Language TemplateHaskell #-}
-- | This  module has in it the abstract representation of
--   coatl programs and program environments (and some
--    more-syntax-level representations) as well as typeclass
--   instances and a number of small functions on them.
--
--   The very first thing we obtain, after parsing the source,
--   is a @'Term Span Identifier'@. These namess are then
--   resolved (currently with 'canonicalize', but probably
--   something more powerful in the future) into 'Canonical'.
--
--   All coatl terms may be checked as some type. Only some coatl terms
--   -- everything but lambdas, right now -- may be inferred. In any
--   case, all definitions are checked as the signature given and all
--   signatures are checked as 'Type'.
--
--   Finally, after type-checking, we evaluate any term into what
--   is supposed to be normal form -- that is, application of functions
--   is carried out as deeply as possible and 'Reference' is taken
--   to represent constructor names.
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

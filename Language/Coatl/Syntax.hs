{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language TemplateHaskell #-}
module Language.Coatl.Syntax where
-- base
import Control.Applicative
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
-- lens
import Control.Lens
-- bifunctors
import Data.Bifunctor
-- trifecta
import Text.Trifecta (Span)

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

-- 
data Syntax a v
  = SReference a v
  | SLambda a (Syntax a (Maybe v))
  | SApplication (Syntax a v) (Syntax a v)
  deriving
  ( Eq
  , Show
  , Functor
  , Foldable
  , Traversable
  )

instance Bifunctor Syntax where
  bimap f g (SReference a v) = SReference (f a) (g v)
  bimap f g (SLambda a b) = SLambda (f a) (bimap f (fmap g) b)
  bimap f g (SApplication a b) = SApplication (bimap f g a)
    (bimap f g b)

-- | A type for declarations.
data Declaration a v
  = Definition
  { _label :: a
  , _lhs   :: v
  , _rhs   :: Syntax a v
  }
  | Signature
  { _label :: a
  , _lhs   :: v
  , _rhs   :: Syntax a v
  } deriving
  ( Eq
  , Show
  , Functor
  , Foldable
  , Traversable
  )
makeLenses ''Declaration
makePrisms ''Declaration

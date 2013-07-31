{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language TemplateHaskell #-}
module Language.Coatl.Abstract where
-- base
import Control.Applicative
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
import Data.Maybe
-- lens
import Control.Lens

-- | Expressions are clumsily organized as of now into three types:
--   
--     (1) A 'Reference' using some type standing in as a variable.
--
--     (2) A nested expression taken as a lambda expression; a
--         reference to 'Nothing' therein is taken as a reference
--         to the parameter of the lambda expression, while a
--         reference to some @'Just' x@ is taken as `x` would be
--         in the surrounding context.
--
--     (3) The application of one expression to another as argument.
--
--   These first two are also parametrized by some extra information:
--   namely their source positions, but we may find something else
--   to store there, too.
--
--   There are some open questions here: how will we represent things
--   like numbers and strings at first? Do we want a separate reference
--   type for foreign functions?
data Expression a v
  = Reference a v
  | Lambda a (Expression a (Maybe v))
  | Application (Expression a v) (Expression a v)
  deriving
  ( Eq
  , Show
  , Functor
  , Foldable
  , Traversable
  )

-- | Recursively traverse all of the free references in an expression.
references :: Applicative f => ((a, v) -> f (a, v))
  -> Expression a v -> f (Expression a v)
references f (Reference a v) = uncurry Reference <$> f (a, v)
references f (Lambda a m) = Lambda a <$> references (altered f) m
  where
    altered :: Applicative f => ((a, v) -> f (a, v))
      -> (a, Maybe v) -> f (a, Maybe v)
    altered _ a@(_, Nothing) = pure a
    altered f (a, Just b) = fmap (fmap Just) $ f (a, b)
references f (Application e e') = Application
  <$> references f e <*> references f e'

-- | Traverse all the annotations in an expression.
annotations :: Applicative f => (a -> f b)
  -> Expression a v -> f (Expression b v)
annotations f (Reference a v) = Reference <$> f a <*> pure v
annotations f (Lambda a e) = Lambda <$> f a <*> annotations f e
annotations f (Application e e') = Application
  <$> annotations f e <*> annotations f e'

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

-- | A type for declarations.
data Declaration a v
  = Value
  { _label :: a
  , _lhs   :: v
  , _rhs   :: Expression a v
  }
  | Signature
  { _label :: a
  , _lhs   :: v
  , _type_ :: Expression a v
  } deriving
  ( Eq
  , Show
  , Functor
  , Foldable
  , Traversable
  )
makeLenses ''Declaration
makePrisms ''Declaration

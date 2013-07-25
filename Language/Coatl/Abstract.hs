{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
module Language.Coatl.Abstract where
-- base
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))

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

-- | We have two semantically distinct types of identifiers in coatl,
--   names and operators. This type distinguishes them.
data Identifier
  = Name String
  | Operator String
  deriving
  ( Eq
  , Show
  )

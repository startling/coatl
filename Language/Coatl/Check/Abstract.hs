{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
module Language.Coatl.Check.Abstract where
-- base
import Control.Applicative
import Data.Foldable (Foldable)
-- bifunctors
import Data.Bitraversable
import Data.Bifoldable
import Data.Bifunctor
-- lens
import Control.Lens
-- coatl
import Language.Coatl.Parse.Syntax

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


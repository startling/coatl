{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language Rank2Types #-}
module Language.Coatl.Abstract.Term where
-- base
import Control.Applicative
import Control.Monad
import Data.Foldable (Foldable)
-- mtl
import Control.Monad.Writer
import Control.Monad.State
-- bifunctors
import Data.Bitraversable
import Data.Bifoldable
import Data.Bifunctor
-- ansi-wl-pprint
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))
-- lens
import Control.Lens

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
  | Applied a (Term a n) (Term a n)
  | Reference a n
  deriving
  ( Eq
  , Ord
  , Show
  , Functor
  , Foldable
  , Traversable
  )

-- | A lens on the outermost source annotation of a @'Term' a n@.
annotation :: Functor f => (a -> f a) -> Term a n -> f (Term a n)
annotation f (Lambda a e) = (\n -> Lambda n e) <$> f a
annotation f (Applied a x y) = (\n -> Applied n x y) <$> f a
annotation f (Reference a n) = (\x -> Reference x n) <$> f a

-- | Abstract a term into a lambda term, given a variable to bind.
abstract :: Eq n => a -> n -> Term a n -> Term a n
abstract a s = Lambda a . fmap
  (\x -> if x == s then Nothing else Just x)

instance Monoid a => Applicative (Term a) where
  pure = return
  (<*>) = ap

instance Monoid a => Monad (Term a) where
  return = Reference mempty
  Reference a r >>= f = over annotation (a <>) (f r)
  Applied a x y >>= f = Applied a (x >>= f) (y >>= f)
  Lambda a body >>= f = Lambda a $ body >>= maybe (return Nothing) (fmap Just . f)

instance Bitraversable Term where
  bitraverse f g (Reference a n) = Reference <$> f a <*> g n
  bitraverse f g (Applied a x y) = Applied <$> f a
    <*> bitraverse f g x <*> bitraverse f g y
  bitraverse f g (Lambda a e) = Lambda <$> f a
    <*> bitraverse f (traverse g) e

instance Bifoldable Term where
  bifoldMap = bifoldMapDefault

instance Bifunctor Term where
  bimap = bimapDefault

instance Pretty (Term a Canonical) where
  pretty = prettyTerm names where
    names :: [String]
    names = (>>=) [1..] . flip replicateM $ ['a'..'z']

-- | Pretty-print a term, given a list of names to give to parameters
--   for lambdas.
prettyTerm :: [String] -> Term a Canonical -> Doc
prettyTerm names = prettyPrec 0 where
  operator :: Int -> Int -> String
    -> Term a Canonical -> Term a Canonical -> Doc
  operator up n o a b = (if n > up then parens else id)
    $ prettyPrec (up + 1) a <+> text o <+> prettyPrec (up + 1) b
  -- Pretty-print a term given the current precedence.
  prettyPrec :: Int -> Term a Canonical -> Doc
  prettyPrec n (Applied _ (Applied _ (Reference _ r) a) b) = case r of 
    Simple (Operator o) -> operator 5 n o a b
    Function -> operator 5 n "->" a b
    Dependent -> operator 5 n "~" a b
  prettyPrec n (Applied _ a b) = (if n > 10 then parens else id)
      $ prettyPrec 10 a <+> prettyPrec 11 b
  prettyPrec _ (Reference _ r) = pretty r
  prettyPrec _ (Lambda _ e) = braces
    $ let (x, w) = flip evalState names . runWriterT $ inLambda e in
      hsep w <+> text "=>" <+> x
  -- Pretty-print the body of a (potentially multiple-argument)
  -- lambda, given an infinite list of names to keep as state
  -- and writing, in order, the parameter names used.
  inLambda ::
    ( MonadState [String] m
    , MonadWriter [Doc] m )
   => Term a (Maybe Canonical) -> m Doc
  inLambda l = (Simple . Name . head) `liftM` get
    >>= \h -> modify tail >>
      if elemOf traverse (Just h) l then inLambda l
      else tell [pretty h] >> case fmap (maybe h id) l of
        Lambda _ e -> inLambda e
        elsewise -> return . pretty $ elsewise

-- | A Prism on binary application of constructors.
binary :: APrism' v Canonical -> Simple Prism (Term () v)
  (Canonical, Term () v, Term () v)
binary nd = prism create decompose where
  create (c, a, b) = Applied ()
    (Applied () (Reference () (view (re $ clonePrism nd) c)) a) b
  decompose ck = case ck of
    i@(Applied _ (Applied _ (Reference _ c) a) b) ->
      case preview (clonePrism nd) c of
        Nothing -> Left i
        Just d -> Right (d, a, b)
    elsewise -> Left elsewise



{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
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
-- mtl
import Control.Monad.Writer
import Control.Monad.State
-- bifunctors
import Data.Bitraversable
import Data.Bifoldable
import Data.Bifunctor
-- lens
import Control.Lens
-- ansi-wl-pprint
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
-- monad-loops
import Control.Monad.Loops

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

instance Applicative (Term ()) where
  pure = return
  (<*>) = ap

instance Monad (Term ()) where
  return = Reference ()
  Reference _ a >>= f = f a
  Applied a b >>= f = Applied (a >>= f) (b >>= f)
  Lambda _ e >>= f = Lambda () $ e >>= maybe (return Nothing) (fmap Just . f)

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

instance Pretty (Term a Canonical) where
  pretty = prettyPrec 0 where
    operator :: Int -> Int -> String
      -> Term a Canonical -> Term a Canonical -> Doc
    operator up n o a b = (if n > up then parens else id)
      $ prettyPrec (up + 1) a <+> text o <+> prettyPrec (up + 1) b
    -- Pretty-print a term given the current precedence.
    prettyPrec :: Int -> Term a Canonical -> Doc
    prettyPrec n (Applied (Applied (Reference _ r) a) b) = case r of 
      Simple (Operator o) -> operator 5 n o a b
      Function -> operator 5 n "->" a b
      Dependent -> operator 5 n "~" a b
    prettyPrec n (Applied a b) = (if n > 10 then parens else id)
        $ prettyPrec 10 a <+> prettyPrec 11 b
    prettyPrec _ (Reference _ r) = pretty r
    prettyPrec _ (Lambda _ e) = braces
      $ let (x, w) = flip evalState names . runWriterT $ inLambda e in
        hsep w <+> text "=>" <+> x
    -- An infinite list of names to draw from.
    names :: [String]
    names = (>>=) [1..] . flip replicateM $ ['a'..'z']
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

-- | Represent some @'Syntax' a v@ as a @'Check' a v@..
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

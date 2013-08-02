{-# Language FlexibleContexts #-}
{-# Language GeneralizedNewtypeDeriving #-}
-- Say we have the following two declarations:
--
-- @
--  the : Type ~ { a => a -> a }
--  the _ a = a
--
--  example : the Type Nat
--  example = the Nat O
-- @
--
-- Looking only at the the declarations, we observe that `example`'s
-- type depends on `the`'s type. So we have the following dependency
-- graph: "[`the`] <= [`const`]`.
--
-- Following this graph, we first type-check `the`'s type declaration;
-- in other words, we check that the type of the type declaration is
-- Type. Since its uses of Type, (~), and (->) are all legal, it passes.
--
-- Then we evaluate it to a normal form where references are considered
-- normal. This just gives us @Type ~ {a => a -> a}@.
--
-- Next, we type-check `example`'s type declaration; since `the` takes
-- two arguments (a type and a value of that type) we're fine.
--
-- Then we evaluate it to the reference-normal form as above. This is,
-- uninterestingly, "the Type Nat".
--
-- Then we look at the RHS of each declaration and check which types
-- each depends on; again, `the` depends on nothing but `example` depends
-- on `the`, so our graph looks like "[`the`] <= [`const`]".
--
-- Then we follow the references in `the`'s type and evaluate it to
-- normal form. This is trivial. Finally we check whether `the`'s RHS
-- agrees with its type declaration. Since lambdas may be of the function
-- type or of the dependent-function type, it does.
--
-- Now we follow the references in `example`'s type and evaluate it to
-- normal form. This gives us just `Nat`. because of `the`'s type, we know
-- the type of the RHS of `example` is just `Nat`, so that succeeds, also.
module Language.Coatl.Check where
-- base
import Control.Applicative
import Control.Arrow
import Data.Either
import Data.Foldable (toList, traverse_, Foldable)
import Data.Monoid
import qualified Data.Traversable as T
-- containers
import Data.Map (Map)
import qualified Data.Map as M
-- transformers
import Control.Monad.Error
import Control.Monad.Identity
-- mtl
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
-- either
import Control.Monad.Trans.Either
-- lens
import Control.Lens
-- trifecta
import Text.Trifecta
-- coatl
import Language.Coatl.Abstract
import Language.Coatl.Graph

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

canonicalize :: Identifier -> Canonical
canonicalize (Operator "->") = Function
canonicalize (Operator "~") = Dependent
canonicalize (Name "Type") = Type
canonicalize o = Simple o

-- | A type representing the things in our checking environment.
type Environment = Map Canonical (Expression (Maybe Span) Canonical)

type WithEnvironmentT m a = ReaderT Environment
  (StateT Environment (EitherT [String] m)) a

type WithEnvironment a = WithEnvironmentT Identity a

-- | Evaluate an environment-dependent action in some 'Monad'.
withEnvironmentT :: Monad m
  => WithEnvironmentT m a -> m (Either [String] Environment)
withEnvironmentT r = runEitherT . flip execStateT mempty 
  $ runReaderT r mempty

-- | Evaluate an environment-dependent action.
withEnvironment :: WithEnvironment a -> Either [String] Environment
withEnvironment = runIdentity . withEnvironmentT

-- | Run an environment-dependant action with an addition to the environment.
assuming ::
  ( MonadReader Environment m
  , MonadError [String] m )
  => [(Canonical, Expression (Maybe Span) Canonical)] -> m a -> m a
assuming as = local $ \e -> e <> M.fromList as

-- | The standard environment. We have the following types:
--
--  @
--    Type : Type
--    (->) : Type -> Type -> Type
--    (~)  : Type ~ { a => (a -> Type) -> Type }
--  @
standard :: [(Canonical, Expression (Maybe a) Canonical)]
standard =
  [ (Type, type_)
  , (Function, binary function type_ type_)
  , (Dependent, binary dependent type_
      . Lambda Nothing
        . binary (Just <$> function)
          (binary (Just <$> function)
            (Reference Nothing Nothing) (Just <$> type_))
             $ Just <$> type_ )
  ]
  where
    binary a b c = Application (Application a b) c
    type_ :: Expression (Maybe a) Canonical
    type_ = Reference Nothing Type
    function :: Expression (Maybe a) Canonical
    function = Reference Nothing Function
    dependent :: Expression (Maybe a) Canonical
    dependent = Reference (Nothing) Dependent

-- | Run a number of 'EitherT e m b' with the same error state,
--   'mappend' the errors together, and throw the result.
collect :: (Foldable t , Monoid e , MonadError e m)
  => (a -> EitherT e m b) -> t a -> m ()
collect f = mapM (runEitherT . f) . toList
  >=> \e -> let ls = lefts e in
    if null ls then return ()
      else throwError (mconcat ls)

-- | Check that all the references in an expression exist either
--   in the environment or in a given list.
checkNames ::
  ( MonadReader Environment m
  , MonadError [String] m )
  => [Canonical] -> Expression a Canonical -> m ()
checkNames cs e = (>> return ())
  . collect id . (`map` toListOf references e)
    $ \(a, v) -> ask >>= \m -> if M.member v m
      || v `elem` cs then return ()
        else throwError ["Unknown name: " ++ show v]

-- | Create a graph from a list of declarations, with the provision
--   that each value declaration depends on the corresponding type
--   signature.
--
--   The identifiers in this graph have a 'Bool' placed in them --
--   'True' if they represent a type signature and 'False' otherwise.
--
--   This might create spooky errors if more than one of either kind
--   of declaration eixsts.
asGraph :: Ord v => [Declaration a v]
  -> Graph (Bool, v) (Declaration a v)
asGraph = connections (has _Signature &&& view lhs) deps where
  deps :: Fold (Declaration v a) (Bool, a)
  deps f d = let
    direct = (rhs . traverse)
      (\x -> (x <$) . f . (,) False $ x) d in
        if has _Value d then f (True, view lhs d) *> direct
          else direct

-- | Conservatively check for partiality: if a declaration references
--   anything that references itself, throw an error.
--
--   This should probably be improved eventually.
checkTotality :: (MonadReader Environment m, MonadError [String] m)
  => [(Canonical, Expression a Canonical)] -> Canonical -> m ()
checkTotality cs c = let
  graph = map (fmap $ map snd . toListOf references) cs
  in if path (associations graph) c c
    then throwError [show c ++ " references itself."]
    else return ()


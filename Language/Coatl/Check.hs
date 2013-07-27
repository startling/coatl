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
import Data.Either
import Data.Foldable (toList)
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
-- either
import Control.Monad.Trans.Either
-- lens
import Control.Lens
-- trifecta
import Text.Trifecta
-- coatl
import Language.Coatl.Abstract

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
data Env = Env
  { _assumptions :: Map Canonical (Expression () Canonical)
  } deriving
  ( Eq
  , Show
  )

-- | A monad representing the context we need when checking things.
newtype EnvironmentT m a = EnvironmentT
    (ReaderT Env (EitherT [String] m) a)
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadReader Env
  , MonadError [String]
  )

type Environment = EnvironmentT Identity

-- | Evaluate an environment-dependent action.
runEnvironmentT :: EnvironmentT m a -> m (Either [String] a)
runEnvironmentT (EnvironmentT r) = runEitherT $ runReaderT r (Env mempty)

-- | Evaluate an environment-dependent action.
runEnvironment :: Environment a -> Either [String] a
runEnvironment = runIdentity . runEnvironmentT

-- | Run an environment-dependant action with an addition to the environment.
assuming :: Monad m => [(Canonical, Expression () Canonical)]
  -> EnvironmentT m a -> EnvironmentT m a
assuming as = local $ \e -> e
  { _assumptions = _assumptions e <> M.fromList as }

-- | The standard environment. We have the following types:
--
--  @
--    Type : Type
--    (->) : Type -> Type -> Type
--    (~)  : Type ~ { a => (a -> Type) -> Type }
--  @
standard :: [(Canonical, Expression () Canonical)]
standard =
  [ (Type, type_)
  , (Function, binary function type_ type_)
  , (Dependent, binary dependent type_
      . Lambda ()
        . binary (Just <$> function)
          (binary (Just <$> function) (Reference () Nothing) (Just <$> type_))
            $ Just <$> type_ )
  ]
  where
    binary a b c = Application (Application a b) c
    type_ :: Expression () Canonical
    type_ = Reference () Type
    function :: Expression () Canonical
    function = Reference () Function
    dependent :: Expression () Canonical
    dependent = Reference () Dependent

-- | Run a number of 'EitherT e m b' with the same error state,
--   'mappend' the errors together, and throw the result.
collectErrors ::
  ( Traversable t
  , Monoid e
  , MonadError e m
  ) => (a -> EitherT e m b) -> t a -> m (t b)
collectErrors f es = T.mapM (runEitherT . f) es
  >>= \xs -> case traverse id xs of
    Left _ -> throwError . mconcat . lefts $ toList xs
    Right r -> return r

-- | Check that all the references in an expression exist.
checkNames :: Monad m => Expression a Canonical -> EnvironmentT m ()
checkNames e = (>> return ())
  . collectErrors id . (`map` toListOf references e)
    $ \(a, v) -> ask >>= \m -> if M.member v (_assumptions m)
      then return ()
      else throwError ["Unknown name: " ++ show v]


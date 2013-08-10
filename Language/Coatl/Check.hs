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
import Language.Coatl.Graph
import Language.Coatl.Parse.Syntax
import Language.Coatl.Evaluate
import Language.Coatl.Check.Abstract
import Language.Coatl.Check.Environment
import Language.Coatl.Check.Types

-- | Run an environment-dependent action with an addition to the environment.
assuming :: (Ord k, MonadReader (Map k v) m) => [(k, v)] -> m a -> m a
assuming as = local $ \e -> e <> M.fromList as

-- | The standard environment. We have the following types:
--
--  @
--    Type : Type
--    (->) : Type -> Type -> Type
--    (~)  : Type ~ { a => (a -> Type) -> Type }
--  @
standard :: [(Canonical, Syntax (Maybe a) Canonical)]
standard =
  [ (Type, type_)
  , (Function, binary function type_ type_)
  , (Dependent, binary dependent type_
      . SLambda Nothing
        . binary (Just <$> function)
          (binary (Just <$> function)
            (SReference Nothing Nothing) (Just <$> type_))
             $ Just <$> type_ )
  ]
  where
    binary a b c = SApplication (SApplication a b) c
    type_ :: Syntax (Maybe a) Canonical
    type_ = SReference Nothing Type
    function :: Syntax (Maybe a) Canonical
    function = SReference Nothing Function
    dependent :: Syntax (Maybe a) Canonical
    dependent = SReference (Nothing) Dependent

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
  ( Ord v, Show v, MonadError [String] m
  , MonadReader (Map v a) m )
  => [Declaration k v] -> m ()
checkNames ds = ask >>= \env ->
  forM_ ds $ mapMOf_ (rhs . traverse)
    $ \r -> unless (r `M.member` env || any ((r ==) . view lhs) ds)
      $ throwError ["Unknown name: " ++ show r]

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
      (\x -> x <$ f (True, x) <* f (False, x)) d in
        if has _Definition d then f (True, view lhs d) *> direct
          else direct

-- | Check a list of declarations and read them into the environment.
declarations ::
  ( MonadError [String] m
  , MonadState
    ( Map Canonical (Value Canonical)
    , Map Canonical (Value Canonical)) m )
  => [Declaration a Canonical] -> m ()
declarations = mapM_ (collect checkd) <=<
  either (throwError . (: []) . show) return . sort . asGraph where
    checkd a = if has _Signature a
      then checkSignature (view lhs a) (view rhs a)
      else checkDefinition (view lhs a) (view rhs a)
    checkSignature l r = get >>= \s -> do
      -- Find the representation of signature.
      r' <- represent r
      -- Check that the signature has type 'Type'.
      runReaderT (check r' $ Construct Type)
        (uncurry (Environment id) s)
      -- Evaluate the signature.
      v <- runReaderT (evaluate r') (snd s)
      -- Set the signature at this lhs to the value.
      (_1 . at l) .= Just v
    checkDefinition l r = get >>= \s -> do
      -- Find the representation of the definition.
      r' <- represent r
      -- Find the type signature corresponding to this
      -- definition.
      ts <- use (_2 . at l) >>= flip maybe return
        (throwError ["Something's wrong."])
      -- Check that the value of the definition checks
      -- as that type.
      runReaderT (check r' ts) (uncurry (Environment id) s)
      -- Evaluate the type.
      v <- runReaderT (evaluate r') (snd s)
      -- Set the value.
      (_2 . at l) .= Just v

-- | Conservatively check for partiality: if a declaration references
--   anything that references itself, throw an error.
--
--   This should probably be improved eventually.
checkTotality :: (Ord a, Show a, Foldable t, MonadError [String] m)
  => [(a, t a)] -> a -> m ()
checkTotality cs c = let
  graph = map (fmap toList) cs
  in if path (associations graph) c c
    then throwError [show c ++ " references itself."]
    else return ()

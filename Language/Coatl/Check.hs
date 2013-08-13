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
import Control.Arrow ((&&&))
import Data.Either
import Data.Foldable (toList, traverse_, Foldable)
import Data.Monoid
import qualified Data.Traversable as T
-- containers
import Data.Map (Map)
import qualified Data.Map as M
-- transformers
import Control.Monad.Error
-- mtl
import Control.Monad.Reader
import Control.Monad.State
-- either
import Control.Monad.Trans.Either
-- lens
import Control.Lens
-- ansi-wl-pprint
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
-- trifecta
import Text.Trifecta hiding (text)
-- coatl
import Language.Coatl.Graph
import Language.Coatl.Abstract
import Language.Coatl.Evaluate
import Language.Coatl.Check.Types

-- | Run a number of 'EitherT e m b' with the same error state,
--   'mappend' the errors together, and throw the result.
collect :: (Foldable t , Monoid e , MonadError e m)
  => (a -> EitherT e m b) -> t a -> m ()
collect f = mapM (runEitherT . f) . toList
  >=> \e -> let ls = lefts e in
    if null ls then return ()
      else throwError (mconcat ls)

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
  ( MonadError Doc m
  , MonadState (Environment a Canonical) m )
  => [Declaration a Canonical] -> m ()
declarations = mapM_ (collect checkDeclaration) <=<
  either (throwError . text . show) return . sort . asGraph

-- | Check that every name in some 'foldable' has a definition
--   and a type signature in the environment.
names ::
  ( Ord v, Pretty v
  , Foldable t
  , MonadState (Environment a v) m
  , MonadError Doc m )
  => t v -> m ()
names = collect $ \n -> get >>= \s -> do
  unless (M.member n (view types s)) .
    throwError $ pretty n <+> text "has no signature."
  unless (M.member n (view definitions s)) $
    throwError $ pretty n <+> text "has no definition."

-- | Check a declaration and read it into the environment, assuming
--   that, if it is a 'Definition', its type signature is present
--   in the first part of the environment.
checkDeclaration ::
  ( MonadError Doc m
  , MonadState (Environment a Canonical) m )
  => Declaration a Canonical -> m ()
checkDeclaration (Signature _ l r) = get >>= \s -> do
  -- Error if a type signature of this already exists.
  when (M.member l $ view types s) . throwError
    $ text "Multiple signatures for" <+> pretty l
  -- Error if any of the names in the declarations do
  -- not yet exist.
  names r
  -- Find the representation of signature.
  r' <- represent r
  -- Check that the signature has type 'Type'.
  runReaderT (check r' $ Reference () Type) (Checking id s)
  -- Evaluate the signature.
  v <- runReaderT (evaluate r') (view definitions s)
  -- Set the signature at this lhs to the value.
  (types . at l) .= Just v
checkDeclaration (Definition _ l r) = get >>= \s -> do
  -- Error if a definition of this already exists.
  when (M.member l $ view definitions s) . throwError
    $ text "Multiple definitions for" <+> pretty l
  -- Error if any of the names in the declarations do
  -- not yet exist.
  names r
  -- Find the representation of the definition.
  r' <- represent r
  -- Find the type signature corresponding to this
  -- definition.
  ts <- use (types . at l) >>= flip maybe return
    (throwError $ text "Something's wrong: " <+> pretty l)
  -- Check that the value of the definition checks
  -- as that type.
  runReaderT (check r' ts) (Checking id s)
  -- Evaluate the type.
  v <- runReaderT (evaluate r') (view definitions s)
  -- Set the value.
  (definitions . at l) .= Just v

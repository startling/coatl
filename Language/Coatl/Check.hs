{-# Language FlexibleContexts #-}
{-# Language GeneralizedNewtypeDeriving #-}
-- | Check declarations.
module Language.Coatl.Check where
-- base
import Control.Applicative
import Control.Arrow ((&&&))
import Data.Foldable (Foldable)
-- containers
import qualified Data.Map as M
-- transformers
import Control.Monad.Error
-- mtl
import Control.Monad.Reader
import Control.Monad.State
-- lens
import Control.Lens
-- ansi-wl-pprint
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
-- coatl
import Language.Coatl.Abstract
import Language.Coatl.Evaluate
import Language.Coatl.Check.Types
import Language.Coatl.Extra.Graph
import Language.Coatl.Extra.Error

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
  -- Check that the signature has type 'Type'.
  check s r $ Reference () Type
  -- Evaluate the signature.
  v <- runReaderT (evaluate r) (view definitions s)
  -- Set the signature at this lhs to the value.
  (types . at l) .= Just v
checkDeclaration (Definition _ l r) = get >>= \s -> do
  -- Error if a definition of this already exists.
  when (M.member l $ view definitions s) . throwError
    $ text "Multiple definitions for" <+> pretty l
  -- Error if any of the names in the declarations do
  -- not yet exist.
  names r
  -- Find the type signature corresponding to this
  -- definition.
  ts <- use (types . at l) >>= flip maybe return
    (throwError $ text "Something's wrong: " <+> pretty l)
  -- Check that the value of the definition checks
  -- as that type.
  check s r ts
  -- Evaluate the type.
  v <- runReaderT (evaluate r) (view definitions s)
  -- Set the value.
  (definitions . at l) .= Just v

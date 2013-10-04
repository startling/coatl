module Language.Coatl.Extra.Error where
-- base
import Data.Either
import Data.Monoid
import Data.Foldable (toList, Foldable)
-- transformers
import Control.Monad.Error
-- either
import Control.Monad.Trans.Either

-- | Run a number of 'EitherT e m b' with the same error state,
--   'mappend' the errors together, and throw the result.
collect :: (Foldable t , Monoid e , MonadError e m)
  => (a -> EitherT e m b) -> t a -> m ()
collect f = mapM (runEitherT . f) . toList
  >=> \e -> let ls = lefts e in
    if null ls then return ()
      else throwError (mconcat ls)


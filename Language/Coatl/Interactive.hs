{-# Language FlexibleContexts #-}
-- | An interactive shell for checking and interpreting coatl.
module Language.Coatl.Interactive where
-- base
import Control.Applicative
import Data.Monoid
-- transformers
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Error
import Control.Monad.Trans
-- mtl
import Control.Monad.State.Strict
import Control.Monad.Reader
-- trifecta
import Text.Trifecta hiding (text)
-- ansi-wl-pprint
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
-- lens
import Control.Lens
-- haskeline
import System.Console.Haskeline
-- either
import Control.Monad.Trans.Either
-- monad-loops
import Control.Monad.Loops
-- coatl
import Language.Coatl.Abstract
import Language.Coatl.Parse
import Language.Coatl.Check
import Language.Coatl.Check.Types
import Language.Coatl.Evaluate

-- | A type for instructions to the interactive shell.
data Command
  = TypeC (Term Span Identifier)
    -- ^ Infer the type of some term.
  | EvalC (Term Span Identifier)
    -- ^ Pretty-print the fully-evaluated form of a term.
  | LoadC FilePath
    -- ^ Load something new into the environment.
  | QuitC
    -- ^ Exit from the interactive shell.
  deriving
  ( Eq
  , Show
  )

-- | Parse a command.
command :: DeltaParsing f => f (Maybe Command)
command = Just <$> cs <|> Nothing <$ spaces where
  cs = 
          TypeC <$> (symbol ":t" *> expression)
      <|> EvalC <$> expression
      <|> LoadC <$> (symbol ":l" *> some anyChar)
      <|> QuitC <$  symbol ":q"

-- | Run the interactive shell, with some 'Environment' as state.
interactive ::
  ( MonadState (Environment Span Canonical) m
  , MonadException m )
  => m ()
interactive = runInputT settings . (() <$) . iterateWhile id $ do
  line <- getInputLine . show . dullcyan . text $ "> "
  case parseString (command <* eof) mempty <$> line of
    Nothing -> return True
    Just (Failure f) -> True <$ liftIO (print $ indent 2 f)
    Just (Success Nothing) -> return True
    Just (Success (Just QuitC)) -> return False
    Just (Success (Just (TypeC s))) -> True <$ lift (showType s)
    Just (Success (Just (EvalC s))) -> True <$ lift (showEval s)
    Just (Success (Just (LoadC f))) -> True <$ lift (loadFile f)
  where
    settings = defaultSettings

handling :: MonadIO m => EitherT Doc m () -> m ()
handling = either (liftIO . print . indent 2) return <=< runEitherT

-- | Load a file into the interactive environment.
loadFile ::
  ( MonadIO m
  , MonadState (Environment Span Canonical) m )
  => FilePath -> m ()
loadFile f = handling $ parseFromFileEx (many declaration) f
  >>= \ds -> case ds of
    Failure d -> throwError . indent 2 $ d
    Success s -> declarations . map (fmap canonicalize) $ s

-- | Evaluate and show the type of a term.
showType ::
  ( MonadIO m
  , MonadState (Environment a Canonical) m )
  => Term Span Identifier -> m ()
showType s = handling $
  represent (fmap canonicalize s)
    >>= \r -> case preview _CInfer r of
      Nothing -> throwError . text $ "Uninferrable type."
      Just ty -> lift get >>= runReaderT (infer ty) . Checking id
        >>= liftIO . print . indent 2 . pretty

-- | Evaluate and pretty-print a term.
showEval ::
  ( MonadIO m
  , MonadState (Environment a Canonical) m )
  => Term Span Identifier -> m ()
showEval s = handling $
  represent (fmap canonicalize s)
    >>= \r -> case preview _CInfer r of
      Nothing -> throwError . text $ "Uninferrable type."
      Just ty -> lift get >>= runReaderT (infer ty) . Checking id
        >> lift get >>= runReaderT (evaluate r) . view definitions
          >>= liftIO . print . indent 2 . pretty

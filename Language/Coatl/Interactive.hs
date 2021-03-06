{-# Language FlexibleContexts #-}
-- | An interactive shell for checking and interpreting coatl.
module Language.Coatl.Interactive where
-- base
import Control.Applicative
import Data.Monoid
-- transformers
import Control.Monad.IO.Class
import Control.Monad.Error
-- mtl
import Control.Monad.State.Strict
import Control.Monad.Reader
-- trifecta
import Text.Trifecta hiding (text, line)
-- ansi-wl-pprint
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), line)
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
import Language.Coatl.Abstract.Term
import Language.Coatl.Parse
import Language.Coatl.Check
import Language.Coatl.Check.Types
import Language.Coatl.Evaluate
import Language.Coatl.Extra.Names

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
    Just (Success (Just (TypeC s))) -> True
      <$ lift (showType $ fmap canonicalize s)
    Just (Success (Just (EvalC s))) -> True
      <$ lift (showEval $ fmap canonicalize s)
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
  => Term Span Canonical -> m ()
showType s = handling $ lift get
  >>= flip infer s
    >>= liftIO . present

-- | Evaluate and pretty-print a term.
showEval ::
  ( MonadIO m
  , MonadState (Environment a Canonical) m )
  => Term Span Canonical -> m ()
showEval s = handling $ lift get
  >>= flip infer s
    >> lift get >>= runReaderT (evaluate s) . view definitions
      >>= liftIO . present

-- Pretty-print a term to the console.
present :: Term a Canonical -> IO ()
present t = shuffled >>= print . indent 2 . flip prettyTerm t

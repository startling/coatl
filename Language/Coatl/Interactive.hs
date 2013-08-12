{-# Language FlexibleContexts #-}
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
import Text.PrettyPrint.ANSI.Leijen (Doc, text)
-- lens
import Control.Lens
-- haskeline
import System.Console.Haskeline
-- either
import Control.Monad.Trans.Either
-- monad-loops
import Control.Monad.Loops
-- coatl
import Language.Coatl.Syntax
import Language.Coatl.Abstract
import Language.Coatl.Parse
import Language.Coatl.Check
import Language.Coatl.Check.Types
import Language.Coatl.Evaluate

data Command
  = TypeC (Syntax Span Identifier)
  | EvalC (Syntax Span Identifier)
  | QuitC
  deriving
  ( Eq
  , Show
  )

command :: DeltaParsing f => f (Maybe Command)
command = Just <$> cs <|> Nothing <$ spaces where
  cs = 
          TypeC <$> (symbol ":t" *> expression)
      <|> EvalC <$> expression
      <|> QuitC <$  symbol ":q"

interactive :: MonadException m => m (Environment Span Canonical)
interactive = flip execStateT standard . runInputT settings
  . (() <$) . iterateWhile id $ do
    line <- getInputLine "> "
    case parseString command mempty <$> line of
      Nothing -> return True
      Just (Failure f) -> True <$ liftIO (print f)
      Just (Success Nothing) -> return True
      Just (Success (Just QuitC)) -> return False
      Just (Success (Just (TypeC s))) -> True <$ lift (showType s)
      Just (Success (Just (EvalC s))) -> True <$ lift (showEval s)
  where
    settings = defaultSettings

showType ::
  ( MonadIO m
  , MonadState (Environment a Canonical) m )
  => Syntax a Identifier -> m ()
showType s = either (liftIO . print) return <=< runEitherT $
  represent (fmap canonicalize s)
    >>= \r -> case preview _CInfer r of
      Nothing -> throwError . text $ "Uninferrable type."
      Just ty -> lift get >>= runReaderT (infer ty) . Checking id
        >>= liftIO . putStrLn . prettily

showEval ::
  ( MonadIO m
  , MonadState (Environment a Canonical) m )
  => Syntax a Identifier -> m ()
showEval s = either (liftIO . print) return <=< runEitherT $
  represent (fmap canonicalize s)
    >>= \r -> case preview _CInfer r of
      Nothing -> throwError . text $ "Uninferrable type."
      Just ty -> lift get >>= runReaderT (infer ty) . Checking id
        >> lift get >>= runReaderT (evaluate r) . view definitions
          >>= liftIO . putStrLn . prettily

-- TODO: make this prettier
prettily :: Value Canonical -> String
prettily = show

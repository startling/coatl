module Main where
-- base
import System.Environment
-- mtl
import Control.Monad.State.Strict
-- coatl
import Language.Coatl.Abstract
import Language.Coatl.Interactive

main :: IO ()
main = flip evalStateT standard $ liftIO getArgs
  >>= mapM_ loadFile >> interactive

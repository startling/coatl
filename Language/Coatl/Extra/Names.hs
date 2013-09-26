{-# Language TemplateHaskell #-}
module Language.Coatl.Extra.Names where
-- base
import Control.Concurrent.MVar
import System.IO.Unsafe
-- utf8-string
import Data.ByteString.UTF8 (toString)
-- file-embed
import Data.FileEmbed
-- random
import System.Random

names :: [String]
names = lines . toString $ $(embedFile "words.txt")

shuffled :: IO [String]
shuffled = (`fmap` newStdGen)
  $ map (names !!) . randomRs (0, length names - 1)

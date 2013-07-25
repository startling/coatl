module Main where
-- base
import Data.Monoid
import Control.Applicative
-- trifecta
import Text.Trifecta
-- hspec
import Test.Hspec
-- coatl
import Language.Coatl.Abstract
import Language.Coatl.Parser.Expression (expression)

shouldParse :: Show a => Parser a -> String -> Expectation
shouldParse p s = parseString p mempty s `shouldSatisfy`
  (\c -> case c of
    Success _ -> True
    Failure _ -> False )

expressions :: Spec
expressions =
  describe "Language.Coatl.Parser.Expression" $ do
    describe "expression" $ do
      it "parses names" $ do
        expression `shouldParse` "function"
      it "parses operators" $ do
        expression `shouldParse` "(++)"
      it "parses infix operators" $ do
        expression `shouldParse` "a -> b"
        expression `shouldParse` "a ~ b"
      it "parses lambdas" $ do
        expression `shouldParse` "{a => a}"
      it "parses the type of the identity function" $ do
        expression `shouldParse` "Type ~ {a => a -> a}"
      it "parses ordinary application" $ do
        expression `shouldParse` "traverse pure"


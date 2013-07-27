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
import Language.Coatl.Graph
import Language.Coatl.Check

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

graphs =
  describe "Language.Coatl.Graph" $ do
    let graph = connections
          [ (1, [1, 2])
          , (2, [3])
          , (3, [4])
          , (4, [])
          ]
    describe "path" $ do
      it "finds direct paths" $ do
        path graph 1 2 `shouldBe` True
        path graph 3 4 `shouldBe` True
      it "finds indirect paths" $ do
        path graph 3 4 `shouldBe` True
        path graph 1 3 `shouldBe` True
        path graph 1 4 `shouldBe` True
      it "does not find nonexistent paths" $ do
        path graph 4 3 `shouldBe` False
        path graph 4 2 `shouldBe` False
        path graph 4 1 `shouldBe` False
        path graph 3 2 `shouldBe` False
        path graph 3 1 `shouldBe` False
        path graph 2 1 `shouldBe` False
      it "finds paths from a node to itself only when appropriate" $ do
        path graph 1 1 `shouldBe` True
        path graph 2 2 `shouldBe` False
        path graph 3 3 `shouldBe` False
      it "does not get confused by loops" $ do
        let graph' = connections
              [ (1, [2])
              , (2, [3])
              , (3, [1])
              ]
        path graph' 1 1 `shouldBe` True
        path graph' 2 1 `shouldBe` True
        path graph' 3 2 `shouldBe` True

checks =
  describe "Language.Coatl.Check" $ do
    let
      the = declaration "Type ~ { a => a -> a}" "{ _ => { a => a }}"
      example = declaration "the Type Nat" "the Nat O"
    describe "checkNames" $ do
      it "errors for `the`'s type without the standard assumptions" $ do
        shouldError . checkNames [] . fst $ the
      it "does not error for `the`'s type with standard assumptions" $ do
        shouldn'tError . assuming standard . checkNames [] . fst $ the
      it "does not error for `the`'s RHS" $ do
        shouldn'tError . checkNames [] . snd $ the
      it "errors for `example`'s type" $ do
        shouldError . checkNames [] . fst $ example
      it "errors for `example`'s RHS" $ do
        shouldError . checkNames [] . snd $ example
      it "does not error for `example`'s RHS given appropriate arguments" $ do
        shouldn'tError . assuming standard
          . checkNames
            [ Simple $ Name "O"
            , Simple $ Name "Nat"
            , Simple $ Name "the"
            ] . snd $ example
  where
    shouldError :: Show a => Environment a -> Expectation
    shouldError m = shouldSatisfy (runEnvironment m)
      $ \e -> case e of Left _ -> True; Right _ -> False;
    shouldn'tError :: Show a => Environment a -> Expectation
    shouldn'tError m = shouldSatisfy (runEnvironment m)
      $ \e -> case e of Left _ -> False; Right _ -> True;
    parse :: String -> Result (Expression Span Canonical)
    parse = fmap (fmap canonicalize) . parseString expression mempty
    declaration :: String -> String
      -> (Expression Span Canonical, Expression Span Canonical)
    declaration a b = case (,) <$> parse a <*> parse b of
      Success a -> a
      Failure f -> error $ show f

main :: IO ()
main = hspec . sequence_ $
  [ expressions
  , graphs
  , checks
  ]

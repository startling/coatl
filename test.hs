module Main where
-- base
import Data.Monoid
import Control.Applicative
import Control.Monad
-- containers
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M
-- trifecta
import Text.Trifecta
-- hspec
import Test.Hspec
-- lens
import Control.Lens
-- coatl
import Language.Coatl.Abstract
import Language.Coatl.Parser.Expression (expression)
import Language.Coatl.Parser.Declaration (declaration)
import Language.Coatl.Graph
import Language.Coatl.Check

shouldParse :: Show a => Parser a -> String -> Expectation
shouldParse p s = parseString p mempty s `shouldSatisfy`
  (\c -> case c of
    Success _ -> True
    Failure _ -> False )

parsing :: Spec
parsing= do
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
      it "parses many declarations" $ do
        (some expression `shouldParse`) $ unlines
          [ "the : Type ~ { a => a -> a }"
          , "the _ a = a"
          ]
  describe "Language.Coatl.Parser.Declaration" $ do
    describe "declaration" $ do
      it "parses simple redefinitions" $ do
        declaration `shouldParse` "a = b"
      it "parses definitions with newlines" $ do
        declaration `shouldParse` "a = b\n"
      it "parses definitions to calls" $ do
        declaration `shouldParse` "a = f (g a b) c"
      it "parses function definitions" $ do
        declaration `shouldParse` "const a b = a"
      it "parses multiline definitions" $ do
        declaration `shouldParse` "const a b = const\n  a b"
      it "parses simple type signatures" $ do
        declaration `shouldParse` "Type : Type"
      it "parses calls in type signatures" $ do
        declaration `shouldParse` "the : Type ~ { a => a -> a}"
      it "parses multiline type signatures" $ do
        declaration `shouldParse` "the : Type ~\n  { a => a -> a }"

graphs =
  describe "Language.Coatl.Graph" $ do
    let graph = associations
          [ (1, [1, 2])
          , (2, [3])
          , (3, [4])
          , (4, [])
          , (5, [6])
          , (6, [7])
          , (7, [5])
          , (8, [9])
          , (9, [8, 10])
          , (10, [9])
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
        path graph 5 5 `shouldBe` True
        path graph 6 5 `shouldBe` True
        path graph 7 6 `shouldBe` True
    describe "cycles" $ do
      let cs = cycles graph
      it "finds single-element cycles" $ do
        cs `shouldSatisfy` (elem [1])
      it "finds ordinary cycles" $ do
        cs `shouldSatisfy` any (`elem` rotations [5, 6, 7])
      it "finds joined cycles" $ do
        cs `shouldSatisfy` any (`elem` rotations [8, 9])
        cs `shouldSatisfy` any (`elem` rotations [9, 10])
      it "doesn't find non-cycles" $ do
        cs `shouldSatisfy` all (`notElem` rotations [2, 3, 4])
    describe "sort" $ do
      let
        directed :: Graph Int (Int, [Int])
        directed = associations
          [ (1, [2, 3])
          , (2, [4])
          , (3, [])
          , (4, [5])
          , (5, [])
          ]
        sg = either (error . show) id $ sort directed
      it "sorts nodes before their children" $ do
        sg `shouldSatisfy` (5 `before` 4)
        sg `shouldSatisfy` (5 `before` 2)
        sg `shouldSatisfy` (5 `before` 1)
        sg `shouldSatisfy` (3 `before` 4)
        sg `shouldSatisfy` (3 `before` 2)
        sg `shouldSatisfy` (3 `before` 1)
        sg `shouldSatisfy` (4 `before` 2)
        sg `shouldSatisfy` (2 `before` 1)
    where
      rotations :: [a] -> [[a]]
      rotations s = rotations' (length s) s
      rotations' :: Int -> [a] -> [[a]]
      rotations' 0 _ = []
      rotations' _ [] = []
      rotations' n (a : as) = let new = as ++ [a]
        in new : rotations' (n - 1) new
      before a b [] = False
      before a b (s : ss) = if any ((== b) . fst) s then False
        else if any ((== a) . fst) s then True else before a b ss

checks =
  describe "Language.Coatl.Check" $ do
    let
      the = declare
        [ "the : Type ~ { a => a -> a}"
        , "the _ a = a"
        ]
      example = declare
        [ "example : the Type Nat"
        , "example = the Nat O"
        ]
    describe "checkNames" $ do
      it "errors for `the` without standard assumptions" $ do
        shouldError . checkNames $ the
      it "does not error for `the` with standard assumptions" $ do
        shouldn'tError . assuming standard . checkNames $ the
      it "errors for `example` without assumptions" $ do
        shouldError . checkNames $ example
      it "errors for `example` even with standard assumptions" $ do
        shouldError . assuming standard . checkNames $ example
    describe "declarations" $ do
      it "errors for trivially unproductively-recursive functions" $ do
        let
          bottom = declare
            [ "bottom : Type ~ {a => a}"
            , "bottom = bottom"
            ]
        shouldError $ declarations bottom
      it "should not error for `the`'s or `example`'s types or definitions" $ do
        shouldn'tError . declarations $ the ++ example
      it "doesn't error for 'x = const O x'" $ do
        pendingWith "is this kind of thing worth the effort?"
        let
          x = declare
            [ "x : Nat"
            , "x = const O x"
            ]
        shouldn'tError . declarations $ x
  where
    shouldError :: Show a => WithEnvironment a -> Expectation
    shouldError m = shouldSatisfy (withEnvironment m)
      $ \e -> case e of Left _ -> True; Right _ -> False;
    shouldn'tError :: Show a => WithEnvironment a -> Expectation
    shouldn'tError m = shouldSatisfy (withEnvironment m)
      $ \e -> case e of Left _ -> False; Right _ -> True;
    parse :: String -> Result [Declaration Span Canonical]
    parse = fmap (map $ fmap canonicalize) . parseString (some declaration) mempty
    declare :: [String] -> [Declaration Span Canonical]
    declare as = case parse $ unlines as of
      Success a -> a
      Failure f -> error $ show f

main :: IO ()
main = hspec . sequence_ $
  [ parsing
  , graphs
  , checks
  ]

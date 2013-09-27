{-# Language ImplicitParams #-}
module Main where
-- base
import Data.Monoid hiding ((<>))
import Control.Applicative
import Control.Monad
-- containers
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
-- transformers
import Control.Monad.Identity
import Control.Monad.Error
-- mtl
import Control.Monad.Reader
import Control.Monad.State
-- either
import Control.Monad.Trans.Either
-- bifunctors
import Data.Bifunctor
-- trifecta
import Text.Trifecta hiding (text)
-- ansi-wl-pprint
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
-- hspec
import Test.Hspec
-- lens
import Control.Lens
-- coatl
import Language.Coatl.Extra.Graph
import Language.Coatl.Abstract
import Language.Coatl.Parse
import Language.Coatl.Check
import Language.Coatl.Check.Types
import Language.Coatl.Evaluate

shouldParse :: Show a => Parser a -> String -> Expectation
shouldParse p s = parseString (p <* eof) mempty s
  `shouldSatisfy` has _Success

run :: (?state :: s, ?read :: r)
  => ReaderT r (StateT s (EitherT e Identity)) a -> Either e a
run = runIdentity . runEitherT . flip evalStateT ?state
  . flip runReaderT ?read

succeeds :: (?state :: s, ?read :: r, Show e, Show a)
  => ReaderT r (StateT s (EitherT e Identity)) a -> Expectation
succeeds = (`shouldSatisfy` has _Right) . run

fails :: (?state :: s, ?read :: r, Show e, Show a)
  => ReaderT r (StateT s (EitherT e Identity)) a -> Expectation
fails = (`shouldSatisfy` has _Left) . run

declare :: [String] -> [Declaration Span Canonical]
declare = map (fmap canonicalize)
  . maybe (error "Parse error in example") id
  . preview _Success . parseString (some declaration) mempty
  . unlines

parsing :: Spec
parsing = do
  describe "Language.Coatl.Parse.Expression" $ do
    describe "expression" $ do
      it "parses names" $ do
        expression `shouldParse` "function"
      it "parses operators" $ do
        expression `shouldParse` "(++)"
      it "parses infix operators" $ do
        expression `shouldParse` "a -> b"
        expression `shouldParse` "a ~ b"
        expression `shouldParse` "a + b"
      it "parses unary lambdas" $ do
        expression `shouldParse` "{a => a}"
      it "parses n-ary lambdas where n > 1" $ do
        expression `shouldParse` "{a b => a}"
        expression `shouldParse` "{a b c => a}"
        expression `shouldParse` "{a b c d => a}"
      it "parses the type of the identity function" $ do
        expression `shouldParse` "Type ~ {a => a -> a}"
      it "parses ordinary application" $ do
        expression `shouldParse` "traverse pure"
      -- TODO: tests for fixity
  describe "Language.Coatl.Parse.Declaration" $ do
    describe "declaration" $ do
      it "parses simple redefinitions" $ do
        declaration `shouldParse` "a = b;"
      it "parses definitions with newlines" $ do
        declaration `shouldParse` "a = b;\n"
      it "parses definitions to calls" $ do
        declaration `shouldParse` "a = f (g a b) c;"
      it "parses function definitions" $ do
        declaration `shouldParse` "const a b = a;"
      it "parses multiline definitions" $ do
        declaration `shouldParse` "const a b = const\n  a b;"
      it "parses simple type signatures" $ do
        declaration `shouldParse` "Type : Type;"
      it "parses calls in type signatures" $ do
        declaration `shouldParse` "the : Type ~ { a => a -> a};"
      it "parses multiline type signatures" $ do
        declaration `shouldParse` "the : Type ~\n  { a => a -> a };"
      it "parses many declarations" $ do
        (some declaration `shouldParse`) $ unlines
          [ "the : Type ~ { a => a -> a };"
          , "the _ a = a;"
          ]

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
      rotations s = take (length s) $ iterate rotate s 
      rotate :: [a] -> [a]
      rotate s = case s of [] -> []; (c : cs) -> cs ++ [c];
      before a b [] = False
      before a b (s : ss) = if any ((== b) . fst) s then False
        else if any ((== a) . fst) s then True else before a b ss

checks = do
  describe "Language.Coatl.Check" $ do
    let
      ?read = M.empty
      ?state = standard
    let
      the = declare
        [ "the : Type ~ { a => a -> a};"
        , "the _ a = a;"
        ]
    describe "asGraph" $ do
      let
        g = asGraph $ declare
          [ "x : Type ~ { a => a -> a };"
          , "x _ a = a;"
          , "y : x Type (a -> a);"
          , "y n = x a n;"
          ]
        dependsOn a b = (g `shouldSatisfy`) $ \g -> elemOf (next g) b a
      it "has definitions depend on corresponding signatures" $ do
        forM_ [Simple $ Name "x", Simple $ Name "y"] $ \n ->
          (False, n) `dependsOn` (True, n)
      it "has type signatures depend on definitions" $ do
        (True, Simple $ Name "y") `dependsOn` (False, Simple $ Name "x")
      it "has type signatures depend on signatures" $ do
        (True, Simple $ Name "y") `dependsOn` (True, Simple $ Name "x")
      it "has definitions depend on definitions" $ do
        (False, Simple $ Name "y") `dependsOn` (False, Simple $ Name "x")
      it "has definitions depend on signatures" $ do
        (False, Simple $ Name "y") `dependsOn` (True, Simple $ Name "x")
    describe "names" $ do
      it "fails for simple references to nonexistent names" $ do
        fails . names . fmap canonicalize . Reference () . Name $ "boo"
      it "succeeds for simple references to extant names" $ do
        succeeds . names . Reference () $ Type
    describe "declarations" $ do
      it "errors for trivially unproductively-recursive functions" $ do
        let
          bottom = declare
            [ "bottom : Type ~ {a => a};"
            , "bottom = bottom;"
            ]
        fails $ declarations bottom
      it "errors for 'the' without the standard environment" $ do
        let ?state = Environment M.empty M.empty
        fails . declarations $ the
      it "should not error for `the`'s types or definitions" $ do
        succeeds . declarations $ the
      it "doesn't error for 'x = const O x'" $ do
        pendingWith "is this kind of thing worth the effort?"
        succeeds . declarations $ declare
          [ "x : Nat;"
          , "x = const O x;"
          ]
      it "errors for uninferrable definitions without signatures" $ do
        fails . declarations $ declare
          [ "xyz _ = the;" ]
      it "errors for definitions referencing nonexistant names" $ do
        fails . declarations $ declare
          [ "fish : Type;"
          , "fish = bird;"
          ]
      it "errors for signatures referencing nonexistant names" $ do
        fails . declarations $ declare
          [ "fish : bird;"
          , "fish = Type;"
          ]
  describe "Language.Coatl.Check.Types" $ do
    describe "check" $ do
      it "allows the type of monomorphic 'id'" $ do
        "a -> a" `checksAs` "Type"
      it "allows monomorphic 'id'" $ do
        "{ x => x }" `checksAs` "a -> a"
      it "allows the type of monomorphic 'const'" $ do
        "a -> a -> a" `checksAs` "Type"
      it "allows monomorphic 'const'" $ do
        "{ x _ => x }" `checksAs` "a -> a -> a"
      it "allows polymorphic 'the'/'id''s type" $ do
        "Type ~ { a => a -> a }" `checksAs` "Type"
      it "allows polymorphic 'the'/'id'" $
        "{ _ x => x }" `checksAs` "Type ~ { a => a -> a }"
  where
    parse = parseString expression mempty
    checksAs v s = let
      ?state = ()
      ?read = set (types . at (Simple $ Name "a")) (Just $ Reference () Type)
          . set (definitions . at (Simple $ Name "a"))
            (Just . Reference () . Simple . Name $ "a")
          $ standard
      in succeeds $ do
        -- Parse both expressions
        (v', s') <- liftM (over both (first (const ()) . fmap canonicalize))
          . maybe (throwError . text $ "Parse error in example.") return
          . preview _Success $ (,) <$> parse v <*> parse s
        -- Fully evaluate the type we're checking as.
        s'' <- view definitions >>= runReaderT (evaluate $ s')
        ask >>= \e -> check e v' s''
        return ()

evaluation :: Spec
evaluation = do
  describe "Language.Coatl.Evaluate" $ do
    describe "evaluate" $ do
      let
        id' = Lambda () $ Reference () Nothing
        id'' = Lambda () . Lambda () $ Reference () Nothing 
        flip' = Lambda () . Lambda () . Lambda ()
          . Applied () (Applied () (Reference () . Just . Just $ Nothing)
            $ Reference () Nothing)
          $ Reference () (Just Nothing)
        const' = Lambda () . Lambda () . Reference () . Just $ Nothing
      let
        ?read = M.fromList
          [ ( Simple $ Name "id", id' )
          , ( Simple $ Name "id'", id'' )
          , ( Simple $ Name "const", const' )
          , ( Simple $ Name "flip", flip' )
          , ( Simple $ Name "A", Reference () . Simple $ Name "A" )
          , ( Simple $ Name "a", Reference () . Simple $ Name "a" )
          , ( Simple $ Name "b", Reference () . Simple $ Name "b" )
          ]
      it "evaluates monomorphic 'id' correctly" $ do
        "{a => a}" `evaluatesTo` id'
      it "evaluates applications of monomorphic 'id' correctly" $ do
        "id a" `evaluatesTo` Reference () (Simple $ Name "a")
      it "evaluates monomorphic 'const' correctly" $ do
        "{a _ => a}" `evaluatesTo` const'
      it "evaluates applications of monomorphic 'const' correctly" $ do
        "const a b" `evaluatesTo` Reference () (Simple $ Name "a")
      it "evaluate monomorphic 'flip' correctly" $ do
        "{f b a => f a b}" `evaluatesTo` flip'
      it "evaluates applications of monomorphic 'flip' correctly" $ do
        "flip const a b" `evaluatesTo` Reference () (Simple $ Name "b")
      it "evaluates polymorphic 'id' correctly" $ do
        "{_ a => a}" `evaluatesTo` id''
      it "evaluates applications of polymorphic 'id' correctly" $ do
        "id' A a" `evaluatesTo` Reference () (Simple $ Name "a")
  where
    evaluatesTo ::
      ( ?read :: Map Canonical (Term () Canonical) )
      => String -> Term () Canonical -> Expectation
    evaluatesTo s v = let ?state = () in succeeds
      $ case parseString expression mempty s of
        Failure f -> throwError
          $ text "Parse failure on" <> text (show s)
        Success c -> do
          e <- evaluate $ fmap canonicalize c
          unless (e == v) . throwError $
            pretty e <> text "/=" <> pretty v

main :: IO ()
main = hspec . sequence_ $
  [ parsing
  , graphs
  , checks
  , evaluation
  ]


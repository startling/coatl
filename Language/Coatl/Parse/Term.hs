-- | This is the set of parsing rules revolving around expressions.
-- 
--   Most of these are polymorphic on a reference type, so that we
--   may parse lambda bodies using 'Maybe' as a type-level De Brujin
--   index -- 'Nothing' refers to the parameter to the innermost
--   lambda and 'Just' lets us embed any other reference that makes
--   sense in the outer context. To aid this, these functions are
--   parametrized by two values: `l`, a function lifting a top-level
--   expression into the context being parsed, and `f`, a parse rule
--   for the references in this context. We recur (as in 'lambda')
--   by passing @fmap Just . l@ in place of `l` and, for `f`, with
--   the string name of the argument as `arg`,
--   @Nothing <$ symbol arg@.
--
--   In any case, the most useful parsing rule in here is probably
--   'expression', which takes care of any top-level expression.
--
--   There remain some things to be done here: the scheme above
--   could probably be more nicely implemented using 'ReaderT'
--   (though there will be some complications with 'Parsing'
--   instances), and we will probably want to expose a way to specify
--   additions to the operator table.
module Language.Coatl.Parse.Term where
-- base
import Control.Applicative
import Control.Monad
import Data.Monoid
-- parsers
import Text.Parser.Expression
-- trifecta
import Text.Trifecta
-- coatl
import Language.Coatl.Abstract
import Language.Coatl.Parse.Common

-- | Parse any expression with a given `l` and `f`.
inner :: DeltaParsing f
  => (Term Span Identifier -> Term Span v)
  -> f v -> f (Term Span v)
inner l f = buildExpressionParser table (applied l f)
  <?> "expression" where
    -- This might annotate things slightly confusingly;
    -- I can't see of a better way to do it.
    binary (s :~ sp) t u = Applied sp 
      (Applied sp (l $ Reference sp $ Operator s) t) u
    table =
      [ [ Infix (binary <$> spanned (symbol "->")) AssocRight ]
      , [ Infix (binary <$> spanned (symbol "~")) AssocLeft ]
      ]

-- | Parse any non-zero-length sequence of simple values,
--   treating them as applications.
applied :: DeltaParsing f
  => (Term Span Identifier -> Term Span v)
  -> f v -> f (Term Span v)
applied l f = position >>= \start -> line >>= \li -> do
  -- This is messy in order to preserve source information
  -- about applications. Basically, having gathered information about
  -- the start, we get every applied value and its end position.
  as <- some $ (,) <$> single l f <*> position
  -- Then we fold over that list and create 'Applied's out of it, using
  -- the start position and each item's end position to create the
  -- right 'Span'.
  return . fst . flip foldl1 as $ \(t, _) (u, r) ->
    (Applied (Span start r li) t u, r)

-- | Parse a simple value; that is, a reference, lambda,
--   or any parenthesized expression.
single :: DeltaParsing f
  => (Term Span Identifier -> Term Span v)
  -> f v -> f (Term Span v)
single l f =
      try (annotated1 Reference f)
  <|> lambda l f
  <|> parens (inner l f)

-- | Given some parser matching a delimiter between arguments and
--   function bodies, parse a function.
function :: DeltaParsing f
  => f n
  => (Term Span Identifier -> Term Span v)
  -> f v
  -> f (Term Span v)
function d l f =
      try (d *> inner l f)
   <|> annotated1 Lambda
     ( ident nameStyle >>= \arg ->
       function d (fmap Just . l)
           $ (Nothing <$ symbol arg) <|> (Just <$> f))

-- | Parse a lambda, with the syntax `{a => a}`. This recurs
--   on `l` and `f` as described above.
lambda :: DeltaParsing f
  => (Term Span Identifier -> Term Span v)
  -> f v -> f (Term Span v)
lambda l f = braces $ function arrow l f where
  arrow :: (Monad m, TokenParsing m) => m ()
  arrow = reserve operatorStyle "=>"

-- | Parse any top-level expression.
expression :: DeltaParsing f => f (Term Span Identifier)
expression = topLevel inner

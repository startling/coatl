-- | This is the set of parsing rules revolving around expressions.
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

-- | Parse any top-level expression.
expression :: DeltaParsing m => m (Term Span Identifier)
expression = buildExpressionParser table applied
  <?> "expression" where
    -- This might annotate things slightly confusingly;
    -- I can't see of a better way to do it.
    binary (s :~ sp) t u = Applied sp 
      (Applied sp (Reference sp $ Operator s) t) u
    table =
      [ [ Infix (binary <$> spanned (symbol "->")) AssocRight ]
      , [ Infix (binary <$> spanned (symbol "~")) AssocLeft ]
      ]

-- | Parse any non-zero-length sequence of simple values,
--   treating them as applications.
applied :: DeltaParsing m => m (Term Span Identifier)
applied = position >>= \start -> line >>= \li -> do
  -- This is messy in order to preserve source information
  -- about applications. Basically, having gathered information about
  -- the start, we get every applied value and its end position.
  as <- some $ (,) <$> single <*> position
  -- Then we fold over that list and create 'Applied's out of it, using
  -- the start position and each item's end position to create the
  -- right 'Span'.
  return . fst . flip foldl1 as $ \(t, _) (u, r) ->
    (Applied (Span start r li) t u, r)

-- | Parse a simple value; that is, a reference, lambda,
--   or any parenthesized expression.
single :: DeltaParsing m => m (Term Span Identifier)
single =
      try (annotated1 Reference identifier)
  <|> lambda
  <|> parens expression

-- | Given some parser matching a delimiter between arguments and
--   function bodies, parse a function.
function :: DeltaParsing m => m () -> m (Term Span Identifier)
function d = (d *> expression)
   <|> (identifier >>=
      \i -> annotated1 (flip abstract i) (function d))

-- | Parse a lambda, with the syntax `{a => a}`.
lambda :: DeltaParsing m => m (Term Span Identifier)
lambda = braces $ function arrow where
  arrow :: (Monad m, TokenParsing m) => m ()
  arrow = reserve operatorStyle "=>"


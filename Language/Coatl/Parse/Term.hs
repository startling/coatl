-- | This is the set of parsing rules revolving around expressions. 
-- 
--   We will probably want to expose a way to specify additions
--   to the operator table.
module Language.Coatl.Parse.Term where
-- base
import Control.Applicative
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
  where
    -- | Parse a simple value; that is, an identifier, lambda,
    single :: DeltaParsing m => m (Term Span Identifier)
    single =
          (annotated1 Reference name)
      <|> (braces . function $ reserve operatorStyle "=>")
      <|> parens (annotated1 Reference operator <|> expression)

-- | Given some parser matching a delimiter between arguments and
--   function bodies, parse a function.
function :: DeltaParsing m => m () -> m (Term Span Identifier)
function d = (<|>) (d *> expression) $
  identifier >>= \i -> annotated1 (flip abstract i) (function d)

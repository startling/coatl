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
module Language.Coatl.Parser.Expression where
-- base
import Control.Applicative
import Data.Char
-- containers
import qualified Data.HashSet as H
-- mtl
import Control.Monad.Reader
-- parsers
import Text.Parser.Expression
import qualified Text.Parser.Token.Highlight as Highlight
-- trifecta
import Text.Trifecta
-- coatl
import Language.Coatl.Abstract
import Language.Coatl.Parser.Common

-- | Parse any expression with a given `l` and `f`.
inner :: DeltaParsing f
  => (Expression Span Identifier -> Expression Span v)
  -> f v -> f (Expression Span v)
inner l f = buildExpressionParser table (applied l f) where
  application l s sp a b = Application
    (Application (l $ Reference sp (Operator s)) a) b
  binary s a = Infix (application l s <$> spanning (symbol s)) a
  table =
    [ [ binary "->" AssocRight ]
    , [ binary "~" AssocLeft ]
    ]

-- | Parse any non-zero-length sequence of simple values,
--   treating them as applications.
applied :: DeltaParsing f
  => (Expression Span Identifier -> Expression Span v)
  -> f v -> f (Expression Span v)
applied l f = foldl1 Application <$> some (single l f)

-- | Parse a simple value; that is, a reference, lambda,
--   or any parenthesized expression.
single :: DeltaParsing f
  => (Expression Span Identifier -> Expression Span v)
  -> f v -> f (Expression Span v)
single l f =
      try (annotated1 Reference f)
  <|> lambda l f
  <|> parens (inner l f)

-- | Given some parser matching a delimiter between arguments and
--   function bodies, parse a function.
function :: DeltaParsing f
  => f n
  => (Expression Span Identifier -> Expression Span v)
  -> f v
  -> f (Expression Span v)
function d l f =
      try (d *> inner l f)
   <|> annotated1 Lambda
     ( ident nameStyle >>= \arg ->
       function d (fmap Just . l)
           $ (Nothing <$ symbol arg) <|> (Just <$> f))

-- | Parse a lambda, with the syntax `{a => a}`. This recurs
--   on `l` and `f` as described above.
lambda :: DeltaParsing f
  => (Expression Span Identifier -> Expression Span v)
  -> f v -> f (Expression Span v)
lambda l f = braces $ function arrow l f where
  arrow :: (Monad m, TokenParsing m) => m ()
  arrow = reserve operatorStyle "=>"

-- | Parse any top-level expression.
expression :: DeltaParsing f => f (Expression Span Identifier)
expression = topLevel inner

module Language.Coatl.Parser.Common where
-- base
import Control.Applicative
import Data.Char
-- containers
import qualified Data.HashSet as H
-- parsers
import Text.Parser.Expression
import qualified Text.Parser.Token.Highlight as Highlight
-- trifecta
import Text.Trifecta
-- coatl
import Language.Coatl.Abstract

-- | The identifier style defining ordinary names.
nameStyle :: CharParsing m => IdentifierStyle m
nameStyle = IdentifierStyle
  { _styleName = "name"
  , _styleStart = letter <|> oneOf "_"
  , _styleLetter = alphaNum <|> oneOf "-'"
  , _styleReserved = H.empty
  , _styleHighlight = Highlight.Identifier
  , _styleReservedHighlight = Highlight.ReservedIdentifier
  }

name :: (Monad m, TokenParsing m) => m Identifier
name = Name <$> ident nameStyle

-- | The identifier style defining operators.
operatorStyle :: (Monad m, CharParsing m) => IdentifierStyle m
operatorStyle = IdentifierStyle
  { _styleName = "operator"
  , _styleStart = part
  , _styleLetter = part <|> alphaNum
  , _styleReserved = H.fromList ["=>", "="]
  , _styleHighlight = Highlight.Operator
  , _styleReservedHighlight = Highlight.ReservedOperator
  } where
    part :: (Monad m, CharParsing m) => m Char
    part = satisfy $ \x -> (isSymbol x || isPunctuation x)
      && x `notElem` "\"_{}()"

operator :: (Monad m, TokenParsing m) => m Identifier
operator = Operator <$> ident operatorStyle

-- | Annotate some parser with a 'Span' and apply the 'Span' and
--   the results to some unary function.
annotated1 :: DeltaParsing f => (Span -> a -> r) -> f a -> f r
annotated1 f a = (\(t :~ s) -> f s t) <$> spanned a

-- | Annotate some parsers with a 'Span' and apply the 'Span' and
--   the results to some binary function.
annotated2 :: DeltaParsing f
  => (Span -> a -> b -> r) -> f a -> f b -> f r
annotated2 f a b = (\((t, u) :~ s) -> f s t u) <$> spanned
  ((,) <$> a <*> b)

-- | Parse with a top-level parametric parser.
topLevel :: (Monad f, TokenParsing f)
  => ((a -> a) -> f Identifier -> f a) -> f a
topLevel f = f id (name <|> parens operator)


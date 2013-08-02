{-# Language GeneralizedNewtypeDeriving #-}
module Language.Coatl.Parser.Declaration where
-- base
import Control.Applicative
import Control.Monad
-- transformers
import Control.Monad.Trans
-- mtl
import Control.Monad.State
-- trifecta
import Text.Trifecta
-- coatl
import Language.Coatl.Abstract
import Language.Coatl.Parser.Expression

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

-- | Parse a top-level type signature.
signature :: DeltaParsing m => m (Declaration Span Identifier)
signature = annotated2 Signature
  (name <|> parens operator)
  (reserve operatorStyle ":" *> expression)

-- | Parse a top-level definition declaration.
definition :: DeltaParsing m => m (Declaration Span Identifier)
definition = annotated2 Value
  (name <|> parens operator)
  (topLevel . function $ reserve operatorStyle "=")

-- | Parse any top-level declaration.
declaration :: DeltaParsing f => f (Declaration Span Identifier)
declaration =
       ((try signature <?> "type signature")
    <|> (try definition <?> "definition")) <* semi


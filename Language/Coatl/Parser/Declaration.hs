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
import Language.Coatl.Parser.Common
import Language.Coatl.Parser.Expression

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


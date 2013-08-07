{-# Language GeneralizedNewtypeDeriving #-}
module Language.Coatl.Parse.Declaration where
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
import Language.Coatl.Parse.Syntax
import Language.Coatl.Parse.Common
import Language.Coatl.Parse.Expression

-- | Parse a top-level type signature.
signature :: DeltaParsing m => m (Declaration Span Identifier)
signature = annotated2 Signature
  (name <|> parens operator)
  (reserve operatorStyle ":" *> expression)

-- | Parse a top-level definition declaration.
definition :: DeltaParsing m => m (Declaration Span Identifier)
definition = annotated2 Definition
  (name <|> parens operator)
  (topLevel . function $ reserve operatorStyle "=")

-- | Parse any top-level declaration.
declaration :: DeltaParsing f => f (Declaration Span Identifier)
declaration =
       ((try signature <?> "type signature")
    <|> (try definition <?> "definition")) <* semi


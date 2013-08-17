-- | Parse coatl declarations. This will probably handle things
--   like types in the future.
module Language.Coatl.Parse.Declaration where
-- base
import Control.Applicative
-- trifecta
import Text.Trifecta
-- coatl
import Language.Coatl.Abstract
import Language.Coatl.Parse.Common
import Language.Coatl.Parse.Term

-- | Parse a top-level type signature.
signature :: DeltaParsing m => m (Declaration Span Identifier)
signature = annotated2 Signature identifier  
  (reserve operatorStyle ":" *> expression)

-- | Parse a top-level definition declaration.
definition :: DeltaParsing m => m (Declaration Span Identifier)
definition = annotated2 Definition identifier
  (function $ reserve operatorStyle "=")

-- | Parse any top-level declaration.
declaration :: DeltaParsing f => f (Declaration Span Identifier)
declaration =
       ((try signature <?> "type signature")
    <|> (try definition <?> "definition")) <* semi

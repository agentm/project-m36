--parse type and data constructors
module TutorialD.Interpreter.Types where
import ProjectM36.Base
import ProjectM36.Interpreter
import Text.Megaparsec
import TutorialD.Interpreter.Base
import Control.Monad

class RelationalMarkerExpr a where
  parseMarkerP :: Parser a

instance RelationalMarkerExpr () where
  parseMarkerP = pure ()

typeConstructorNameP :: Parser TypeConstructorName
typeConstructorNameP = capitalizedIdentifier <* spaceConsumer

dataConstructorNameP :: Parser DataConstructorName
dataConstructorNameP = try $ do
  ident <- capitalizedIdentifier <* spaceConsumer
  when (ident `elem` ["True", "False"]) $ failure Nothing mempty --don't parse True or False as ConstructedAtoms (use NakedAtoms instead)
  pure ident

attributeNameP :: Parser AttributeName
attributeNameP = uncapitalizedOrQuotedIdentifier <* spaceConsumer

functionNameP :: Parser FunctionName
functionNameP = uncapitalizedOrQuotedIdentifier <* spaceConsumer

-- does not consumer following spaces
uncapitalizedOrQuotedIdentifier :: Parser StringType
uncapitalizedOrQuotedIdentifier =
  try uncapitalizedIdentifier <|> quotedIdentifier

-- | Upper case names are type names while lower case names are polymorphic typeconstructor arguments.
-- data *Either a b* = Left a | Right b
typeConstructorDefP :: Parser TypeConstructorDef
typeConstructorDefP = ADTypeConstructorDef <$> typeConstructorNameP <*> typeVarNamesP

typeVarNamesP :: Parser [TypeVarName]
typeVarNamesP = many typeVariableIdentifierP
  
-- data Either a b = *Left a* | *Right b*
dataConstructorDefP :: Parser DataConstructorDef
dataConstructorDefP = DataConstructorDef <$> typeConstructorNameP <*> many dataConstructorDefArgP

-- data *Either a b* = Left *a* | Right *b*
dataConstructorDefArgP :: Parser DataConstructorDefArg
dataConstructorDefArgP = DataConstructorDefTypeConstructorArg <$> (monoTypeConstructorP <|> parens typeConstructorP) <|>
                         DataConstructorDefTypeVarNameArg <$> typeConstructorNameP
  
-- relation{a Int} in type construction (no tuples parsed)
relationTypeConstructorP :: Parser TypeConstructor
relationTypeConstructorP = do
  reserved "relation"
  RelationAtomTypeConstructor <$> makeAttributeExprsP
  
--used in relation creation
makeAttributeExprsP :: RelationalMarkerExpr a => Parser [AttributeExprBase a]
makeAttributeExprsP = braces (sepBy attributeAndTypeNameP comma)

attributeAndTypeNameP :: RelationalMarkerExpr a => Parser (AttributeExprBase a)
attributeAndTypeNameP = AttributeAndTypeNameExpr <$> attributeNameP <*> typeConstructorP <*> parseMarkerP

typeIdentifierP :: Parser TypeConstructorName
typeIdentifierP = capitalizedIdentifier <* spaceConsumer

typeVariableIdentifierP :: Parser TypeVarName
typeVariableIdentifierP = uncapitalizedIdentifier <* spaceConsumer
                            
-- *Either Int Text*, *Int*
typeConstructorP :: Parser TypeConstructor                  
typeConstructorP = relationTypeConstructorP <|>
                   ADTypeConstructor <$> typeIdentifierP <*> many (monoTypeConstructorP <|> parens typeConstructorP) <|>
                   monoTypeConstructorP
                   
monoTypeConstructorP :: Parser TypeConstructor                   
monoTypeConstructorP = ADTypeConstructor <$> typeConstructorNameP <*> pure [] <|>
                       TypeVariable <$> typeVariableIdentifierP
                   
-- regular, uncapitalized name, but don't conflate it with a function (followed by parenthesis)
relVarNameP :: Parser RelVarName
relVarNameP = uncapitalizedOrQuotedIdentifier <* notFollowedBy "(" <* spaceConsumer

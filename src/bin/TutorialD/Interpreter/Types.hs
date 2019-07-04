--parse type and data constructors
module TutorialD.Interpreter.Types where
import ProjectM36.Base
import Text.Megaparsec
import TutorialD.Interpreter.Base

class RelationalMarkerExpr a where
  parseMarkerP :: Parser a

instance RelationalMarkerExpr () where
  parseMarkerP = pure ()

typeConstructorNameP :: Parser TypeConstructorName
typeConstructorNameP = capitalizedIdentifier

dataConstructorNameP :: Parser DataConstructorName
dataConstructorNameP = capitalizedIdentifier

-- | Upper case names are type names while lower case names are polymorphic typeconstructor arguments.
-- data *Either a b* = Left a | Right b
typeConstructorDefP :: Parser TypeConstructorDef
typeConstructorDefP = ADTypeConstructorDef <$> typeConstructorNameP <*> typeVarNamesP

typeVarNamesP :: Parser [TypeVarName]
typeVarNamesP = many uncapitalizedIdentifier 
  
-- data Either a b = *Left a* | *Right b*
dataConstructorDefP :: Parser DataConstructorDef
dataConstructorDefP = DataConstructorDef <$> typeConstructorNameP <*> many dataConstructorDefArgP

-- data *Either a b* = Left *a* | Right *b*
dataConstructorDefArgP :: Parser DataConstructorDefArg
dataConstructorDefArgP = DataConstructorDefTypeConstructorArg <$> (monoTypeConstructorP <|> parens typeConstructorP) <|>
                         DataConstructorDefTypeVarNameArg <$> uncapitalizedIdentifier
  
-- relation{a Int} in type construction (no tuples parsed)
relationTypeConstructorP :: Parser TypeConstructor
relationTypeConstructorP = do
  reserved "relation"
  RelationAtomTypeConstructor <$> makeAttributeExprsP
  
--used in relation creation
makeAttributeExprsP :: RelationalMarkerExpr a => Parser [AttributeExprBase a]
makeAttributeExprsP = braces (sepBy attributeAndTypeNameP comma)

attributeAndTypeNameP :: RelationalMarkerExpr a => Parser (AttributeExprBase a)
attributeAndTypeNameP = AttributeAndTypeNameExpr <$> identifier <*> typeConstructorP <*> parseMarkerP
                            
-- *Either Int Text*, *Int*
typeConstructorP :: Parser TypeConstructor                  
typeConstructorP = relationTypeConstructorP <|>
                   ADTypeConstructor <$> capitalizedIdentifier <*> many (monoTypeConstructorP <|> parens typeConstructorP) <|>
                   monoTypeConstructorP
                   
monoTypeConstructorP :: Parser TypeConstructor                   
monoTypeConstructorP = ADTypeConstructor <$> typeConstructorNameP <*> pure [] <|>
                       TypeVariable <$> uncapitalizedIdentifier
                   




--parse type and data constructors
module TutorialD.Interpreter.Types where
import ProjectM36.Base
import Text.Megaparsec.Text
import Text.Megaparsec
import TutorialD.Interpreter.Base
import ProjectM36.DataTypes.Primitive
import qualified Data.Text as T

-- | Upper case names are type names while lower case names are polymorphic typeconstructor arguments.
-- data *Either a b* = Left a | Right b
typeConstructorDefP :: Parser TypeConstructorDef
typeConstructorDefP = ADTypeConstructorDef <$> capitalizedIdentifier <*> typeVarNamesP

typeVarNamesP :: Parser [TypeVarName]
typeVarNamesP = many uncapitalizedIdentifier 
  
-- data Either a b = *Left a* | *Right b*
dataConstructorDefP :: Parser DataConstructorDef
dataConstructorDefP = DataConstructorDef <$> capitalizedIdentifier <*> many dataConstructorDefArgP

-- data *Either a b* = Left *a* | Right *b*
dataConstructorDefArgP :: Parser DataConstructorDefArg
dataConstructorDefArgP = parens (DataConstructorDefTypeConstructorArg <$> typeConstructorP) <|>
                         DataConstructorDefTypeConstructorArg <$> typeConstructorP <|>
                         DataConstructorDefTypeVarNameArg <$> uncapitalizedIdentifier
  
--built-in, nullary type constructors
-- Int, Text, etc.
primitiveTypeConstructorP :: Parser TypeConstructor
primitiveTypeConstructorP = choice (map (\(PrimitiveTypeConstructorDef name typ, _) -> do
                                               tName <- try $ symbol (T.unpack name)
                                               pure $ PrimitiveTypeConstructor tName typ)
                                       primitiveTypeConstructorMapping)
                               
-- *Either Int Text*, *Int*
typeConstructorP :: Parser TypeConstructor                  
typeConstructorP = primitiveTypeConstructorP <|>
                   ADTypeConstructor <$> capitalizedIdentifier <*> many typeConstructorArgP
                   
monoTypeConstructorP :: Parser TypeConstructor                   
monoTypeConstructorP = ADTypeConstructor <$> capitalizedIdentifier <*> pure []
                   
typeConstructorArgP :: Parser TypeConstructorArg                   
typeConstructorArgP = TypeConstructorArg <$> monoTypeConstructorP <|>
  parens (TypeConstructorArg <$> typeConstructorP) <|>
  TypeConstructorTypeVarArg <$> uncapitalizedIdentifier




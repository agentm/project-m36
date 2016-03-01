--parse type and data constructors
module TutorialD.Interpreter.Types where
import ProjectM36.Base
import Text.Parsec.String
import Text.Parsec
import TutorialD.Interpreter.Base
import ProjectM36.DataTypes.Primitive
import Control.Monad (liftM)
import qualified Data.Text as T

-- | Upper case names are type names while lower case names are polymorphic typeconstructor arguments.
-- data *Either a b* = Left a | Right b
typeConstructorDefP :: Parser TypeConstructorDef
typeConstructorDefP = ADTypeConstructorDef <$> liftM T.pack capitalizedIdentifier <*> typeVarNamesP

typeVarNamesP :: Parser [TypeVarName]
typeVarNamesP = do
  vars <- many uncapitalizedIdentifier 
  pure (map T.pack vars)
  
-- data Either a b = *Left a* | *Right b*
dataConstructorDefP :: Parser DataConstructorDef
dataConstructorDefP = DataConstructorDef <$> liftM T.pack identifier <*> many dataConstructorDefArgP

-- data *Either a b* = Left *a* | Right *b*
dataConstructorDefArgP :: Parser DataConstructorDefArg
dataConstructorDefArgP = parens (DataConstructorDefTypeConstructorArg <$> typeConstructorP) <|>
                         DataConstructorDefTypeVarNameArg <$> liftM T.pack uncapitalizedIdentifier
  
--built-in, nullary type constructors
-- Int, Text, etc.
primitiveTypeConstructorP :: Parser TypeConstructor
primitiveTypeConstructorP = choice (map (\(PrimitiveTypeConstructorDef name typ, _) -> do
                                               tName <- liftM T.pack (symbol (T.unpack name))
                                               pure $ PrimitiveTypeConstructor tName typ)
                                       primitiveTypeConstructorMapping)
                               
-- *Either Int Text*, *Int*
typeConstructorP :: Parser TypeConstructor                  
typeConstructorP = primitiveTypeConstructorP <|>
                   ADTypeConstructor <$> liftM T.pack identifier <*> many typeConstructorArgP
                   
typeConstructorArgP :: Parser TypeConstructorArg                   
typeConstructorArgP = TypeConstructorArg <$> typeConstructorP <|>
                      TypeConstructorTypeVarArg <$> liftM T.pack uncapitalizedIdentifier




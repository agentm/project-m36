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
-- Either a b
typeConstructorP :: Parser TypeConstructor
typeConstructorP = ADTypeConstructor <$> liftM T.pack capitalizedIdentifier <*> many typeConstructorArgP
  
typeConstructorArgP :: Parser TypeConstructorArg
typeConstructorArgP = TypeConstructorPolymorphicArg <$> liftM T.pack uncapitalizedIdentifier

dataConstructorP :: Parser DataConstructor
dataConstructorP = DataConstructor <$> liftM T.pack identifier <*> many dataConstructorArgP

--Either Int Text
--Either a Int
--Either (Either a b) Int
dataConstructorArgP :: Parser TypeConstructorArg
dataConstructorArgP = primitiveTypeConstructorArgP <|>
                      TypeConstructorPolymorphicArg <$> liftM T.pack uncapitalizedIdentifier <|>
                      parens (TypeConstructorArg <$> typeConstructorP)
  
primitiveTypeConstructorArgP :: Parser TypeConstructorArg
primitiveTypeConstructorArgP = choice (map (\(PrimitiveTypeConstructor name typ, _) -> do
                                               tName <- liftM T.pack (symbol (T.unpack name))
                                               pure $ TypeConstructorArg (PrimitiveTypeConstructor tName typ))
                                       primitiveTypeConstructors)
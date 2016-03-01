module ProjectM36.TypeConstructorDef where
import ProjectM36.Base
import qualified Data.Set as S

name :: TypeConstructorDef -> TypeConstructorName
name (ADTypeConstructorDef nam _) = nam
name (PrimitiveTypeConstructorDef nam _) = nam

typeVars :: TypeConstructorDef -> [TypeVarName]
typeVars (PrimitiveTypeConstructorDef _ _) = []                      
typeVars (ADTypeConstructorDef _ args) = args
  

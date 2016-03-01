module ProjectM36.TypeConstructor where
import ProjectM36.Base
import qualified Data.Set as S

name :: TypeConstructor -> TypeConstructorName
name (ADTypeConstructor name' _) = name'
name (PrimitiveTypeConstructor name' _) = name'

arguments :: TypeConstructor -> [TypeConstructorArg]
arguments (ADTypeConstructor _ args) = args
arguments (PrimitiveTypeConstructor _ _) = []

typeVars :: TypeConstructor -> S.Set TypeVarName
typeVars (PrimitiveTypeConstructor _ _) = S.empty
typeVars (ADTypeConstructor _ args) = S.unions (map typeVarsForArg args)
  
typeVarsForArg :: TypeConstructorArg -> S.Set TypeVarName
typeVarsForArg (TypeConstructorArg arg) = typeVars arg
typeVarsForArg (TypeConstructorTypeVarArg pName) = S.singleton pName
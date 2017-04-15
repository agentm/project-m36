module ProjectM36.TypeConstructor where
import ProjectM36.Base
import qualified Data.Set as S

name :: TypeConstructor -> TypeConstructorName
name (ADTypeConstructor name' _) = name'
name (PrimitiveTypeConstructor name' _) = name'
name (TypeVariable _) = error "spam" --v --not really the name, but this is used for display only

arguments :: TypeConstructor -> [TypeConstructor]
arguments (ADTypeConstructor _ args) = args
arguments (PrimitiveTypeConstructor _ _) = []
arguments (TypeVariable _) = []

typeVars :: TypeConstructor -> S.Set TypeVarName
typeVars (PrimitiveTypeConstructor _ _) = S.empty
typeVars (ADTypeConstructor _ args) = S.unions (map typeVars args)
typeVars (TypeVariable v) = S.singleton v
  

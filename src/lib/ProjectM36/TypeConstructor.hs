module ProjectM36.TypeConstructor where
import ProjectM36.Base

name :: TypeConstructor -> TypeConstructorName
name (ADTypeConstructor name' _) = name'
name (PrimitiveTypeConstructor name' _) = name'
name (RelationAtomTypeConstructor _) = error "name called on RelationAtomTypeConstructor"
name (TypeVariable _) = error "name called on TypeVariable" --v --not really the name, but this is used for display only

arguments :: TypeConstructor -> [TypeConstructor]
arguments (ADTypeConstructor _ args) = args
arguments (PrimitiveTypeConstructor _ _) = []
arguments (RelationAtomTypeConstructor _) = []
arguments (TypeVariable _) = []


  

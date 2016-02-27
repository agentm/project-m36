module ProjectM36.TypeConstructor where
import ProjectM36.Base

name :: TypeConstructor -> TypeConstructorName
name (ADTypeConstructor nam _) = nam
name (PrimitiveTypeConstructor nam _) = nam

arguments :: TypeConstructor -> [TypeConstructorArg]
arguments (PrimitiveTypeConstructor _ _) = []
arguments (ADTypeConstructor _ tConsArgs) = tConsArgs

polymorphicVariables :: TypeConstructor -> [TypeConstructorPolymorphicName]
polymorphicVariables (PrimitiveTypeConstructor _ _) = []
polymorphicVariables (ADTypeConstructor _ tConsArgs) = concatMap polymorphicVariablesInArg tConsArgs
  
polymorphicVariablesInArg :: TypeConstructorArg -> [TypeConstructorPolymorphicName]   
polymorphicVariablesInArg (TypeConstructorPolymorphicArg nam) = [nam]
polymorphicVariablesInArg (TypeConstructorArg tCons) = polymorphicVariables tCons

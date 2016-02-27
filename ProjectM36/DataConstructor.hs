module ProjectM36.DataConstructor where
import ProjectM36.Base
import qualified ProjectM36.TypeConstructor as TC

emptyDataConstructor :: DataConstructorName -> DataConstructor
emptyDataConstructor name' = DataConstructor name' []

name :: DataConstructor -> DataConstructorName
name (DataConstructor name' _) = name'

fields :: DataConstructor -> [TypeConstructorArg]
fields (DataConstructor _ types) = types

polymorphicVariables :: DataConstructor -> [TypeConstructorPolymorphicName]
polymorphicVariables (DataConstructor _ tConsArgs) = concatMap TC.polymorphicVariablesInArg tConsArgs
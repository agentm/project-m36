module ProjectM36.DataConstructorDef where
import ProjectM36.Base
import qualified ProjectM36.TypeConstructor as TC
import qualified Data.Set as S

emptyDataConstructor :: DataConstructorName -> DataConstructorDef
emptyDataConstructor name' = DataConstructorDef name' []

name :: DataConstructorDef -> DataConstructorName
name (DataConstructorDef name' _) = name'

fields :: DataConstructorDef -> [DataConstructorDefArg]
fields (DataConstructorDef _ args) = args

typeVars :: DataConstructorDef -> S.Set TypeVarName
typeVars (DataConstructorDef _ tConsArgs) = S.unions $ map typeVarsInDefArg tConsArgs

typeVarsInDefArg :: DataConstructorDefArg -> S.Set TypeVarName
typeVarsInDefArg (DataConstructorDefTypeConstructorArg tCons) = TC.typeVars tCons
typeVarsInDefArg (DataConstructorDefTypeVarNameArg pVarName) = S.singleton pVarName
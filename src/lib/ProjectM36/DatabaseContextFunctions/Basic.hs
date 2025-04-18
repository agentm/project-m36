module ProjectM36.DatabaseContextFunctions.Basic where
import ProjectM36.DatabaseContextFunction
import ProjectM36.DatabaseContext.Types
import ProjectM36.Base
import qualified Data.HashSet as HS

basicDatabaseContextFunctions :: DatabaseContextFunctions
basicDatabaseContextFunctions = HS.fromList [
  Function { funcName = "deleteAll",
             funcType = [],
             funcBody = FunctionBuiltInBody (\_ ctx -> pure $ ctx { relationVariables = emptyValue })
           }
  ]

--the precompiled functions are special because they cannot be serialized. Their names are therefore used in perpetuity so that the functions can be "serialized" (by name).
precompiledDatabaseContextFunctions :: DatabaseContextFunctions
precompiledDatabaseContextFunctions = HS.filter (not . isScriptedDatabaseContextFunction) basicDatabaseContextFunctions

{-# LANGUAGE TupleSections #-}
module ProjectM36.DatabaseContextFunctions.Basic where
import ProjectM36.DatabaseContextFunction
import ProjectM36.DatabaseContext.Types
import ProjectM36.ValueMarker
import ProjectM36.Base
import ProjectM36.AccessControlList
import qualified Data.HashSet as HS
import qualified Data.Map as M
import qualified Data.Set as S

basicDatabaseContextFunctions :: DatabaseContextFunctions
basicDatabaseContextFunctions = HS.fromList [
  Function { funcName = "deleteAll",
             funcType = [],
             funcBody = FunctionBuiltInBody (\_ ctx -> pure $ ctx { relationVariables = emptyValue }),
             funcACL = defaultACL
           }
  ]

defaultACL :: DBCFunctionAccessControlList
defaultACL = AccessControlList (M.singleton adminRoleId (M.fromList (map (, True) (S.toList allPermissions))))

--the precompiled functions are special because they cannot be serialized. Their names are therefore used in perpetuity so that the functions can be "serialized" (by name).
precompiledDatabaseContextFunctions :: DatabaseContextFunctions
precompiledDatabaseContextFunctions = HS.filter (not . isScriptedDatabaseContextFunction) basicDatabaseContextFunctions

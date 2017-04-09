module ProjectM36.DatabaseContextFunction where
--implements functions which operate as: [Atom] -> DatabaseContextExpr -> Either RelationalError DatabaseContextExpr
import ProjectM36.Base
import ProjectM36.Error
import qualified Data.HashSet as HS
import qualified Data.Map as M

emptyDatabaseContextFunction :: DatabaseContextFunctionName -> DatabaseContextFunction
emptyDatabaseContextFunction name = DatabaseContextFunction { 
  dbcFuncName = name,
  dbcFuncType = [],
  dbcFuncBody = DatabaseContextFunctionBody Nothing (\_ ctx -> pure ctx)
  }

databaseContextFunctionForName :: DatabaseContextFunctionName -> DatabaseContextFunctions -> Either RelationalError DatabaseContextFunction
databaseContextFunctionForName funcName funcs = if HS.null foundFunc then
                                                   Left $ NoSuchFunctionError funcName
                                                else
                                                  Right (head (HS.toList foundFunc))
  where
    foundFunc = HS.filter (\(DatabaseContextFunction name _ _) -> name == funcName) funcs

evalDatabaseContextFunction :: DatabaseContextFunction -> [Atom] -> DatabaseContext -> Either RelationalError DatabaseContext
evalDatabaseContextFunction func args ctx = case dbcFuncBody func of
  (DatabaseContextFunctionBody _ f) -> case f args ctx of
    Left err -> Left (DatabaseContextFunctionUserError err)
    Right c -> pure c
  
basicDatabaseContextFunctions :: DatabaseContextFunctions
basicDatabaseContextFunctions = HS.fromList [
  DatabaseContextFunction { dbcFuncName = "deleteAll",
                            dbcFuncType = [],
                            dbcFuncBody = DatabaseContextFunctionBody Nothing (\_ ctx -> pure $ ctx { relationVariables = M.empty })
                          }
  ]
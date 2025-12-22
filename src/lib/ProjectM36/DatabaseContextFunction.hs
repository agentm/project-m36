module ProjectM36.DatabaseContextFunction where
--implements functions which operate as: [Atom] -> DatabaseContextExpr -> Either RelationalError DatabaseContextExpr
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Attribute as A
import ProjectM36.Relation
import ProjectM36.AtomType
import ProjectM36.Function
import ProjectM36.DatabaseContext.Types
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Data.Maybe (isJust)
import Data.Default

externalDatabaseContextFunction :: DatabaseContextFunctionBodyType -> DatabaseContextFunctionBody
externalDatabaseContextFunction = FunctionBuiltInBody

emptyDatabaseContextFunction :: FunctionName -> DatabaseContextFunction
emptyDatabaseContextFunction name = Function { 
  funcName = name,
  funcType = [],
  funcBody = FunctionBuiltInBody (\_ _ ctx -> pure ctx),
  funcACL = def
  }

databaseContextFunctionForName :: FunctionName -> DatabaseContextFunctions -> Either RelationalError DatabaseContextFunction
databaseContextFunctionForName funcName' funcs =
  case HS.toList $ HS.filter (\f -> funcName f == funcName') funcs of
    [] -> Left $ NoSuchFunctionError funcName'
    x : _ -> Right x

evalDatabaseContextFunction :: DatabaseContextFunction -> DatabaseContextFunctionUtils -> [Atom] -> DatabaseContext -> Either RelationalError DatabaseContext
evalDatabaseContextFunction func utils args ctx =
  case f utils args ctx of
    Left err -> Left (DatabaseContextFunctionUserError err)
    Right c -> pure c
  where
   f = function (funcBody func)
                                   
                                
isScriptedDatabaseContextFunction :: DatabaseContextFunction -> Bool
isScriptedDatabaseContextFunction func = isJust (functionScript func)
  
databaseContextFunctionReturnType :: TypeConstructor -> TypeConstructor
databaseContextFunctionReturnType tCons = ADTypeConstructor "Either" [
  ADTypeConstructor "DatabaseContextFunctionError" [],
  tCons]
                                          
{-createScriptedDatabaseContextFunction :: FunctionName -> [TypeConstructor] -> TypeConstructor -> FunctionBodyScript -> DatabaseContextIOExpr
createScriptedDatabaseContextFunction funcName' argsIn retArg = AddDatabaseContextFunction funcName' (argsIn ++ [databaseContextFunctionReturnType retArg])
-}

databaseContextFunctionsAsRelation :: DatabaseContextFunctions -> Either RelationalError Relation
databaseContextFunctionsAsRelation dbcFuncs = mkRelationFromList attrs tups
  where
    attrs = A.attributesFromList [Attribute "name" TextAtomType,
                                  Attribute "arguments" TextAtomType]
    tups = map dbcFuncToTuple (HS.toList dbcFuncs)
    dbcFuncToTuple func = [TextAtom (funcName func),
                           TextAtom (dbcTextType (funcType func))]
    dbcTextType typ = T.intercalate " -> " (map prettyAtomType typ ++ ["DatabaseContext", "DatabaseContext"])


{-# LANGUAGE CPP #-}
module ProjectM36.DatabaseContextFunction where
--implements functions which operate as: [Atom] -> DatabaseContextExpr -> Either RelationalError DatabaseContextExpr
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Serialise.Base ()
import ProjectM36.Attribute as A
import ProjectM36.Relation
import ProjectM36.AtomType
import ProjectM36.Function
import qualified Data.HashSet as HS
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe (isJust)

externalDatabaseContextFunction :: DatabaseContextFunctionBodyType -> DatabaseContextFunctionBody
externalDatabaseContextFunction = FunctionBuiltInBody

emptyDatabaseContextFunction :: FunctionName -> DatabaseContextFunction
emptyDatabaseContextFunction name = Function { 
  funcName = name,
  funcType = [],
  funcBody = FunctionBuiltInBody (\_ ctx -> pure ctx)
  }

databaseContextFunctionForName :: FunctionName -> DatabaseContextFunctions -> Either RelationalError DatabaseContextFunction
databaseContextFunctionForName funcName' funcs = if HS.null foundFunc then
                                                   Left $ NoSuchFunctionError funcName'
                                                else
                                                  Right (head (HS.toList foundFunc))
  where
    foundFunc = HS.filter (\f -> funcName f == funcName') funcs

evalDatabaseContextFunction :: DatabaseContextFunction -> [Atom] -> DatabaseContext -> Either RelationalError DatabaseContext
evalDatabaseContextFunction func args ctx =
  case f args ctx of
    Left err -> Left (DatabaseContextFunctionUserError err)
    Right c -> pure c
  where
   f = function (funcBody func)
   
basicDatabaseContextFunctions :: DatabaseContextFunctions
basicDatabaseContextFunctions = HS.fromList [
  Function { funcName = "deleteAll",
             funcType = [],
             funcBody = FunctionBuiltInBody (\_ ctx -> pure $ ctx { relationVariables = M.empty })
           }
  ]
                                
--the precompiled functions are special because they cannot be serialized. Their names are therefore used in perpetuity so that the functions can be "serialized" (by name).
precompiledDatabaseContextFunctions :: DatabaseContextFunctions
precompiledDatabaseContextFunctions = HS.filter (not . isScriptedDatabaseContextFunction) basicDatabaseContextFunctions
                                
isScriptedDatabaseContextFunction :: DatabaseContextFunction -> Bool
isScriptedDatabaseContextFunction func = isJust (functionScript func)
  
databaseContextFunctionReturnType :: TypeConstructor -> TypeConstructor
databaseContextFunctionReturnType tCons = ADTypeConstructor "Either" [
  ADTypeConstructor "DatabaseContextFunctionError" [],
  tCons]
                                          
createScriptedDatabaseContextFunction :: FunctionName -> [TypeConstructor] -> TypeConstructor -> FunctionBodyScript -> DatabaseContextIOExpr
createScriptedDatabaseContextFunction funcName' argsIn retArg = AddDatabaseContextFunction funcName' (argsIn ++ [databaseContextFunctionReturnType retArg])

databaseContextFunctionsAsRelation :: DatabaseContextFunctions -> Either RelationalError Relation
databaseContextFunctionsAsRelation dbcFuncs = mkRelationFromList attrs tups
  where
    attrs = A.attributesFromList [Attribute "name" TextAtomType,
                                  Attribute "arguments" TextAtomType]
    tups = map dbcFuncToTuple (HS.toList dbcFuncs)
    dbcFuncToTuple func = [TextAtom (funcName func),
                           TextAtom (dbcTextType (funcType func))]
    dbcTextType typ = T.intercalate " -> " (map prettyAtomType typ ++ ["DatabaseContext", "DatabaseContext"])


{-# LANGUAGE CPP #-}
module ProjectM36.AtomFunction where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Relation
import ProjectM36.AtomType
import ProjectM36.AtomFunctionError
import ProjectM36.ScriptSession
import qualified ProjectM36.Attribute as A
import qualified Data.HashSet as HS
import qualified Data.Text as T


foldAtomFuncType :: AtomType -> AtomType -> [AtomType]
--the underscore in the attribute name means that any attributes are acceptable
foldAtomFuncType foldType returnType = [RelationAtomType (A.attributesFromList [Attribute "_" foldType]), returnType]

atomFunctionForName :: AtomFunctionName -> AtomFunctions -> Either RelationalError AtomFunction
atomFunctionForName funcName funcSet = if HS.null foundFunc then
                                         Left $ NoSuchFunctionError funcName
                                        else
                                         Right $ head $ HS.toList foundFunc
  where
    foundFunc = HS.filter (\(AtomFunction name _ _) -> name == funcName) funcSet

-- | Create a junk named atom function for use with searching for an already existing function in the AtomFunctions HashSet.
emptyAtomFunction :: AtomFunctionName -> AtomFunction
emptyAtomFunction name = AtomFunction { atomFuncName = name,
                                        atomFuncType = [TypeVariableType "a", TypeVariableType "a"],
                                        atomFuncBody = AtomFunctionBody Nothing (\(x:_) -> pure x) }
                                          
                                          
-- | AtomFunction constructor for compiled-in functions.
compiledAtomFunction :: AtomFunctionName -> [AtomType] -> AtomFunctionBodyType -> AtomFunction
compiledAtomFunction name aType body = AtomFunction { atomFuncName = name,
                                                      atomFuncType = aType,
                                                      atomFuncBody = AtomFunctionBody Nothing body }

--the atom function really should offer some way to return an error
evalAtomFunction :: AtomFunction -> [Atom] -> Either AtomFunctionError Atom
evalAtomFunction func args = case atomFuncBody func of
  (AtomFunctionBody _ f) -> f args

--expect "Int -> Either AtomFunctionError Int"
--return "Int -> Int" for funcType
extractAtomFunctionType :: [TypeConstructor] -> Either RelationalError [TypeConstructor]
extractAtomFunctionType typeIn = do
  let atomArgs = take (length typeIn - 1) typeIn
      --expected atom ret value - used to make funcType
      lastArg = take 1 (reverse typeIn)
  case lastArg of
    [ADTypeConstructor "Either" 
     [ADTypeConstructor "AtomFunctionError" [],
      atomRetArg]] ->
      pure (atomArgs ++ [atomRetArg])
    otherType -> 
      Left (ScriptError (TypeCheckCompilationError "function returning \"Either AtomFunctionError a\"" (show otherType)))
    
isScriptedAtomFunction :: AtomFunction -> Bool    
isScriptedAtomFunction func = case atomFuncBody func of
  AtomFunctionBody (Just _) _ -> True
  AtomFunctionBody Nothing _ -> False
  
atomFunctionScript :: AtomFunction -> Maybe AtomFunctionBodyScript
atomFunctionScript func = case atomFuncBody func of
  AtomFunctionBody script _ -> script
  
-- | Create a 'DatabaseContextIOExpr' which can be used to load a new atom function written in Haskell and loaded at runtime.
createScriptedAtomFunction :: AtomFunctionName -> [TypeConstructor] -> TypeConstructor -> AtomFunctionBodyScript -> DatabaseContextIOExpr
createScriptedAtomFunction funcName argsType retType = AddAtomFunction funcName (
  argsType ++ [ADTypeConstructor "Either" [
                ADTypeConstructor "AtomFunctionError" [],                     
                retType]])

loadAtomFunctions :: ModName -> FuncName -> FilePath -> IO (Either LoadSymbolError [AtomFunction])
#ifdef PM36_HASKELL_SCRIPTING
loadAtomFunctions = loadFunction
#else
loadAtomFunctions _ _ _ = pure (Left LoadSymbolError)
#endif

atomFunctionsAsRelation :: AtomFunctions -> Either RelationalError Relation
atomFunctionsAsRelation funcs = mkRelationFromList attrs tups
  where tups = map atomFuncToTuple (HS.toList funcs)
        attrs = A.attributesFromList [Attribute "name" TextAtomType,
                                     Attribute "arguments" TextAtomType]
        atomFuncToTuple aFunc = [TextAtom (atomFuncName aFunc),
                                 TextAtom (atomFuncTypeToText aFunc)]
        atomFuncTypeToText aFunc = T.intercalate " -> " (map prettyAtomType (atomFuncType aFunc))

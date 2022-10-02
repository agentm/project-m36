{-# LANGUAGE CPP #-}
module ProjectM36.AtomFunction where
import ProjectM36.Base
import ProjectM36.Serialise.Base ()
import ProjectM36.Error
import ProjectM36.Relation
import ProjectM36.AtomType
import ProjectM36.AtomFunctionError
import ProjectM36.Function
import qualified ProjectM36.Attribute as A
import qualified Data.HashSet as HS
import qualified Data.Text as T

foldAtomFuncType :: AtomType -> AtomType -> [AtomType]
--the underscore in the attribute name means that any attributes are acceptable
foldAtomFuncType foldType returnType = [RelationAtomType (A.attributesFromList [Attribute "_" foldType]), returnType]

atomFunctionForName :: FunctionName -> AtomFunctions -> Either RelationalError AtomFunction
atomFunctionForName funcName' funcSet = if HS.null foundFunc then
                                         Left $ NoSuchFunctionError funcName'
                                        else
                                         Right $ head $ HS.toList foundFunc
  where
    foundFunc = HS.filter (\f -> funcName f == funcName') funcSet

-- | Create a junk named atom function for use with searching for an already existing function in the AtomFunctions HashSet.
emptyAtomFunction :: FunctionName -> AtomFunction
emptyAtomFunction name = Function { funcName = name,
                                    funcType = [TypeVariableType "a", TypeVariableType "a"],
                                    funcBody = FunctionBuiltInBody $
                                               \case
                                                 x:_ -> pure x
                                                 _ -> Left AtomFunctionTypeMismatchError
                                  }
                                          
                                          
-- | AtomFunction constructor for compiled-in functions.
compiledAtomFunction :: FunctionName -> [AtomType] -> AtomFunctionBodyType -> AtomFunction
compiledAtomFunction name aType body = Function { funcName = name,
                                                  funcType = aType,
                                                  funcBody = FunctionBuiltInBody body }

--the atom function really should offer some way to return an error
evalAtomFunction :: AtomFunction -> [Atom] -> Either AtomFunctionError Atom
evalAtomFunction func = function (funcBody func)

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
isScriptedAtomFunction func = case funcBody func of
  FunctionScriptBody{} -> True
  _ -> False
  
-- | Create a 'DatabaseContextIOExpr' which can be used to load a new atom function written in Haskell and loaded at runtime.
createScriptedAtomFunction :: FunctionName -> [TypeConstructor] -> TypeConstructor -> FunctionBodyScript -> DatabaseContextIOExpr
createScriptedAtomFunction funcName' argsType retType = AddAtomFunction funcName' (
  argsType ++ [ADTypeConstructor "Either" [
                ADTypeConstructor "AtomFunctionError" [],                     
                retType]])

{-
loadAtomFunctions :: ModName -> FuncName -> Maybe FilePath -> FilePath -> IO (Either LoadSymbolError [AtomFunction])
#ifdef PM36_HASKELL_SCRIPTING
Loadatomfunctions modName funcName' mModDir objPath =
  case mModDir of
    Just modDir -> do
      eNewFs <- loadFunctionFromDirectory LoadAutoObjectFile modName funcName' modDir objPath
      case eNewFs of
        Left err -> pure (Left err)
        Right newFs ->
          pure (Right (processFuncs newFs))
    Nothing -> do
      loadFunction LoadAutoObjectFile modName funcName' objPath
 where
   --functions inside object files probably won't have the right function body metadata
   processFuncs = map processor
   processor newF = newF { funcBody = processObjectLoadedFunctionBody modName funcName' objPath (funcBody newF)}
#else
loadAtomFunctions _ _ _ _ = pure (Left LoadSymbolError)
#endif
-}

atomFunctionsAsRelation :: AtomFunctions -> Either RelationalError Relation
atomFunctionsAsRelation funcs = mkRelationFromList attrs tups
  where tups = map atomFuncToTuple (HS.toList funcs)
        attrs = A.attributesFromList [Attribute "name" TextAtomType,
                                     Attribute "arguments" TextAtomType]
        atomFuncToTuple aFunc = [TextAtom (funcName aFunc),
                                 TextAtom (atomFuncTypeToText aFunc)]
        atomFuncTypeToText aFunc = T.intercalate " -> " (map prettyAtomType (funcType aFunc))

--for calculating the merkle hash
  
-- | Used to mark functions which are loaded externally from the server.      
externalAtomFunction :: AtomFunctionBodyType -> AtomFunctionBody
externalAtomFunction = FunctionBuiltInBody

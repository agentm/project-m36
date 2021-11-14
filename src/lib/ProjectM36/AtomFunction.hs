{-# LANGUAGE CPP #-}
module ProjectM36.AtomFunction where
import ProjectM36.Base
import ProjectM36.Serialise.Base ()
import ProjectM36.Error
import ProjectM36.Relation
import ProjectM36.AtomType
import ProjectM36.AtomFunctionError
import ProjectM36.ScriptSession
import qualified ProjectM36.Attribute as A
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Codec.Winery

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
                                        atomFuncBody = AtomFunctionBuiltInBody (\(x:_) -> pure x) }
                                          
                                          
-- | AtomFunction constructor for compiled-in functions.
compiledAtomFunction :: AtomFunctionName -> [AtomType] -> AtomFunctionBodyType -> AtomFunction
compiledAtomFunction name aType body = AtomFunction { atomFuncName = name,
                                                      atomFuncType = aType,
                                                      atomFuncBody = AtomFunctionBuiltInBody body }

--the atom function really should offer some way to return an error
evalAtomFunction :: AtomFunction -> [Atom] -> Either AtomFunctionError Atom
evalAtomFunction func args = (function (atomFuncBody func)) args

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
  AtomFunctionScriptBody{} -> True
  _ -> False
  
atomFunctionScript :: AtomFunction -> Maybe AtomFunctionBodyScript
atomFunctionScript func = case atomFuncBody func of
  AtomFunctionScriptBody script _ -> Just script
  _ -> Nothing
  
-- | Create a 'DatabaseContextIOExpr' which can be used to load a new atom function written in Haskell and loaded at runtime.
createScriptedAtomFunction :: AtomFunctionName -> [TypeConstructor] -> TypeConstructor -> AtomFunctionBodyScript -> DatabaseContextIOExpr
createScriptedAtomFunction funcName argsType retType = AddAtomFunction funcName (
  argsType ++ [ADTypeConstructor "Either" [
                ADTypeConstructor "AtomFunctionError" [],                     
                retType]])

loadAtomFunctions :: ModName -> FuncName -> Maybe FilePath -> FilePath -> IO (Either LoadSymbolError [AtomFunction])
#ifdef PM36_HASKELL_SCRIPTING
loadAtomFunctions modName funcName mModDir objPath =
  case mModDir of
    Just modDir -> do
      eNewFs <- loadFunctionFromDirectory LoadAutoObjectFile modName funcName modDir objPath
      case eNewFs of
        Left err -> pure (Left err)
        Right newFs ->
          pure (Right (processFuncs newFs))
    Nothing -> do
      loadFunction LoadAutoObjectFile modName funcName objPath
 where
   --functions inside object files probably won't have the right function body metadata
   processFuncs = map processor
   processor newF = newF { atomFuncBody = processObjectLoadedFunctionBody modName funcName objPath (atomFuncBody newF)}
#else
loadAtomFunctions _ _ _ _ = pure (Left LoadSymbolError)
#endif

atomFunctionsAsRelation :: AtomFunctions -> Either RelationalError Relation
atomFunctionsAsRelation funcs = mkRelationFromList attrs tups
  where tups = map atomFuncToTuple (HS.toList funcs)
        attrs = A.attributesFromList [Attribute "name" TextAtomType,
                                     Attribute "arguments" TextAtomType]
        atomFuncToTuple aFunc = [TextAtom (atomFuncName aFunc),
                                 TextAtom (atomFuncTypeToText aFunc)]
        atomFuncTypeToText aFunc = T.intercalate " -> " (map prettyAtomType (atomFuncType aFunc))

--for calculating the merkle hash
hashBytes :: AtomFunction -> BL.ByteString
hashBytes func = BL.fromChunks [serialise (atomFuncName func),
                                serialise (atomFuncType func),
                                bodyBin
                               ]
  where
    bodyBin = case atomFuncBody func of
                AtomFunctionScriptBody mScript _ -> serialise mScript
                AtomFunctionBuiltInBody _ -> ""
                AtomFunctionObjectLoadedBody f m n _ -> serialise (f,m,n)

-- | Return the underlying function to run the AtomFunction.
function :: AtomFunctionBody -> AtomFunctionBodyType
function (AtomFunctionScriptBody _ f) = f
function (AtomFunctionBuiltInBody f) = f
function (AtomFunctionObjectLoadedBody _ _ _ f) = f
  
-- | Change atom function definition to reference proper object file source. Useful when moving the object file into the database directory.
processObjectLoadedFunctionBody :: ObjectModuleName -> ObjectFileEntryFunctionName -> FilePath -> AtomFunctionBody -> AtomFunctionBody
processObjectLoadedFunctionBody modName fentry objPath body =
  AtomFunctionObjectLoadedBody objPath modName fentry f
  where
    f = function body
      
externalAtomFunction :: AtomFunctionBodyType -> AtomFunctionBody
externalAtomFunction = AtomFunctionBuiltInBody

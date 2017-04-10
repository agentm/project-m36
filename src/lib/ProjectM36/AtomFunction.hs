module ProjectM36.AtomFunction where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.AtomFunctionError
import qualified ProjectM36.Attribute as A
import qualified Data.HashSet as HS

foldAtomFuncType :: AtomType -> AtomType -> [AtomType]
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
                                        atomFuncType = [AnyAtomType, AnyAtomType],
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
    (ADTypeConstructor "Either" 
     (TypeConstructorArg 
      (ADTypeConstructor "AtomFunctionError" []):
      (TypeConstructorArg atomRetArg):[])):[] -> do
      pure (atomArgs ++ [atomRetArg])
    otherType -> Left (ScriptError (TypeCheckCompilationError "function returning \"Either AtomFunctionError a\"" (show otherType)))
module ProjectM36.AtomFunction where
import ProjectM36.Base
import ProjectM36.Error
import qualified ProjectM36.Attribute as A
import qualified Data.HashSet as HS

foldAtomFuncType :: AtomType -> AtomType -> [AtomType]
foldAtomFuncType foldType returnType = [RelationAtomType (A.attributesFromList [Attribute "_" foldType]), returnType]

atomFunctionForName :: AtomFunctionName -> AtomFunctions -> Either RelationalError AtomFunction
atomFunctionForName funcName funcSet = if HS.null foundFunc then
                                         Left $ NoSuchTupleExprFunctionError funcName
                                        else
                                         Right $ head $ HS.toList foundFunc
  where
    foundFunc = HS.filter (\(AtomFunction name _ _) -> name == funcName) funcSet

-- | Create a junk named atom function for use with searching for an already existing function in the AtomFunctions HashSet.
emptyAtomFunction :: AtomFunctionName -> AtomFunction
emptyAtomFunction name = AtomFunction { atomFuncName = name,
                                        atomFuncType = [AnyAtomType, AnyAtomType],
                                        atomFuncBody = AtomFunctionBody Nothing (\(x:_) -> x) }
                                          
                                          
-- | AtomFunction constructor for compiled-in functions.
compiledAtomFunction :: AtomFunctionName -> [AtomType] -> AtomFunctionBodyType -> AtomFunction
compiledAtomFunction name aType body = AtomFunction { atomFuncName = name,
                                                      atomFuncType = aType,
                                                      atomFuncBody = AtomFunctionBody Nothing body }

--the atom function really should offer some way to return an error
evalAtomFunction :: AtomFunction -> [Atom] -> Atom
evalAtomFunction func args = case atomFuncBody func of
  (AtomFunctionBody _ f) -> f args
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

                                          
                                          

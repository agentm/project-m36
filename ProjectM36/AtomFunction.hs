{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.AtomFunction where
import ProjectM36.Base
import ProjectM36.Error
import qualified ProjectM36.Attribute as A
import qualified Data.HashSet as HS
import qualified Data.Vector as V

foldAtomFuncType :: AtomType -> AtomType -> [AtomType]
foldAtomFuncType foldType returnType = [RelationAtomType (A.attributesFromList [Attribute "_" foldType]), returnType]

atomFunctionForName :: AtomFunctionName -> AtomFunctions -> Either RelationalError AtomFunction
atomFunctionForName funcName funcSet = if HS.null foundFunc then
                                         Left $ NoSuchTupleExprFunctionError funcName
                                        else
                                         Right $ head $ HS.toList foundFunc
  where
    foundFunc = HS.filter (\(AtomFunction name _ _) -> name == funcName) funcSet

--determine if two types are equal or compatible
atomTypeVerify :: AtomType -> AtomType -> Either RelationalError AtomType
atomTypeVerify AnyAtomType x = Right x
atomTypeVerify x AnyAtomType = Right x
atomTypeVerify x@(RelationAtomType attrs1) y@(RelationAtomType attrs2) = do
  _ <- mapM (\(attr1,attr2) -> let name1 = A.attributeName attr1
                                   name2 = A.attributeName attr2 in
                               if notElem "_" [name1, name2] && name1 /= name2 then 
                                 Left $ AtomTypeMismatchError x y
                               else
                                 atomTypeVerify (A.atomType attr1) (A.atomType attr2)) $ V.toList (V.zip attrs1 attrs2)
  return x
atomTypeVerify x y = if x == y then
                       Right x
                     else
                       Left $ AtomTypeMismatchError x y
                                          
                                          
{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.AtomType where
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.DataTypes.Primitive
import qualified Data.Text as T
import ProjectM36.Error
import ProjectM36.Attribute
import ProjectM36.Atom
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Either (isRight)
import Data.Maybe

typesAsRelation :: AtomTypes -> Either RelationalError Relation
typesAsRelation aTypes = mkRelationFromTuples attrs tuples
  where
    attrs = attributesFromList [Attribute "TypeConstructor" textAtomType,
                                Attribute "DataConstructors" dConsType]
    subAttrs = attributesFromList [Attribute "DataConstructor" textAtomType]
    dConsType = RelationAtomType subAttrs
    tuples = map mkTypeConsDescription (M.toList aTypes)
    mkTypeConsDescription (tConsName, (_, AtomConstructor dConsMap)) = RelationTuple attrs (V.fromList [textAtom tConsName, mkDataConsRelation dConsMap])
    mkDataConsRelation dConsMap = case mkRelationFromTuples subAttrs $ map (\(dConsName, dConsTypeNames) -> RelationTuple subAttrs (V.singleton $ textAtom $ T.intercalate " " (dConsName:dConsTypeNames))) (M.toList dConsMap) of
      Left err -> error ("mkRelationFromTuples pooped " ++ show err)
      Right rel -> Atom rel
{-      
-- | Scan all type constructors for the proper data constructor      
dataConstructorTypeNamesForName :: DataConstructorName -> AtomTypes -> Either RelationalError [AtomTypeName]
dataConstructorTypeNamesForName dConsName aTypes = M.foldrWithKey typeFolder defaultErr aTypes
  where
    defaultErr = Left (NoSuchDataConstructorError dConsName)
    typeFolder _ (_, AtomConstructor dConsMap) accum = if isRight accum then
                                                                            accum
                                                                         else 
                                                                           case M.lookup dConsName dConsMap of
      Nothing -> accum
      Just aTypeNames -> Right (dConsName, aTypeNames)
-}
-- fix mostly-duplicated fold code
-- | Scan the atom types and return the resultant ConstructedAtomType or error.
atomAndArgsTypesForDataConstructorName :: DataConstructorName -> AtomTypes -> Either RelationalError (AtomType, [AtomType])
atomAndArgsTypesForDataConstructorName dConsName aTypes = M.foldrWithKey typeFolder defaultErr aTypes
  where
    defaultErr = Left (NoSuchDataConstructorError dConsName)
    typeFolder _ (aType, AtomConstructor dConsMap) accum = if isRight accum then
                                                                            accum
                                                                         else 
                                                                           case M.lookup dConsName dConsMap of
      Nothing -> accum
      Just aArgsTypeNames -> do --convert aTypeNames into AtomTypes
        aArgsTypes <- mapM (\tName -> atomTypeForTypeConstructor tName aTypes) aArgsTypeNames
        Right (aType, aArgsTypes)

atomTypeForTypeConstructor :: TypeConstructorName -> AtomTypes -> Either RelationalError AtomType
atomTypeForTypeConstructor tConsName aTypes = case M.lookup tConsName aTypes of
                                                Nothing -> Left (NoSuchTypeConstructorError tConsName)
                                                Just (aType, _) -> Right aType


-- | Validate that the arguments to the data constructors in the atom constructor are valid type names.
validateAtomConstructor :: AtomConstructor -> AtomTypes -> Maybe RelationalError
validateAtomConstructor (AtomConstructor aConsMap) validAtomTypes = mapM validateDataConstructor (M.elems aConsMap) >>= listToMaybe
  where
    validateDataConstructor :: [AtomTypeName] -> Maybe RelationalError
    validateDataConstructor dConsTypeNames = mapM validateTypeConstructor dConsTypeNames >>= listToMaybe
    validateTypeConstructor :: AtomTypeName -> Maybe RelationalError
    validateTypeConstructor dConsTypeName = case atomTypeForTypeConstructor dConsTypeName validAtomTypes of
      Right _ -> Nothing
      Left err -> Just err
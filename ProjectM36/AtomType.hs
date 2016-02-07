{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.AtomType where
import ProjectM36.Base
import ProjectM36.Relation
import qualified Data.Text as T
import ProjectM36.Error
import ProjectM36.Attribute
import ProjectM36.Atom
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Either (isRight)

typesAsRelation :: AtomTypes -> Either RelationalError Relation
typesAsRelation aTypes = mkRelationFromTuples attrs tuples
  where
    attrs = attributesFromList [Attribute "TypeConstructor" stringAtomType,
                                Attribute "DataConstructors" dConsType]
    subAttrs = attributesFromList [Attribute "DataConstructor" stringAtomType]
    dConsType = RelationAtomType subAttrs
    tuples = map mkTypeConsDescription (M.toList aTypes)
    mkTypeConsDescription (tConsName, (_, AtomConstructor dConsMap)) = RelationTuple attrs (V.fromList [stringAtom tConsName, mkDataConsRelation dConsMap])
    mkDataConsRelation dConsMap = case mkRelationFromTuples subAttrs $ map (\(dConsName, dConsTypeNames) -> RelationTuple subAttrs (V.singleton $ stringAtom $ T.intercalate " " (dConsName:dConsTypeNames))) (M.toList dConsMap) of
      Left err -> error ("mkRelationFromTuples pooped " ++ show err)
      Right rel -> Atom rel
      
-- | Scan all type constructors for the proper data constructor      
dataConstructorForName :: DataConstructorName -> AtomTypes -> Either RelationalError (DataConstructorName, [AtomTypeName])
dataConstructorForName dConsName aTypes = M.foldrWithKey typeFolder defaultErr aTypes
  where
    defaultErr = Left (NoSuchDataConstructor dConsName)
    typeFolder _ (_, AtomConstructor dConsMap) accum = if isRight accum then
                                                                            accum
                                                                         else 
                                                                           case M.lookup dConsName dConsMap of
      Nothing -> accum
      Just aTypeNames -> Right (dConsName, aTypeNames)
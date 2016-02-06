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

atomTypesAsRelation :: AtomTypes -> Either RelationalError Relation
atomTypesAsRelation aTypes = mkRelationFromTuples attrs tuples
  where
    attrs = attributesFromList [Attribute "TypeConstructor" stringAtomType,
                                Attribute "DataConstructors" dConsType]
    subAttrs = attributesFromList [Attribute "DataConstructor" stringAtomType]
    dConsType = RelationAtomType subAttrs
    tuples = map mkTypeConsDescription (M.toList aTypes)
    mkTypeConsDescription (tConsName, (_, AtomConstructor dConsMap)) = RelationTuple attrs (V.fromList [stringAtom tConsName, mkDataConsRelation dConsMap])
    mkDataConsRelation dConsMap = case mkRelationFromTuples subAttrs $ map (\(dConsName, dConsTypeNames) -> RelationTuple subAttrs (V.singleton $ T.intercalate " " dConsName:dConsTypeNames)) $ M.toList dConsMap of
      Left err -> error "mkRelationFromTuples pooped"
      Right rel -> Atom rel
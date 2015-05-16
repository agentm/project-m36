{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.Atom where
import ProjectM36.Base
import ProjectM36.Error

relationForAtom :: Atom -> Either RelationalError Relation
relationForAtom (RelationAtom rel) = Right rel
relationForAtom _ = Left $ AttributeIsNotRelationValuedError ""

atomTypeForAtom :: Atom -> AtomType
atomTypeForAtom (StringAtom _) = StringAtomType
atomTypeForAtom (IntAtom _) = IntAtomType
atomTypeForAtom (RelationAtom (Relation attributes _)) = RelationAtomType attributes

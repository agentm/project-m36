{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.Atom where
import ProjectM36.Base
import ProjectM36.Error
import Data.Typeable
import ProjectM36.ConcreteTypeRep

relationForAtom :: Atom -> Either RelationalError Relation
relationForAtom (Atom atom) = case cast atom of
  Just rel@(Relation _ _) -> Right rel
  Nothing -> Left $ AttributeIsNotRelationValuedError ""

atomTypeForAtom :: Atom -> AtomType
atomTypeForAtom (Atom atom) = case cast atom of
  Just (Relation attrs _) -> RelationAtomType attrs
  Nothing -> AtomType $ CTR (typeOf atom)

atomTypeForProxy :: (Atomable a) => Proxy a -> AtomType
atomTypeForProxy prox = AtomType $ CTR (typeRep prox)

--some convenience functions
stringAtomType :: AtomType
stringAtomType = atomTypeForProxy (Proxy :: Proxy StringType)

intAtomType :: AtomType
intAtomType = atomTypeForProxy (Proxy :: Proxy Int)

boolAtomType :: AtomType
boolAtomType = atomTypeForProxy (Proxy :: Proxy Bool)

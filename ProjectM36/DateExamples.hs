{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.DateExamples where
import ProjectM36.Base
import ProjectM36.Atom
import qualified ProjectM36.Attribute as A
import ProjectM36.Key
import ProjectM36.AtomFunctions.Basic
import ProjectM36.DataTypes.Basic
import ProjectM36.DatabaseContext
import ProjectM36.DataTypes.Primitive
import ProjectM36.Relation
import qualified Data.Map as M
import qualified Data.Set as S

dateExamples :: DatabaseContext
dateExamples = DatabaseContext { inclusionDependencies = dateIncDeps,
                                 relationVariables = M.union (relationVariables basicDatabaseContext) dateRelVars,
                                 notifications = M.empty,
                                 atomFunctions = basicAtomFunctions,
                                 typeConstructorMapping = basicTypeConstructorMapping }
  where -- these must be lower case now that data constructors are in play
    dateRelVars = M.fromList [("s", suppliers),
                              ("p", products),
                              ("sp", supplierProducts)]
    suppliers = suppliersRel
    products = productsRel
    supplierProducts = supplierProductsRel
    dateIncDeps = M.fromList [("s_pkey", simplePKey ["s#"] "s"),
                              ("p_pkey", simplePKey ["p#"] "p"),
                              ("sp_pkey", simplePKey ["s#", "p#"] "sp")
                              ]
    simplePKey attrNames relvarName = inclusionDependencyForKey (AttributeNames $ S.fromList attrNames) (RelationVariable relvarName)

suppliersRel :: Relation
suppliersRel = case mkRelationFromList attrs atomMatrix of
  Left _ -> undefined
  Right rel -> rel
  where
    attrs = A.attributesFromList [Attribute "s#" textAtomType,
                                  Attribute "sname" textAtomType,
                                  Attribute "status" intAtomType,
                                  Attribute "city" textAtomType]
    atomMatrix = [
      [textAtom "S1", textAtom "Smith", intAtom 20, textAtom "London"],
      [textAtom "S2", textAtom "Jones", intAtom 10, textAtom "Paris"],
      [textAtom "S3", textAtom "Blake", intAtom 30, textAtom "Paris"],
      [textAtom "S4", textAtom "Clark", intAtom 20, textAtom "London"],
      [textAtom "S5", textAtom "Adams", intAtom 30, textAtom "Athens"]]

supplierProductsRel :: Relation
supplierProductsRel = case mkRelationFromList attrs matrix of
  Left _ -> undefined
  Right rel -> rel
  where
    attrs = A.attributesFromList [Attribute "s#" textAtomType,
                                  Attribute "p#" textAtomType,
                                  Attribute "qty" intAtomType]
    matrix = [
      [textAtom "S1", textAtom "P1", intAtom 300],
      [textAtom "S1", textAtom "P2", intAtom 200],
      [textAtom "S1", textAtom "P3", intAtom 400],
      [textAtom "S1", textAtom "P4", intAtom 200],
      [textAtom "S1", textAtom "P5", intAtom 100],
      [textAtom "S1", textAtom "P6", intAtom 100],
      [textAtom "S2", textAtom "P1", intAtom 300],
      [textAtom "S2", textAtom "P2", intAtom 400],
      [textAtom "S3", textAtom "P2", intAtom 200],
      [textAtom "S4", textAtom "P2", intAtom 200],
      [textAtom "S4", textAtom "P4", intAtom 300],
      [textAtom "S4", textAtom "P5", intAtom 400]
      ]

productsRel :: Relation
productsRel = case mkRelationFromList attrs matrix of
  Left _ -> undefined
  Right rel -> rel
  where
    attrs = A.attributesFromList [Attribute "p#" textAtomType,
                                  Attribute "pname" textAtomType,
                                  Attribute "color" textAtomType,
                                  Attribute "weight" intAtomType,
                                  Attribute "city" textAtomType]
    matrix = [
      [textAtom "P1", textAtom "Nut", textAtom "Red", intAtom 12, textAtom "London"],
      [textAtom "P2", textAtom "Bolt", textAtom "Green", intAtom 17, textAtom "Paris"],
      [textAtom "P3", textAtom "Screw", textAtom "Blue", intAtom 17, textAtom "Oslo"],
      [textAtom "P4", textAtom "Screw", textAtom "Red", intAtom 14, textAtom "London"],
      [textAtom "P5", textAtom "Cam", textAtom "Blue", intAtom 12, textAtom "Paris"],
      [textAtom "P6", textAtom "Cog", textAtom "Red", intAtom 19, textAtom "London"]
      ]

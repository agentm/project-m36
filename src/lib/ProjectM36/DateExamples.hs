module ProjectM36.DateExamples where
import ProjectM36.Base
import qualified ProjectM36.Attribute as A
import ProjectM36.Key
import ProjectM36.AtomFunctions.Basic
import ProjectM36.DataTypes.Basic
import ProjectM36.DatabaseContext
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
    attrs = A.attributesFromList [Attribute "s#" TextAtomType,
                                  Attribute "sname" TextAtomType,
                                  Attribute "status" IntAtomType,
                                  Attribute "city" TextAtomType]
    atomMatrix = [
      [TextAtom "S1", TextAtom "Smith", IntAtom 20, TextAtom "London"],
      [TextAtom "S2", TextAtom "Jones", IntAtom 10, TextAtom "Paris"],
      [TextAtom "S3", TextAtom "Blake", IntAtom 30, TextAtom "Paris"],
      [TextAtom "S4", TextAtom "Clark", IntAtom 20, TextAtom "London"],
      [TextAtom "S5", TextAtom "Adams", IntAtom 30, TextAtom "Athens"]]

supplierProductsRel :: Relation
supplierProductsRel = case mkRelationFromList attrs matrix of
  Left _ -> undefined
  Right rel -> rel
  where
    attrs = A.attributesFromList [Attribute "s#" TextAtomType,
                                  Attribute "p#" TextAtomType,
                                  Attribute "qty" IntAtomType]
    matrix = [
      [TextAtom "S1", TextAtom "P1", IntAtom 300],
      [TextAtom "S1", TextAtom "P2", IntAtom 200],
      [TextAtom "S1", TextAtom "P3", IntAtom 400],
      [TextAtom "S1", TextAtom "P4", IntAtom 200],
      [TextAtom "S1", TextAtom "P5", IntAtom 100],
      [TextAtom "S1", TextAtom "P6", IntAtom 100],
      [TextAtom "S2", TextAtom "P1", IntAtom 300],
      [TextAtom "S2", TextAtom "P2", IntAtom 400],
      [TextAtom "S3", TextAtom "P2", IntAtom 200],
      [TextAtom "S4", TextAtom "P2", IntAtom 200],
      [TextAtom "S4", TextAtom "P4", IntAtom 300],
      [TextAtom "S4", TextAtom "P5", IntAtom 400]
      ]

productsRel :: Relation
productsRel = case mkRelationFromList attrs matrix of
  Left _ -> undefined
  Right rel -> rel
  where
    attrs = A.attributesFromList [Attribute "p#" TextAtomType,
                                  Attribute "pname" TextAtomType,
                                  Attribute "color" TextAtomType,
                                  Attribute "weight" IntAtomType,
                                  Attribute "city" TextAtomType]
    matrix = [
      [TextAtom "P1", TextAtom "Nut", TextAtom "Red", IntAtom 12, TextAtom "London"],
      [TextAtom "P2", TextAtom "Bolt", TextAtom "Green", IntAtom 17, TextAtom "Paris"],
      [TextAtom "P3", TextAtom "Screw", TextAtom "Blue", IntAtom 17, TextAtom "Oslo"],
      [TextAtom "P4", TextAtom "Screw", TextAtom "Red", IntAtom 14, TextAtom "London"],
      [TextAtom "P5", TextAtom "Cam", TextAtom "Blue", IntAtom 12, TextAtom "Paris"],
      [TextAtom "P6", TextAtom "Cog", TextAtom "Red", IntAtom 19, TextAtom "London"]
      ]

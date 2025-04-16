module ProjectM36.DateExamples where
import ProjectM36.Base
import qualified ProjectM36.Attribute as A
import ProjectM36.Key
import ProjectM36.DatabaseContext.Basic
import ProjectM36.DatabaseContext.Types
import ProjectM36.AtomFunctions.Basic
import ProjectM36.DataTypes.Basic
import ProjectM36.DatabaseContext
import ProjectM36.Relation
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Functor.Identity

dateExamples :: ResolvedDatabaseContext
dateExamples = DatabaseContext {
  inclusionDependencies = Identity dateIncDeps,
  relationVariables = relationVariables basicDatabaseContext <> Identity dateRelVars,
  notifications = Identity M.empty,
  dbcFunctions = Identity mempty,
  atomFunctions = Identity basicAtomFunctions,
  registeredQueries = Identity mempty,
  typeConstructorMapping = Identity basicTypeConstructorMapping
  }
  where -- these must be lower case now that data constructors are in play
    dateRelVars = M.fromList [("s", ExistingRelation suppliers),
                              ("p", ExistingRelation products),
                              ("sp", ExistingRelation supplierProducts)]
    suppliers = suppliersRel
    products = productsRel
    supplierProducts = supplierProductsRel
    dateIncDeps = M.fromList [
      ("s_pkey", simplePKey ["s#"] "s"),
      ("p_pkey", simplePKey ["p#"] "p"),
      ("sp_pkey", simplePKey ["s#", "p#"] "sp"),
      ("s_sp_fk", inclusionDependencyForForeignKey ("sp", ["s#"]) ("s", ["s#"])),
      ("p_sp_fk", inclusionDependencyForForeignKey ("sp", ["p#"]) ("p", ["p#"]))
      ]
    simplePKey attrNames relvarName = inclusionDependencyForKey (AttributeNames $ S.fromList attrNames) (RelationVariable relvarName ())

suppliersRel :: Relation
suppliersRel = case mkRelationFromList attrs atomMatrix of
  Left _ -> undefined
  Right rel -> rel
  where
    attrs = A.attributesFromList [Attribute "s#" TextAtomType,
                                  Attribute "sname" TextAtomType,
                                  Attribute "status" IntegerAtomType,
                                  Attribute "city" TextAtomType]
    atomMatrix = [
      [TextAtom "S1", TextAtom "Smith", IntegerAtom 20, TextAtom "London"],
      [TextAtom "S2", TextAtom "Jones", IntegerAtom 10, TextAtom "Paris"],
      [TextAtom "S3", TextAtom "Blake", IntegerAtom 30, TextAtom "Paris"],
      [TextAtom "S4", TextAtom "Clark", IntegerAtom 20, TextAtom "London"],
      [TextAtom "S5", TextAtom "Adams", IntegerAtom 30, TextAtom "Athens"]]

supplierProductsRel :: Relation
supplierProductsRel = case mkRelationFromList attrs matrix of
  Left _ -> undefined
  Right rel -> rel
  where
    attrs = A.attributesFromList [Attribute "s#" TextAtomType,
                                  Attribute "p#" TextAtomType,
                                  Attribute "qty" IntegerAtomType]
    matrix = [
      [TextAtom "S1", TextAtom "P1", IntegerAtom 300],
      [TextAtom "S1", TextAtom "P2", IntegerAtom 200],
      [TextAtom "S1", TextAtom "P3", IntegerAtom 400],
      [TextAtom "S1", TextAtom "P4", IntegerAtom 200],
      [TextAtom "S1", TextAtom "P5", IntegerAtom 100],
      [TextAtom "S1", TextAtom "P6", IntegerAtom 100],
      [TextAtom "S2", TextAtom "P1", IntegerAtom 300],
      [TextAtom "S2", TextAtom "P2", IntegerAtom 400],
      [TextAtom "S3", TextAtom "P2", IntegerAtom 200],
      [TextAtom "S4", TextAtom "P2", IntegerAtom 200],
      [TextAtom "S4", TextAtom "P4", IntegerAtom 300],
      [TextAtom "S4", TextAtom "P5", IntegerAtom 400]
      ]

productsRel :: Relation
productsRel = case mkRelationFromList attrs matrix of
  Left _ -> undefined
  Right rel -> rel
  where
    attrs = A.attributesFromList [Attribute "p#" TextAtomType,
                                  Attribute "pname" TextAtomType,
                                  Attribute "color" TextAtomType,
                                  Attribute "weight" IntegerAtomType,
                                  Attribute "city" TextAtomType]
    matrix = [
      [TextAtom "P1", TextAtom "Nut", TextAtom "Red", IntegerAtom 12, TextAtom "London"],
      [TextAtom "P2", TextAtom "Bolt", TextAtom "Green", IntegerAtom 17, TextAtom "Paris"],
      [TextAtom "P3", TextAtom "Screw", TextAtom "Blue", IntegerAtom 17, TextAtom "Oslo"],
      [TextAtom "P4", TextAtom "Screw", TextAtom "Red", IntegerAtom 14, TextAtom "London"],
      [TextAtom "P5", TextAtom "Cam", TextAtom "Blue", IntegerAtom 12, TextAtom "Paris"],
      [TextAtom "P6", TextAtom "Cog", TextAtom "Red", IntegerAtom 19, TextAtom "London"]
      ]

import Test.HUnit
import ProjectM36.Base
import ProjectM36.IsomorphicSchema
import ProjectM36.Relation
import ProjectM36.RelationalExpression
import qualified ProjectM36.DatabaseContext as DBC
import qualified ProjectM36.Attribute as A
import System.Exit
import qualified Data.Map as M
import Control.Monad.State

testList :: Test
testList = TestList [testIsoRename, testIsoRestrict, testIsoUnion]

main :: IO ()           
main = do 
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

assertEither :: (Show a) => Either a b -> IO b
assertEither x = case x of
  Left err -> assertFailure (show err) >> undefined
  Right val -> pure val
  
-- create some potential schemas which should not be accepted  
testSchemaValidation :: Test
testSchemaValidation = TestCase $ do  
  undefined
  
testIsoRename :: Test
testIsoRename = TestCase $ do
  -- create a schema with two relvars and rename one while the other remains the same in the isomorphic schema
  let schemaA = DBC.empty { 
        relationVariables = M.fromList [("employee", relationTrue), 
                                        ("department", relationFalse)]
        }
      isomorphsAtoB = [IsoRename "emp" "employee", 
                       IsoRename "department" "department"]
      unionExpr = Union (RelationVariable "emp" ()) (RelationVariable "department" ())      
  relExpr <- assertEither (applyRelationalExprSchemaIsomorphs isomorphsAtoB unionExpr)
  let relResult = evalState (evalRelationalExpr relExpr) schemaA                             
  assertEqual "employee relation morph" (Right relationTrue) relResult
  
testIsoRestrict :: Test
testIsoRestrict = TestCase $ do
  -- create a emp relation which is restricted into two boss, nonboss rel vars
  -- the virtual schema has an employee
  emprel <- assertEither $ mkRelationFromList (A.attributesFromList [Attribute "name" TextAtomType,
                                                                    Attribute "boss" TextAtomType])
            [[TextAtom "Steve", TextAtom ""],
             [TextAtom "Cindy", TextAtom "Steve"],
             [TextAtom "Sam", TextAtom "Steve"]]
  let predicate = AttributeEqualityPredicate "boss" (NakedAtomExpr (TextAtom ""))
  bossRel <- assertEither $ evalState (evalRelationalExpr (Restrict predicate (ExistingRelation emprel))) DBC.empty
  nonBossRel <- assertEither $ evalState (evalRelationalExpr (Restrict (NotPredicate predicate) (ExistingRelation emprel))) DBC.empty
            
  let schemaA = DBC.empty {
        relationVariables = M.fromList [("nonboss", nonBossRel),
                                        ("boss", bossRel)]
        }

      isomorphsAtoB = [IsoRestrict "employee" predicate ("boss", "nonboss")]
      bossq = RelationVariable "boss" ()
      nonbossq = RelationVariable "nonboss" ()
      employeeq = RelationVariable "employee" ()
      unionq = Union bossq nonbossq
  empExpr <- assertEither (applyRelationalExprSchemaIsomorphs isomorphsAtoB employeeq)
  let empResult = evalState (evalRelationalExpr empExpr) schemaA
      unionResult = evalState (evalRelationalExpr unionq) schemaA
  assertEqual "boss/nonboss isorestrict" unionResult empResult
  -- TODO: add databaseContextExprs
  
testIsoUnion :: Test  
testIsoUnion = TestCase $ do
  --create motors relation which is split into low-power (<50 horsepower) and high-power (>=50 horsepower) motors
  --the schema is contains the split relvars
  motorsRel <- assertEither $ mkRelationFromList (A.attributesFromList [Attribute "name" TextAtomType,
                                                                        Attribute "power" IntAtomType]) 
               [[TextAtom "Puny", IntAtom 10],
                [TextAtom "Scooter", IntAtom 49],
                [TextAtom "Auto", IntAtom 200],
                [TextAtom "Tractor", IntAtom 500]]
  let baseSchema = DBC.basicDatabaseContext {
        relationVariables = M.singleton "motor" motorsRel
        }
      splitPredicate = AtomExprPredicate (FunctionAtomExpr "lt" [AttributeAtomExpr "power", NakedAtomExpr (IntAtom 50)] ())
      splitIsomorphs = [IsoUnion ("lowpower", "highpower") splitPredicate "motor",
                        IsoRename "true" "true",
                        IsoRename "false" "false"]
      lowmotors = evalState (evalRelationalExpr (Restrict splitPredicate (RelationVariable "motor" ()))) baseSchema
      highmotors = evalState (evalRelationalExpr (Restrict (NotPredicate splitPredicate) (RelationVariable "motor" ()))) baseSchema
      relResult expr = evalState (evalRelationalExpr expr) baseSchema
  lowpowerExpr <- assertEither (processRelationalExprInSchema (Schema splitIsomorphs) (RelationVariable "lowpower" ()))
  lowpowerRel <- assertEither (relResult lowpowerExpr)
  highpowerExpr <- assertEither (processRelationalExprInSchema (Schema splitIsomorphs) (RelationVariable "highpower" ()))
  highpowerRel <- assertEither (relResult highpowerExpr)
  assertEqual "lowpower relation difference" (Right lowpowerRel) lowmotors
  assertEqual "highpower relation difference" (Right highpowerRel) highmotors
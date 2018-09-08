import Test.HUnit
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.IsomorphicSchema
import ProjectM36.Relation
import ProjectM36.RelationalExpression
import qualified ProjectM36.DatabaseContext as DBC
import qualified ProjectM36.Attribute as A
import System.Exit
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Trans.Reader

testList :: Test
testList = TestList [testIsoRename, testIsoRestrict, testIsoUnion, testSchemaValidation]

main :: IO ()           
main = do 
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

assertEither :: (Show a) => Either a b -> IO b
assertEither x = case x of
  Left err -> assertFailure (show err) >> undefined
  Right val -> pure val

{-  
assertMaybe :: Maybe a -> String -> IO a  
assertMaybe x msg = case x of
  Nothing -> assertFailure msg >> undefined
  Just x' -> pure x'
-}
  
-- create some potential schemas which should not be accepted  
testSchemaValidation :: Test
testSchemaValidation = TestCase $ do  
  let potentialSchema = DBC.basicDatabaseContext {
        relationVariables = M.singleton "anotherRel" relationTrue
        }
  -- missing relvars failure
      morphs = [IsoRename "true" "true", IsoRename "false" "false"]
      missingRelVarError = Just (RelVarReferencesMissing (S.singleton "anotherRel"))
  assertEqual "missing relvar validation" missingRelVarError (validateSchema (Schema morphs) potentialSchema)
  -- duplicate relvar mention
  let morphs' = [IsoRename "true" "true", IsoRename "false" "true", IsoRename "true" "anotherRel"]
      duplicateRelVarError = Just (RelVarOutReferencedMoreThanOnce "true")
  assertEqual "duplicate relvars in morphs" duplicateRelVarError (validateSchema (Schema morphs') potentialSchema)
  
testIsoRename :: Test
testIsoRename = TestCase $ do
  -- create a schema with two relvars and rename one while the other remains the same in the isomorphic schema
  let schemaA = mkRelationalExprState DBC.empty { 
        relationVariables = M.fromList [("employee", relationTrue), 
                                        ("department", relationFalse)]
        }
      isomorphsAtoB = [IsoRename "emp" "employee", 
                       IsoRename "department" "department"]
      unionExpr = Union (RelationVariable "emp" ()) (RelationVariable "department" ())      
  relExpr <- assertEither (applyRelationalExprSchemaIsomorphs isomorphsAtoB unionExpr)
  let relResult = runReader (evalRelationalExpr relExpr) schemaA                             
  assertEqual "employee relation morph" (Right relationTrue) relResult
  
testIsoRestrict :: Test
testIsoRestrict = TestCase $ do
  -- create a emp relation which is restricted into two boss, nonboss rel vars
  -- the virtual schema has an employee
  let empattrs = A.attributesFromList [Attribute "name" TextAtomType,
                                        Attribute "boss" TextAtomType]
  emprel <- assertEither $ mkRelationFromList empattrs
            [[TextAtom "Steve", TextAtom ""],
             [TextAtom "Cindy", TextAtom "Steve"],
             [TextAtom "Sam", TextAtom "Steve"]]
  let predicate = AttributeEqualityPredicate "boss" (NakedAtomExpr (TextAtom ""))
  bossRel <- assertEither $ runReader (evalRelationalExpr (Restrict predicate (ExistingRelation emprel))) (mkRelationalExprState DBC.empty)
  nonBossRel <- assertEither $ runReader (evalRelationalExpr (Restrict (NotPredicate predicate) (ExistingRelation emprel))) (mkRelationalExprState DBC.empty)
            
  let schemaA = mkRelationalExprState DBC.empty {
        relationVariables = M.fromList [("nonboss", nonBossRel),
                                        ("boss", bossRel)]
        }

      isomorphsAtoB = [IsoRestrict "employee" predicate ("boss", "nonboss")]
      bossq = RelationVariable "boss" ()
      nonbossq = RelationVariable "nonboss" ()
      employeeq = RelationVariable "employee" ()
      unionq = Union bossq nonbossq
  empExpr <- assertEither (applyRelationalExprSchemaIsomorphs isomorphsAtoB employeeq)
  let empResult = runReader (evalRelationalExpr empExpr) schemaA
      unionResult = runReader (evalRelationalExpr unionq) schemaA
  assertEqual "boss/nonboss isorestrict" unionResult empResult
  --execute database context expr
  bobRel <- assertEither (mkRelationFromList empattrs [[TextAtom "Bob", TextAtom ""]])
  let schemaBInsertExpr = Insert "employee" (ExistingRelation bobRel)
  schemaBInsertExpr' <- assertEither (processDatabaseContextExprInSchema (Schema isomorphsAtoB) schemaBInsertExpr)
  let (postInsertContext,_,_) = execState (evalDatabaseContextExpr schemaBInsertExpr') (freshDatabaseState (stateElemsContext schemaA))
      expectedRel = runReader (evalRelationalExpr (Union (RelationVariable "boss" ()) (RelationVariable "nonboss" ()))) (mkRelationalExprState postInsertContext)
  --execute the expression against the schema and compare against the base context
  processedExpr <- assertEither (processRelationalExprInSchema (Schema isomorphsAtoB) (RelationVariable "employee" ()))
  let processedRel = runReader (evalRelationalExpr processedExpr) (mkRelationalExprState postInsertContext)
  assertEqual "insert bob boss" expectedRel processedRel
  
testIsoUnion :: Test  
testIsoUnion = TestCase $ do
  --create motors relation which is split into low-power (<50 horsepower) and high-power (>=50 horsepower) motors
  --the schema is contains the split relvars
  motorsRel <- assertEither $ mkRelationFromList (A.attributesFromList [Attribute "name" TextAtomType,
                                                                        Attribute "power" IntegerAtomType]) 
               [[TextAtom "Puny", IntegerAtom 10],
                [TextAtom "Scooter", IntegerAtom 49],
                [TextAtom "Auto", IntegerAtom 200],
                [TextAtom "Tractor", IntegerAtom 500]]
  let baseSchema = mkRelationalExprState DBC.basicDatabaseContext {
        relationVariables = M.singleton "motor" motorsRel
        }
      splitPredicate = AtomExprPredicate (FunctionAtomExpr "lt" [AttributeAtomExpr "power", NakedAtomExpr (IntegerAtom 50)] ())
      splitIsomorphs = [IsoUnion ("lowpower", "highpower") splitPredicate "motor",
                        IsoRename "true" "true",
                        IsoRename "false" "false"]
      lowmotors = runReader (evalRelationalExpr (Restrict splitPredicate (RelationVariable "motor" ()))) baseSchema
      highmotors = runReader (evalRelationalExpr (Restrict (NotPredicate splitPredicate) (RelationVariable "motor" ()))) baseSchema
      relResult expr = runReader (evalRelationalExpr expr) baseSchema
  lowpowerExpr <- assertEither (processRelationalExprInSchema (Schema splitIsomorphs) (RelationVariable "lowpower" ()))
  lowpowerRel <- assertEither (relResult lowpowerExpr)
  highpowerExpr <- assertEither (processRelationalExprInSchema (Schema splitIsomorphs) (RelationVariable "highpower" ()))
  highpowerRel <- assertEither (relResult highpowerExpr)
  assertEqual "lowpower relation difference" (Right lowpowerRel) lowmotors
  assertEqual "highpower relation difference" (Right highpowerRel) highmotors
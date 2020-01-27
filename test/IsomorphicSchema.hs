import Test.HUnit
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.IsomorphicSchema
import ProjectM36.Relation
import ProjectM36.RelationalExpression
import ProjectM36.TransactionGraph
import ProjectM36.StaticOptimizer
import qualified ProjectM36.DatabaseContext as DBC
import qualified ProjectM36.Attribute as A
import System.Exit
import qualified Data.Map as M
import qualified Data.Set as S

testList :: Test
testList = TestList [testIsoRename, testIsoRestrict]--, testIsoUnion, testSchemaValidation]

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
        relationVariables = M.singleton "anotherRel" (ExistingRelation relationTrue)
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
  let ctx = DBC.empty { 
        relationVariables = M.fromList [("employee", ExistingRelation relationTrue), 
                                        ("department", ExistingRelation relationFalse)]
        }
  (graph, _) <- freshTransactionGraph ctx
  let isomorphsAtoB = [IsoRename "emp" "employee", 
                       IsoRename "department" "department"]
      unionExpr = Union (RelationVariable "emp" ()) (RelationVariable "department" ())
      env = mkRelationalExprEnv ctx graph
  relExpr <- assertEither (applyRelationalExprSchemaIsomorphs isomorphsAtoB unionExpr)
  let relResult = optimizeAndEvalRelationalExpr env relExpr
  assertEqual "employee relation morph" (Right relationTrue) relResult
  
testIsoRestrict :: Test
testIsoRestrict = TestCase $ do
  -- create a emp relation which is restricted into two boss, nonboss rel vars
  -- the virtual schema has an employee
  let empattrs = A.attributesFromList [Attribute "name" TextAtomType,
                                        Attribute "boss" TextAtomType]
  (graph, transId) <- freshTransactionGraph DBC.empty                 
  emprel <- assertEither $ mkRelationFromList empattrs
            [[TextAtom "Steve", TextAtom ""],
             [TextAtom "Cindy", TextAtom "Steve"],
             [TextAtom "Sam", TextAtom "Steve"]]
  let predicate = AttributeEqualityPredicate "boss" (NakedAtomExpr (TextAtom ""))
      reenv = mkRelationalExprEnv DBC.empty graph
  bossRel <- assertEither $ runRelationalExprM reenv (evalRelationalExpr (Restrict predicate (ExistingRelation emprel)))
  print "spam1"
  nonBossRel <- assertEither $ runRelationalExprM reenv (evalRelationalExpr (Restrict (NotPredicate predicate) (ExistingRelation emprel)))
            
  let schemaA = mkRelationalExprEnv baseContext graph
      baseContext = DBC.empty {
        relationVariables = M.fromList [("nonboss", ExistingRelation nonBossRel),
                                        ("boss", ExistingRelation bossRel)]
        }
      isomorphsAtoB = [IsoRestrict "employee" predicate ("boss", "nonboss")]
      bossq = RelationVariable "boss" ()
      nonbossq = RelationVariable "nonboss" ()
      employeeq = RelationVariable "employee" ()
      unionq = Union bossq nonbossq
  empExpr <- assertEither (applyRelationalExprSchemaIsomorphs isomorphsAtoB employeeq)
  let empResult = evalRelExpr (evalRelationalExpr empExpr)
      unionResult = evalRelExpr (evalRelationalExpr unionq)
      evalRelExpr = runRelationalExprM schemaA
  print "spam2"
  assertEqual "boss/nonboss isorestrict" unionResult empResult
  --execute database context expr
  bobRel <- assertEither (mkRelationFromList empattrs [[TextAtom "Bob", TextAtom ""]])
  let schemaBInsertExpr = Insert "employee" (ExistingRelation bobRel)
  schemaBInsertExpr' <- assertEither (processDatabaseContextExprInSchema (Schema isomorphsAtoB) schemaBInsertExpr)
  let Right dbcState = evalDBCExpr schemaBInsertExpr'
      postInsertContext = dbc_context dbcState
      evalDBCExpr expr' = runDatabaseContextEvalMonad baseContext dbcenv (optimizeAndEvalDatabaseContextExpr False expr')
      dbcenv = mkDatabaseContextEvalEnv transId graph
      expectedRel = runRelationalExprM postInsertEnv (evalRelationalExpr (Union (RelationVariable "boss" ()) (RelationVariable "nonboss" ())))
      postInsertEnv = mkRelationalExprEnv postInsertContext graph
  --execute the expression against the schema and compare against the base context
  processedExpr <- assertEither (processRelationalExprInSchema (Schema isomorphsAtoB) (RelationVariable "employee" ()))
  let processedRel = runRelationalExprM postInsertEnv (evalRelationalExpr processedExpr)
  assertEqual "insert bob boss" expectedRel processedRel
  
testIsoUnion :: Test  
testIsoUnion = TestCase $ do
  --create motors relation which is split into low-power (<50 horsepower) and high-power (>=50 horsepower) motors
  --the schema is contains the split relvars
  (graph, _) <- freshTransactionGraph DBC.empty
  motorsRel <- assertEither $ mkRelationFromList (A.attributesFromList [Attribute "name" TextAtomType,
                                                                        Attribute "power" IntegerAtomType]) 
               [[TextAtom "Puny", IntegerAtom 10],
                [TextAtom "Scooter", IntegerAtom 49],
                [TextAtom "Auto", IntegerAtom 200],
                [TextAtom "Tractor", IntegerAtom 500]]
  let env = mkRelationalExprEnv DBC.basicDatabaseContext {
        relationVariables = M.singleton "motor" (ExistingRelation motorsRel)
        } graph
      splitPredicate = AtomExprPredicate (FunctionAtomExpr "lt" [AttributeAtomExpr "power", NakedAtomExpr (IntegerAtom 50)] ())
      splitIsomorphs = [IsoUnion ("lowpower", "highpower") splitPredicate "motor",
                        IsoRename "true" "true",
                        IsoRename "false" "false"]
      lowmotors = runRelationalExprM env (evalRelationalExpr (Restrict splitPredicate (RelationVariable "motor" ())))
      highmotors = runRelationalExprM env (evalRelationalExpr (Restrict (NotPredicate splitPredicate) (RelationVariable "motor" ())))
      relResult expr = runRelationalExprM env (evalRelationalExpr expr)
  lowpowerExpr <- assertEither (processRelationalExprInSchema (Schema splitIsomorphs) (RelationVariable "lowpower" ()))
  lowpowerRel <- assertEither (relResult lowpowerExpr)
  highpowerExpr <- assertEither (processRelationalExprInSchema (Schema splitIsomorphs) (RelationVariable "highpower" ()))
  highpowerRel <- assertEither (relResult highpowerExpr)
  assertEqual "lowpower relation difference" (Right lowpowerRel) lowmotors
  assertEqual "highpower relation difference" (Right highpowerRel) highmotors

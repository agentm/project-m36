import TutorialD.Interpreter.DatabaseContextExpr
import TutorialD.Interpreter.TestBase
import TutorialD.Interpreter
import TutorialD.Interpreter.Base
import Test.HUnit
import ProjectM36.Relation as R
import ProjectM36.Tuple
import ProjectM36.TupleSet as TS
import ProjectM36.Error
import ProjectM36.DatabaseContext.Types hiding (DatabaseContextFunctionUtils(..))
import ProjectM36.DatabaseContext
import ProjectM36.AtomFunctions.Primitive
import ProjectM36.RelationalExpression
import ProjectM36.DataTypes.Either
import ProjectM36.DataTypes.Interval
import ProjectM36.DataTypes.NonEmptyList
import ProjectM36.DataTypes.List
import ProjectM36.DateExamples
import ProjectM36.Base hiding (Finite)
import ProjectM36.TransactionGraph
import ProjectM36.TransactionGraph.Types
import ProjectM36.Transaction.Types
import ProjectM36.Client
import ProjectM36.HashSecurely
import ProjectM36.Interpreter as I
import qualified ProjectM36.DisconnectedTransaction as Discon
import qualified ProjectM36.AttributeNames as AN
import qualified ProjectM36.Session as Sess
import qualified ProjectM36.Attribute as A
import qualified Data.Map as M
import System.Exit
import Data.Maybe (isJust)
import qualified Data.Vector as V
import Data.Text.Encoding as TE
import Control.Concurrent
import qualified Data.Set as S
import Data.Text (unpack, Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX hiding (getCurrentTime)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Calendar (fromGregorian)
import Data.Either
import Data.Scientific
import Data.ByteString.Base64 as B64

main :: IO ()
main = do
  tcounts <- runTestTT (TestList tests)
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess
  where
    tests = [
      simpleRelTests,
      dateExampleRelTests,
      transactionGraphBasicTest, 
      transactionGraphAddCommitTest, 
      transactionRollbackTest, 
      transactionJumpTest, 
      transactionBranchTest, 
      simpleJoinTest, 
      testTypeConstructors, 
      testMergeTransactions, 
      testComments,
      testTransGraphRelationalExpr, 
      failJoinTest, 
      testMultiAttributeRename, 
      testSchemaExpr, 
      testContextTuples,
      testFunctionalDependencies, 
      testEmptyCommits,
      testIntervalAtom,
      testListConstructedAtom,
      testTypeChecker,
      testRestrictionPredicateExprs,
      testRelationalAttributeNames,
      testSemijoin,
      testAntijoin,
      testRelationAttributeDefinition,
      testAssignWithTypeVar,
      testDefineWithTypeVar,
      testIntervalType,
      testArbitraryRelation,
      testNonEmptyListType,
      testUnresolvedAtomTypes,
      testWithClause,
      testAtomFunctionArgumentMismatch,
      testInvalidDataConstructor,
      testBasicList,
      testRelationDeclarationMismatch,
      testInvalidTuples,
      testSelfReferencingUncommittedContext,
      testUnionAndIntersectionAttributeExprs,
      testUndefineConstraints,
      testQuotedRelVarNames,
      testScientific,
      testExtendProcessorTuplePushdown,
      testDDLHash,
      testShowDDL,
      testRegisteredQueries,
      testCrossJoin,
      testIfThenExpr,
      testSubrelationAttributeAtomExpr,
      testComplexTypeVarResolution,
      testNotifications,
      testDBCFunctionAccessControl,
      testAttributeTypeHintsFromExistingRelVar
      ]

simpleRelTests :: Test
simpleRelTests = TestCase $ do
  (graph, transId) <- freshTransactionGraph' dateExamples
  mapM_ (\(tutd, expected) ->
            assertTutdEqual basicDatabaseContext transId graph expected tutd) testTups
  where
    testTups = [("x:=true", Right relationTrue),
                      ("x:=false", Right relationFalse),
                      ("x:=true union false", Right relationTrue),
                      ("x:=true minus false", Right relationTrue),
                      ("x:=false minus true", Right relationFalse),                      
                      ("x:=true; x:=false", Right relationFalse),
                      ("x:=relation{a Integer}{}", mkRelation simpleAAttributes TS.empty),
                      ("x:=relation{c Integer}{} rename {c as d}", mkRelation simpleBAttributes TS.empty),
                      ("y:=relation{b Integer, c Integer}{}; x:=y{c}", mkRelation simpleProjectionAttributes TS.empty),
                      ("x:=relation{tuple{a \"spam\", b 5}}", mkRelation simpleCAttributes $ RelationTupleSet [RelationTuple simpleCAttributes (V.fromList [TextAtom "spam", IntegerAtom 5])]),
                      ("constraint failc true in false; x:=true", Left $ InclusionDependencyCheckError "failc" Nothing),
                      ("x:=y; x:=true", Left $ RelVarNotDefinedError "y"),
                      ("x:=relation{}{}", Right relationFalse),
                      ("x:=relation{tuple{}}", Right relationTrue),
                      ("x:=true where true", Right relationTrue),
                      ("x:=true where false", Right relationFalse),
                      ("x:=true where true or false", Right relationTrue),
                      ("x:=true where false or false", Right relationFalse),
                      ("x:=true where true and false", Right relationFalse),
                      ("x:=true where false and true", Right relationFalse),                      
                      ("x:=true where True and False", Right relationFalse),
                      ("x:=true where true and true", Right relationTrue),
                      ("x:=true where true and True", Right relationTrue),
                      ("x:=true where True and true", Right relationTrue), -- flip these around to catch parser bugs
                      ("x:=true=true", Right relationTrue),
                      ("x:=true=false", Right relationFalse),
                      ("x:=true; undefine x", Left (RelVarNotDefinedError "x")),
                      ("x:=relation {b Integer, a Text}{}; insert x relation{tuple{b -5, a \"spam\"}}", mkRelationFromTuples simpleCAttributes [RelationTuple simpleCAttributes $ V.fromList [TextAtom "spam", IntegerAtom (-5)]]),
                      -- test nested relation constructor
                      ("x:=relation{tuple{a 5, b relation{tuple{a 6}}}}", mkRelation nestedRelationAttributes $ RelationTupleSet [RelationTuple nestedRelationAttributes (V.fromList [IntegerAtom 5, RelationAtom (Relation simpleAAttributes $ RelationTupleSet [RelationTuple simpleAAttributes $ V.fromList [IntegerAtom 6]])])]),
                      ("x:=relation{tuple{b 5,a \"spam\"},tuple{b 6,a \"sam\"}}; delete x where b=6", mkRelation simpleCAttributes $ RelationTupleSet [RelationTuple simpleCAttributes (V.fromList [TextAtom "spam", IntegerAtom 5])]),
                      ("x:=relation{tuple{a 5}} : {b:=@a}", mkRelation simpleDAttributes $ RelationTupleSet [RelationTuple simpleDAttributes (V.fromList [IntegerAtom 5, IntegerAtom 5])]),
                      ("x:=relation{tuple{a 5}} : {b:=6}", mkRelationFromTuples simpleDAttributes [RelationTuple simpleDAttributes (V.fromList [IntegerAtom 5, IntegerAtom 6])]),
                      ("x:=relation{tuple{a 5}} : {b:=add(@a,5)}", mkRelationFromTuples simpleDAttributes [RelationTuple simpleDAttributes (V.fromList [IntegerAtom 5, IntegerAtom 10])]),
                      ("x:=relation{tuple{a 5}} : {b:=add(@a,\"spam\")}", Left (AtomFunctionTypeError "add" 2 IntegerAtomType TextAtomType)),
                      ("x:=relation{tuple{a 5}} : {b:=add(add(@a,2),5)}", mkRelationFromTuples simpleDAttributes [RelationTuple simpleDAttributes (V.fromList [IntegerAtom 5, IntegerAtom 12])]),
                      ("x:=false:{a:=1,b:=2}", mkRelationFromTuples simpleDAttributes [])
                     ]
    simpleAAttributes = A.attributesFromList [Attribute "a" IntegerAtomType]
    simpleBAttributes = A.attributesFromList [Attribute "d" IntegerAtomType]
    simpleCAttributes = A.attributesFromList [Attribute "a" TextAtomType, Attribute "b" IntegerAtomType]
    simpleDAttributes = A.attributesFromList [Attribute "a" IntegerAtomType, Attribute "b" IntegerAtomType]
    simpleProjectionAttributes = A.attributesFromList [Attribute "c" IntegerAtomType]
    nestedRelationAttributes = A.attributesFromList [Attribute "a" IntegerAtomType, Attribute "b" (RelationAtomType $ A.attributesFromList [Attribute "a" IntegerAtomType])]
  
dateExampleRelTests :: Test
dateExampleRelTests = TestCase $ do
  (graph, transId) <- freshTransactionGraph' dateExamples
  mapM_ (\(tutd, expected) ->
            assertTutdEqual dateExamples transId graph expected tutd) testTups
  where
    simpleEitherIntTextAttributes = A.attributesFromList [Attribute "a" (eitherAtomType IntegerAtomType TextAtomType)]
    maybeIntegerAtomType = ConstructedAtomType "Maybe" (M.singleton "a" IntegerAtomType)
    maybeTextAtomType = ConstructedAtomType "Maybe" (M.singleton "a" TextAtomType)
    simpleMaybeTextAttributes = A.attributesFromList [Attribute "a" maybeTextAtomType]
    simpleMaybeIntAttributes = A.attributesFromList [Attribute "a" maybeIntegerAtomType]
    byteStringAttributes = A.attributesFromList [Attribute "y" ByteStringAtomType]    
    minMaxAttrs = A.attributesFromList [Attribute "s#" TextAtomType, Attribute "z" IntegerAtomType]
    groupCountAttrs = A.attributesFromList [Attribute "z" IntegerAtomType]
    updateParisPlus10 = relMap (\tuple -> do
                                   statusAtom <- atomForAttributeName "status" tuple
                                   cityAtom <- atomForAttributeName "city" tuple
                                   if cityAtom == TextAtom "Paris" then
                                     Right $ updateTupleWithAtoms (M.singleton "status" (IntegerAtom (castInteger statusAtom + 10))) tuple
                                     else Right tuple) suppliersRel
    extendTestAttributes = A.attributesFromList [Attribute "a" IntegerAtomType, Attribute "b" $ RelationAtomType (R.attributes suppliersRel)]
    testTups = [("x:=s where true", Right suppliersRel),
                           ("x:=s where city = \"London\"", restrict (\tuple _ -> pure $ atomForAttributeName "city" tuple == Right (TextAtom "London")) suppliersRel),
                           ("x:=s where false", Right $ Relation (R.attributes suppliersRel) TS.empty),
                           ("x:=p where color=\"Blue\" and city=\"Paris\"", mkRelationFromList (R.attributes productsRel) [[TextAtom "P5", TextAtom "Cam", TextAtom "Blue", IntegerAtom 12, TextAtom "Paris"]]),
                           ("a:=s; update a (status:=50); x:=a{status}", mkRelation (A.attributesFromList [Attribute "status" IntegerAtomType]) (RelationTupleSet [mkRelationTuple (A.attributesFromList [Attribute "status" IntegerAtomType]) (V.fromList [IntegerAtom 50])])),
                           ("x:=s minus (s where status=20)", mkRelationFromList (R.attributes suppliersRel) [[TextAtom "S2", TextAtom "Jones", IntegerAtom 10, TextAtom "Paris"], [TextAtom "S3", TextAtom "Blake", IntegerAtom 30, TextAtom "Paris"], [TextAtom "S5", TextAtom "Adams", IntegerAtom 30, TextAtom "Athens"]]),
                           --atom function tests
                           ("x:=((s : {status2 := add(10,@status)}) where status2=add(10,@status)){city,s#,sname,status}", Right suppliersRel),
                           ("x:=relation{tuple{a 5}} : {b:=s}", mkRelation extendTestAttributes (RelationTupleSet [mkRelationTuple extendTestAttributes (V.fromList [IntegerAtom 5, RelationAtom suppliersRel])])),
                           ("x:=s; update x where sname=\"Blake\" (city:=\"Boston\")", relMap (\tuple -> if atomForAttributeName "sname" tuple == Right (TextAtom "Blake") then Right $ updateTupleWithAtoms (M.singleton "city" (TextAtom "Boston")) tuple else Right tuple) suppliersRel),
                           ("x:=s; update x where city=\"Paris\" (status:=add(@status,10))", updateParisPlus10),
                           --relatom function tests
                           ("x:=((s group ({city} as y)):{z:=count(@y)}){z}", mkRelation groupCountAttrs (RelationTupleSet [mkRelationTuple groupCountAttrs (V.singleton $ IntegerAtom 1)])),
                           ("x:=(sp group ({s#} as y)) ungroup y", Right supplierProductsRel),
                           ("x:=((sp{s#,qty}) group ({qty} as x):{z:=max(@x.qty)}){s#,z}", mkRelationFromList minMaxAttrs (map (\(s,i) -> [TextAtom s,IntegerAtom i]) [("S1", 400), ("S2", 400), ("S3", 200), ("S4", 400)])),
                           ("x:=((sp{s#,qty}) group ({qty} as x):{z:=min(@x.qty)}){s#,z}", mkRelationFromList minMaxAttrs (map (\(s,i) -> [TextAtom s,IntegerAtom i]) [("S1", 100), ("S2", 300), ("S3", 200), ("S4", 200)])),
                           ("x:=((sp{s#,qty}) group ({qty} as x):{z:=sum(@x.qty)}){s#,z}", mkRelationFromList minMaxAttrs (map (\(s,i) -> [TextAtom s,IntegerAtom i]) [("S1", 1000), ("S2", 700), ("S3", 200), ("S4", 900)])),
                           --boolean function restriction
                           ("x:=s where lt(@status,20)", mkRelationFromList (R.attributes suppliersRel) [[TextAtom "S2", TextAtom "Jones", IntegerAtom 10, TextAtom "Paris"]]),
                           ("x:=s where gt(@status,20)", mkRelationFromList (R.attributes suppliersRel) [[TextAtom "S3", TextAtom "Blake", IntegerAtom 30, TextAtom "Paris"],
                                                                                                       [TextAtom "S5", TextAtom "Adams", IntegerAtom 30, TextAtom "Athens"]]),
                           ("x:=s where sum(@status)", Left $ AtomFunctionTypeError "sum" 1 (SubrelationFoldAtomType IntegerAtomType) IntegerAtomType),
                           ("x:=s where not(gte(@status,20))", mkRelationFromList (R.attributes suppliersRel) [[TextAtom "S2", TextAtom "Jones", IntegerAtom 10, TextAtom "Paris"]]),
                           --test "all but" attribute inversion syntax
                           ("x:=s{all but s#} = s{city,sname,status}", Right relationTrue),
                           --test key syntax
                           ("x:=s; key testconstraint {s#,city} x; insert x relation{tuple{city \"London\", s# \"S1\", sname \"gonk\", status 50}}", Left (InclusionDependencyCheckError "testconstraint" Nothing)),
                           ("y:=s; key testconstraint {s#} y; insert y relation{tuple{city \"London\", s# \"S6\", sname \"gonk\", status 50}}; x:=y{s#} = s{s#} union relation{tuple{s# \"S6\"}}", Right relationTrue),
                           --test binary bytestring data type
                           ("x:=relation{tuple{y bytestring(\"dGVzdGRhdGE=\")}}", mkRelationFromList byteStringAttributes [[ByteStringAtom (TE.encodeUtf8 "testdata")]]),
                           --test Maybe Text
                           ("x:=relation{tuple{a Just \"spam\"}}", mkRelationFromList simpleMaybeTextAttributes [[ConstructedAtom "Just" maybeTextAtomType [TextAtom "spam"]]]),
                           --test Maybe Integer
                           ("x:=relation{tuple{a Just 3}}", mkRelationFromList simpleMaybeIntAttributes [[ConstructedAtom "Just" maybeIntegerAtomType [IntegerAtom 3]]]),
                           --test Either Integer Text
                           ("x:=relation{tuple{a Left 3}}",  Left (TypeConstructorTypeVarMissing "b")), -- Left 3, alone is not enough information to imply the type
                           ("x:=relation{a Either Integer Text}{tuple{a Left 3}}", mkRelationFromList simpleEitherIntTextAttributes [[ConstructedAtom "Left" (eitherAtomType IntegerAtomType TextAtomType) [IntegerAtom 3]]]),
                           --test datetime constructor
                           ("x:=relation{tuple{a dateTimeFromEpochSeconds(1495199790)}}", mkRelationFromList (A.attributesFromList [Attribute "a" DateTimeAtomType]) [[DateTimeAtom (posixSecondsToUTCTime(realToFrac (1495199790 :: Int)))]]),
                           --test Day constructor
                           ("x:=relation{tuple{a fromGregorian(2017,05,30)}}", mkRelationFromList (A.attributesFromList [Attribute "a" DayAtomType]) [[DayAtom (fromGregorian 2017 05 30)]])
                          ]

assertTutdEqual :: ResolvedDatabaseContext -> TransactionId -> TransactionGraph -> Either RelationalError Relation -> Text -> Assertion
assertTutdEqual contextIn transId graph expected tutd = assertEqual (unpack tutd) expected interpreted
  where
    interpreted = case interpretDatabaseContextExpr (toDatabaseContext contextIn) transId graph tutd of
      Left err -> Left err
      Right context -> do
        case resolveDBC' graph context relationVariables of
          Left err -> Left err
          Right relVars ->
            case M.lookup "x" relVars of
              Nothing -> Left $ RelVarNotDefinedError "x"
              Just relExpr -> do
                let env = freshGraphRefRelationalExprEnv (Just context) graph
                runGraphRefRelationalExprM env (evalGraphRefRelationalExpr relExpr)


transactionGraphBasicTest :: Test
transactionGraphBasicTest = TestCase $ do
  (_, dbconn) <- dateExamplesConnection emptyNotificationCallback
  graph <- transactionGraph_ dbconn
  assertEqual "validate bootstrapped graph" (validateGraph graph) Nothing

--add a new transaction to the graph, validate it is in the graph
transactionGraphAddCommitTest :: Test
transactionGraphAddCommitTest = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  case parseTutorialD "x:=s" of
    Left err -> assertFailure (show err)
    Right parsed -> do 
      result <- evalTutorialD sessionId dbconn UnsafeEvaluation parsed
      case result of
        QuitResult -> assertFailure "quit?"
        DisplayResult _ -> assertFailure "display?"
        DisplayIOResult _ -> assertFailure "displayIO?"
        DisplayRelationResult _ -> assertFailure "displayrelation?"
        DisplayDataFrameResult _ -> assertFailure "displaydataframe?"
        DisplayParseErrorResult _ _ -> assertFailure "displayparseerror?"
        DisplayErrorResult err -> assertFailure (show err)   
        I.QuietSuccessResult -> do
          commit sessionId dbconn >>= eitherFail
          eX <- executeRelationalExpr sessionId dbconn (RelationVariable "x" ())
          assertEqual "ensure x was added" (Right suppliersRel) eX
        DisplayRelationalErrorResult err -> assertFailure (show err)
        DisplayHintWith{} -> assertFailure "displayhintwith?"

transactionRollbackTest :: Test
transactionRollbackTest = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  graph <- transactionGraph_ dbconn
  executeDatabaseContextExpr sessionId dbconn (Assign "x" (RelationVariable "s" ())) >>= eitherFail
  rollback sessionId dbconn >>= eitherFail
  graph' <- transactionGraph_ dbconn
  eX <- executeRelationalExpr sessionId dbconn (RelationVariable "x" ())
  
  assertEqual "validate context" (Left (RelVarNotDefinedError "x")) eX
  let graphEq graphArg = S.map transactionId (transactionsForGraph graphArg)
  assertEqual "validate graph" (graphEq graph) (graphEq graph')

--commit a new transaction with "x" relation, jump to first transaction, verify that "x" is not present
transactionJumpTest :: Test
transactionJumpTest = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  firstUUID <- Discon.disconTransactionId <$> disconnectedTransaction_ sessionId dbconn
  executeDatabaseContextExpr sessionId dbconn (Assign "x" (RelationVariable "s" ())) >>= eitherFail
  commit sessionId dbconn >>= eitherFail
  --perform the jump
  executeTransactionGraphExpr sessionId dbconn (JumpToTransaction firstUUID) >>= eitherFail
  --check that the disconnected transaction does not include "x"
  eX <- executeRelationalExpr sessionId dbconn (RelationVariable "x" ())
  assertEqual "ensure x is not present" (Left (RelVarNotDefinedError "x")) eX
--branch from the first transaction and verify that there are two heads
transactionBranchTest :: Test
transactionBranchTest = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  mapM_ (>>= eitherFail) [executeAlterTransactionGraphExpr sessionId dbconn (Branch "test"),
                                  executeDatabaseContextExpr sessionId dbconn (Assign "x" (RelationVariable "s" ())),
                                  commit sessionId dbconn,
                                  executeTransactionGraphExpr sessionId dbconn (JumpToHead "master"),
                                  executeDatabaseContextExpr sessionId dbconn (Assign "y" (RelationVariable "s" ()))
                  ]
  graph <- transactionGraph_ dbconn
  assertBool "master branch exists" $ isJust (transactionForHead "master" graph)
  assertBool "test branch exists" $ isJust (transactionForHead "test" graph)

-- test that overlapping attribute names with different types fail with an error
failJoinTest :: Test
failJoinTest = TestCase $ do
  (graph, transId) <- freshTransactionGraph' dateExamples  
  assertTutdEqual basicDatabaseContext transId graph err "x:=relation{tuple{test 4}} join relation{tuple{test \"test\"}}"
  where
    err = Left (TupleAttributeTypeMismatchError (A.attributesFromList [Attribute "test" IntegerAtomType]))

simpleJoinTest :: Test
simpleJoinTest = TestCase $ do
  (graph, transId) <- freshTransactionGraph' dateExamples    
  assertTutdEqual dateExamples transId graph joinedRel "x:=s join sp"
    where
        attrs = A.attributesFromList [Attribute "city" TextAtomType,
                                      Attribute "qty" IntegerAtomType,
                                      Attribute "p#" TextAtomType,
                                      Attribute "s#" TextAtomType,
                                      Attribute "sname" TextAtomType,
                                      Attribute "status" IntegerAtomType]
        joinedRel = mkRelationFromList attrs [[TextAtom "London", IntegerAtom 100, TextAtom "P6", TextAtom "S1", TextAtom "Smith", IntegerAtom 20],
                                              [TextAtom "London", IntegerAtom 400, TextAtom "P3", TextAtom "S1", TextAtom "Smith", IntegerAtom 20],
                                              [TextAtom "London", IntegerAtom 400, TextAtom "P5", TextAtom "S4", TextAtom "Clark", IntegerAtom 20],
                                              [TextAtom "London", IntegerAtom 300, TextAtom "P1", TextAtom "S1", TextAtom "Smith", IntegerAtom 20],
                                              [TextAtom "Paris", IntegerAtom 200, TextAtom "P2", TextAtom "S3", TextAtom "Blake", IntegerAtom 30],
                                              [TextAtom "Paris", IntegerAtom 300, TextAtom "P1", TextAtom "S2", TextAtom "Jones", IntegerAtom 10],
                                              [TextAtom "London", IntegerAtom 100, TextAtom "P5", TextAtom "S1", TextAtom "Smith", IntegerAtom 20],
                                              [TextAtom "London", IntegerAtom 300, TextAtom "P4", TextAtom "S4", TextAtom "Clark", IntegerAtom 20],
                                              [TextAtom "Paris", IntegerAtom 400, TextAtom "P2", TextAtom "S2", TextAtom "Jones", IntegerAtom 10],
                                              [TextAtom "London", IntegerAtom 200, TextAtom "P2", TextAtom "S1", TextAtom "Smith", IntegerAtom 20],
                                              [TextAtom "London", IntegerAtom 200, TextAtom "P4", TextAtom "S1", TextAtom "Smith", IntegerAtom 20],
                                              [TextAtom "London", IntegerAtom 200, TextAtom "P2", TextAtom "S4", TextAtom "Clark", IntegerAtom 20]
                                              ]
                    
                    
{-
inclusionDependencies :: Connection -> M.Map IncDepName InclusionDependency
inclusionDependencies (InProcessConnection (DisconnectedTransaction _ context)) = inclusionDependencies context
inclusionDependencies _ = error "remote connection used"                       
-}
                           
-- test notifications over the InProcessConnection
testNotifications :: Test
testNotifications = TestCase $ do
  notifmvar <- newEmptyMVar
  let notifCallback mvar _ _ = putMVar mvar ()
      relvarx = RelationVariable "x" ()
  (sess, conn) <- dateExamplesConnection (notifCallback notifmvar)
  executeDatabaseContextExpr sess conn (Assign "x" (ExistingRelation relationTrue)) >>= eitherFail
  executeDatabaseContextExpr sess conn (AddNotification "test notification" relvarx relvarx relvarx) >>= eitherFail
  commit sess conn >>= eitherFail
  executeDatabaseContextExpr sess conn (Assign "x" (ExistingRelation relationFalse)) >>= eitherFail
  commit sess conn >>= eitherFail
  takeMVar notifmvar

  -- test documented behavior
  let notifCallback2 name _evald = 
        assertEqual "notification" "steve_change" name
  (session, dbconn) <- dateExamplesConnection notifCallback2
  executeTutorialD session dbconn "person:=relation{tuple{name \"Steve\",address \"Main St.\"},tuple{name \"John\", address \"Elm St.\"}}"
  executeTutorialD session dbconn "notify steve_change person where name=\"Steve\" true (person where name=\"Steve\"){address}"
  _ <- commit session dbconn
  executeTutorialD session dbconn "update person where name=\"Steve\" (address:=\"Grove St.\")"
  expectTutorialDErr session dbconn (T.isPrefixOf "NotificationValidationError") "undefine person"
      
testTypeConstructors :: Test
testTypeConstructors = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sessionId dbconn "data Hair = Color Text | Bald | UserRefusesToSpecify"
  executeTutorialD sessionId dbconn "x:=relation{a Hair}{tuple{a Color \"Blonde\"},tuple{a Bald},tuple{a UserRefusesToSpecify}}"
  executeTutorialD sessionId dbconn "data Tree a = Node a (Tree a) (Tree a) | EmptyNode"
  executeTutorialD sessionId dbconn "y:=relation{a Tree Integer}{tuple{a Node 3 (Node 4 EmptyNode EmptyNode) EmptyNode},tuple{a Node 4 EmptyNode EmptyNode}}"
  
testMergeTransactions :: Test
testMergeTransactions = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  mapM_ (executeTutorialD sessionId dbconn) [
   ":branch branchA",
   "conflictrv := relation{conflict Integer}{tuple{conflict 1}}",
   ":commit",
   ":jumphead master",
   ":branch branchB",
   "conflictrv := relation{conflict Integer}{tuple{conflict 2}}",
   ":commit",
   ":mergetrans union branchA branchB"
   ]
  case mkRelationFromList (attributesFromList [Attribute "conflict" IntegerAtomType]) [[IntegerAtom 1],[IntegerAtom 2]] of
    Left err -> assertFailure (show err)
    Right conflictCheck -> do
      eRv <- executeRelationalExpr sessionId dbconn (RelationVariable "conflictrv" ())
      case eRv of
        Left err -> assertFailure (show err)
        Right conflictrv -> assertEqual "conflict union merge relvar" conflictCheck conflictrv

testComments :: Test
testComments = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback  
  mapM_ (executeTutorialD sessionId dbconn) [
    ":branch testbranch --test comment",
    ":jumphead {- test comment -} master"]
  
-- create a graph and query from two disparate contexts  
testTransGraphRelationalExpr :: Test    
testTransGraphRelationalExpr = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback  
  mapM_ (executeTutorialD sessionId dbconn) [
    "x:=s", --dud relvar so that the commit isn't empty
    ":commit",
    ":branch testbranch",
    "insert s relation{tuple{city \"Boston\", s# \"S9\", sname \"Smithers\", status 50}}",
    ":commit"
    ]
  let masterMarker = TransactionIdHeadNameLookup "master" []
      testBranchMarker = TransactionIdHeadNameLookup "testbranch" []
      sattrs = attributesFromList [Attribute "city" TextAtomType,
                                   Attribute "sname" TextAtomType,
                                   Attribute "s#" TextAtomType,
                                   Attribute "status" IntegerAtomType]
      expectedRel = mkRelationFromList sattrs [[TextAtom "Boston",
                                                TextAtom "Smithers",
                                                TextAtom "S9",
                                                IntegerAtom 50]]
  diff <- executeTransGraphRelationalExpr sessionId dbconn (Difference (RelationVariable "s" testBranchMarker) (RelationVariable "s" masterMarker))
  assertEqual "difference in s" expectedRel diff 
  
  --test graph traversal (head backtracking)
  let testBranchBacktrack = TransactionIdHeadNameLookup "testbranch" [TransactionIdHeadParentBacktrack 1]
  backtrackRel <- executeTransGraphRelationalExpr sessionId dbconn (Equals (RelationVariable "s" testBranchBacktrack) (RelationVariable "s" masterMarker))
  assertEqual "backtrack to master" (Right relationTrue) backtrackRel
  
  --test walkback to time (stay in current location)
  now <- getCurrentTime
  headId <- headTransactionId sessionId dbconn
  _ <- executeTransactionGraphExpr sessionId dbconn (WalkBackToTime now)
  headId' <- headTransactionId sessionId dbconn
  assertEqual "transaction walk back stays in place" headId headId'

  --test branch deletion
  mapM_ (executeTutorialD sessionId dbconn) [
        ":jumphead master",
        ":deletebranch testbranch"]
  eEvald <- case parseTutorialD ":jumphead testbranch" of
    Left _ -> assertFailure "jumphead parse error" >> error "x"
    Right parsed -> evalTutorialD sessionId dbconn UnsafeEvaluation parsed
  case eEvald of
    DisplayErrorResult err -> assertEqual "testbranch deletion"  (show (NoSuchHeadNameError "testbranch")) (unpack err)
    _ -> assertFailure "failed to delete branch"
    
testMultiAttributeRename :: Test
testMultiAttributeRename = TestCase $ do
  (graph, transId) <- freshTransactionGraph' dateExamples    
  assertTutdEqual dateExamples transId graph renamedRel "x:=s rename {city as town, status as price} where false"
  where
    sattrs = attributesFromList [Attribute "town" TextAtomType,
                                 Attribute "sname" TextAtomType,
                                 Attribute "s#" TextAtomType,
                                 Attribute "price" IntegerAtomType]
    renamedRel = mkRelationFromList sattrs []

testSchemaExpr :: Test
testSchemaExpr = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback  
  mapM_ (executeTutorialD sessionId dbconn) [
    ":addschema test (isopassthrough \"true\", isopassthrough \"false\", isorename \"supplier\" \"s\", isorename \"supplier_product\" \"sp\", isounion \"heavy_product\" \"light_product\" \"p\" gte(17,@weight))",
    ":setschema test",
    ""
    ]
  eLightProduct <- executeRelationalExpr sessionId dbconn (RelationVariable "light_product" ())
  lightProduct <- assertEither eLightProduct
  let restriction = NotPredicate (AtomExprPredicate (FunctionAtomExpr "gte" [NakedAtomExpr (IntegerAtom 17), AttributeAtomExpr "weight"] ()))
  setCurrentSchemaName sessionId dbconn Sess.defaultSchemaName >>= eitherFail
  eRestrictedProduct <- executeRelationalExpr sessionId dbconn (Restrict restriction (RelationVariable "p" ()))
  restrictedProduct <- assertEither eRestrictedProduct
  assertEqual "light product" restrictedProduct lightProduct
  
assertEither :: (Show a) => Either a b -> IO b
assertEither x = case x of
  Left err -> assertFailure (show err) >> undefined
  Right val -> pure val
  
-- | Validate that a tuple passed through the context correctly typechecks and propagates to the AttributeAtomExpr.
testContextTuples :: Test
testContextTuples = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sessionId dbconn "x := (s : { parts := p rename {city as pcity} where pcity=@city}) : {z:=count(@parts)}"

  executeTutorialD sessionId dbconn "y:=x{city,z}"
  eRv <- executeRelationalExpr sessionId dbconn (RelationVariable "y" ())
  let expectedRel = mkRelationFromList (attributesFromList [Attribute "city" TextAtomType,
                                                            Attribute "z" IntegerAtomType])
                    [[TextAtom "Paris", IntegerAtom 2],
                     [TextAtom "London", IntegerAtom 3],
                     [TextAtom "Athens", IntegerAtom 0]]
  assertEqual "validate parts count" expectedRel eRv
  
  executeTutorialD sessionId dbconn "rv1:=relation{tuple{val1 1}}"
  executeTutorialD sessionId dbconn "rv2:=relation{tuple{val2 1},tuple{val2 2}}"
  --check subexpression evaluation in restriction predicate
  -- "rv1 where ((rv2 where val=@test) {})"
  let correctSubexpr = Restrict (AttributeEqualityPredicate "val2" (AttributeAtomExpr "val2")) (RelationVariable "rv2" ())
      mainExpr subexpr = Restrict 
                         (RelationalExprPredicate
                          (Project AN.empty subexpr)) (RelationVariable "rv1" ())
  eRv2 <- executeRelationalExpr sessionId dbconn (mainExpr correctSubexpr)
  let expectedRel2 = mkRelationFromList (attributesFromList [Attribute "val1" IntegerAtomType]) [[IntegerAtom 1]]
  assertEqual "validate sub-expression attribute" expectedRel2 eRv2
  
  --check error in subexpression
  let wrongSubexpr = Restrict (AttributeEqualityPredicate "nosuchattr" (AttributeAtomExpr "val1")) (RelationVariable "rv2" ())
  eRv3 <- executeRelationalExpr sessionId dbconn (mainExpr wrongSubexpr)
  assertEqual "validate missing attribute in subexpression" (Left (NoSuchAttributeNamesError (S.singleton "nosuchattr"))) eRv3
  
-- | Add a functional dependency on sname -> status and insert one tuple which is valid and another tuple which is invalid.
testFunctionalDependencies :: Test    
testFunctionalDependencies = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback  
  executeTutorialD sessionId dbconn "funcdep sname_status (sname) -> (status) s"
  --insert a new, valid tuple
  executeTutorialD sessionId dbconn "insert s relation{tuple{city \"Boston\", s# \"S6\", sname \"Stevens\", status 30}}"
  --insert an constraint-violating tuple
  let expectedError = "InclusionDependencyCheckError \"sname_status_A\""
  expectTutorialDErr sessionId dbconn (T.isPrefixOf expectedError) "insert s relation{tuple{city \"Boston\", s# \"S7\", sname \"Jones\", status 20}}"

testEmptyCommits :: Test
testEmptyCommits = TestCase $ do 
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  dirty <- disconnectedTransactionIsDirty sessionId dbconn
  assertEqual "no change not dirty" (Right False) dirty
  Right () <- commit sessionId dbconn

  --insert no tuples
  Right () <- executeDatabaseContextExpr sessionId dbconn (Insert "s" (RelationVariable "s" ()))
  dirty' <- disconnectedTransactionIsDirty sessionId dbconn
  assertEqual "empty insert empty commit" (Right False) dirty'
  Right () <- commit sessionId dbconn
  
  --update no tuples- since we defer the restriction, we can only assume the context is dirty, perhaps the constraint checker could offer an optimization here
  Right () <- executeDatabaseContextExpr sessionId dbconn (Update "s" (M.singleton "sname" (NakedAtomExpr (TextAtom "Bob"))) (AttributeEqualityPredicate "sname" (NakedAtomExpr (TextAtom "Mike"))))
  dirty'' <- disconnectedTransactionIsDirty sessionId dbconn
  assertEqual "empty update empty commit" (Right True) dirty''
  Right () <- commit sessionId dbconn

  --assign the same rel expr
  Right () <- executeDatabaseContextExpr sessionId dbconn (Assign "true" (ExistingRelation relationTrue))
  sameREdirty <- disconnectedTransactionIsDirty sessionId dbconn
  assertEqual "same relexpr assigned" (Right False) sameREdirty
  Right () <- commit sessionId dbconn
  
  --delete no tuples
  Right () <- executeDatabaseContextExpr sessionId dbconn (Delete "s" (AttributeEqualityPredicate "sname" (NakedAtomExpr (TextAtom "Mike"))))
  dirty''' <- disconnectedTransactionIsDirty sessionId dbconn
  assertEqual "empty delete empty commit" (Right True) dirty'''
 
testIntervalAtom :: Test  
testIntervalAtom = TestCase $ do  
  --test interval creation
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback  
  executeTutorialD sessionId dbconn "x:=relation{tuple{n 1, a interval(3,4,False,False), b interval(4,5,False,False)}, tuple{n 2,a interval(3,4,True,True), b interval(4,5,True,True)}}"
  --test failed interval creation
  let err1 = "AtomFunctionUserError InvalidIntervalOrderingError"
      err2 = "RelationTypeMismatchError"
  expectTutorialDErr sessionId dbconn (T.isPrefixOf err1) "z:=relation{tuple{a interval(4,3,False,False)}}"
  expectTutorialDErr sessionId dbconn (T.isPrefixOf err2) "z:=relation{tuple{a interval(\"s\",\"t\",False,False)}}"  

  --test interval_overlaps
  executeTutorialD sessionId dbconn "y:=x:{c:=interval_overlaps(@a,@b)}"
  eRv <- executeRelationalExpr sessionId dbconn (Project (AttributeNames (S.fromList ["n","c"])) (RelationVariable "y" ()))
  assertEqual "interval overlap check" (mkRelationFromList (attributesFromList [Attribute "c" BoolAtomType,Attribute "n" IntegerAtomType]) [[BoolAtom True, IntegerAtom 1], [BoolAtom False, IntegerAtom 2]]) eRv

testListConstructedAtom :: Test
testListConstructedAtom = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sessionId dbconn "x:=relation{tuple{l Cons 4 (Cons 5 Empty)}}"
  let err1 = "ConstructedAtomArgumentCountMismatchError 2 1"
  expectTutorialDErr sessionId dbconn (T.isPrefixOf err1) "z:=relation{tuple{l Cons 4}}"
  let err2 = "TypeConstructorAtomTypeMismatch \"List\" IntegerAtomType"
  expectTutorialDErr sessionId dbconn (T.isPrefixOf err2) "z:=relation{tuple{l Cons 4 5}}"
  
testTypeChecker :: Test  
testTypeChecker = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback  
  let err1 = "AtomFunctionTypeError \"max\" 1"
  expectTutorialDErr sessionId dbconn (T.isPrefixOf err1) "x:=relation{tuple{a 0}}:{b:=max(@a)}"
  
--exercise expression parser
testRestrictionPredicateExprs :: Test
testRestrictionPredicateExprs = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback  
  -- or
  executeTutorialD sessionId dbconn "x:=s where status=20 or status=10"
  eRvOr <- executeRelationalExpr sessionId dbconn (RelationVariable "x" ())
  let expectedRelOr = restrict (\tuple _ -> 
                               pure (atomForAttributeName "status" tuple `elem` [Right (IntegerAtom 10), Right (IntegerAtom 20)])) suppliersRel
  assertEqual "status 20 or 10" expectedRelOr eRvOr
  -- and
  executeTutorialD sessionId dbconn "x:=s where status=20 and status=10"
  eRvAnd <- executeRelationalExpr sessionId dbconn (RelationVariable "x" ())
  let expectedRelAnd = Right (emptyRelationWithAttrs (R.attributes suppliersRel))
  assertEqual "status 20 and 10" expectedRelAnd eRvAnd
  
testRelationalAttributeNames :: Test
testRelationalAttributeNames = TestCase $ do
    (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback  
    executeTutorialD sessionId dbconn "x:=(s join sp){all from sp}"
    eRv <- executeRelationalExpr sessionId dbconn (RelationVariable "x" ())
    case eRv of
      Left err -> assertFailure (show err)
      Right rel -> 
        assertEqual "attributes from sp" (R.attributes supplierProductsRel) (R.attributes rel)
    
testSemijoin :: Test
testSemijoin = TestCase $ do
    (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback  
    executeTutorialD sessionId dbconn "x:=s semijoin sp"
    executeTutorialD sessionId dbconn "y:=s where not (sname = \"Adams\")"
    eX <- executeRelationalExpr sessionId dbconn (RelationVariable "x" ())
    eY <- executeRelationalExpr sessionId dbconn (RelationVariable "y" ())
    assertEqual "semijoin missing Adams" eX eY

testAntijoin :: Test
testAntijoin = TestCase $ do
    (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback  
    executeTutorialD sessionId dbconn "x:=s antijoin sp"
    executeTutorialD sessionId dbconn "y:=s where sname = \"Adams\""
    eX <- executeRelationalExpr sessionId dbconn (RelationVariable "x" ())
    eY <- executeRelationalExpr sessionId dbconn (RelationVariable "y" ())
    assertEqual "antijoin only Adams" eX eY
  
testRelationAttributeDefinition :: Test
testRelationAttributeDefinition = TestCase $ do
    -- test normal subrelation construction
    (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback  
    executeTutorialD sessionId dbconn "x:=relation{a relation{b Integer}}{tuple{a relation{tuple{b 4}}}}"
    eX <- executeRelationalExpr sessionId dbconn (RelationVariable "x" ())  
    let expected = mkRelationFromList attrs [[RelationAtom subRel]]
        attrs = attributesFromList [Attribute "a" (RelationAtomType subRelAttrs)]
        subRelAttrs = attributesFromList [Attribute "b" IntegerAtomType]
        subRel = case mkRelationFromList subRelAttrs [[IntegerAtom 4]] of
                   Left err -> error (show err)
                   Right rel -> rel
    assertEqual "relation attribute construction" expected eX
    -- test rejected subrelation construction due to floating type variables
    expectTutorialDErr sessionId dbconn (T.isPrefixOf "TypeConstructorTypeVarMissing") "y:=relation{a relation{b x}}"
    
testAssignWithTypeVar :: Test
testAssignWithTypeVar = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback    
  let err1 = "TypeConstructorTypeVarMissing"
  expectTutorialDErr sessionId dbconn (T.isPrefixOf err1) "y:=relation{a int, b invalidtype}"
  
testDefineWithTypeVar :: Test  
testDefineWithTypeVar = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback    
  let err1 = "TypeConstructorTypeVarMissing"
  expectTutorialDErr sessionId dbconn (T.isInfixOf err1) "y::{a int, b invalidtype}"

testIntervalType :: Test
testIntervalType = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sessionId dbconn "x:=relation{a Interval Integer}"
  eX <- executeRelationalExpr sessionId dbconn (RelationVariable "x" ())
  let expectedIntervalInt = mkRelationFromList expectedAttrs []
      expectedAttrs = A.attributesFromList [Attribute "a" (intervalAtomType IntegerAtomType)]
  assertEqual "Interval Int attribute" expectedIntervalInt eX
  
testArbitraryRelation :: Test  
testArbitraryRelation = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sessionId dbconn "createarbitraryrelation rv1 {a Integer} 5-10"
  executeTutorialD sessionId dbconn "createarbitraryrelation rv2 {a Integer, b relation{c Integer}} 10-100"
  executeTutorialD sessionId dbconn "createarbitraryrelation rv3 {a Int, b relation{c Interval Int}} 3-100"
  
testNonEmptyListType :: Test
testNonEmptyListType = TestCase $ do
  --create a NonEmptyList
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback  
  executeTutorialD sessionId dbconn "x:=relation{tuple{a NECons 3 (Cons 4 Empty)}} : {x:=nonEmptyListHead(@a)}"
  eX <- executeRelationalExpr sessionId dbconn (RelationVariable "x" ())
  let expected = mkRelationFromList attrs [[nelist, nehead]]
      attrs = attributesFromList [Attribute "a" neListType,
                                  Attribute "x" IntegerAtomType]
      neListType = nonEmptyListAtomType IntegerAtomType
      listType = listAtomType IntegerAtomType
      nelist = ConstructedAtom "NECons" (nonEmptyListAtomType IntegerAtomType) [
        IntegerAtom 3,
        ConstructedAtom "Cons" listType [IntegerAtom 4, ConstructedAtom "Empty" listType []]]
      nehead = IntegerAtom 3
  assertEqual "non-empty list type construction" expected eX
  
testUnresolvedAtomTypes :: Test
testUnresolvedAtomTypes = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  let err1 = "TypeConstructorTypeVarMissing"
  expectTutorialDErr sessionId dbconn (T.isPrefixOf err1) "x:=relation{tuple{a Empty}}"
  executeTutorialD sessionId dbconn "x:=relation{a List Int}{tuple{a Empty}}"

-- with (x as s) s
-- TODO: add tests with names shadowing existing with clauses and rv names
testWithClause :: Test
testWithClause = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sessionId dbconn "x:=with (w as s) w"
  eX <- executeRelationalExpr sessionId dbconn (RelationVariable "x" ())
  assertEqual "with x as s" (Right suppliersRel) eX

  executeTutorialD sessionId dbconn "y:=with(a as true) (with (b as a) b)"
  eY <- executeRelationalExpr sessionId dbconn (RelationVariable "y" ())
  assertEqual "nested with" (Right relationTrue) eY
  
  let err1 = "RelVarAlreadyDefinedError"  
  expectTutorialDErr sessionId dbconn (T.isPrefixOf err1) "x:=with (s as s) s"  
  
  expectTutorialDErr sessionId dbconn (T.isPrefixOf err1) "x:=with (s as sp) s"  

testAtomFunctionArgumentMismatch :: Test
testAtomFunctionArgumentMismatch = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  let err1 = "AtomFunctionTypeError"
  --atom function type mismatch
  expectTutorialDErr sessionId dbconn (T.isPrefixOf err1) "x:=relation{tuple{a 5}} where gt(@a,1.5)"
  --wrong argument count
  let err2 = "FunctionArgumentCountMismatchError"
  expectTutorialDErr sessionId dbconn (T.isPrefixOf err2) "x:=relation{tuple{a 5}} where gt(@a,1,3)"

testInvalidDataConstructor :: Test
testInvalidDataConstructor = TestCase $ do
  --test that a referenced TypeConstructor in a DataConstructor definition matches the expected count of arguments
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  let err1 = "ConstructedAtomArgumentCountMismatchError"
  expectTutorialDErr sessionId dbconn (T.isPrefixOf err1) "data TestT = TestT Maybe Int"

testBasicList :: Test
testBasicList = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sessionId dbconn "x := relation{tuple{ a (Cons 1 (Cons 2 Empty)) }}"

testRelationDeclarationMismatch :: Test
testRelationDeclarationMismatch = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  expectTutorialDErr sessionId dbconn (T.isPrefixOf "AtomTypeMismatchError") "data A a = A a | B | C; a := relation{a A Integer}{tuple{a A \"1\"}}"

--generate errors when the tuples in a new relation don't match
testInvalidTuples :: Test
testInvalidTuples = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  expectTutorialDErr sessionId dbconn (T.isPrefixOf "TupleAttributeTypeMismatchError") ":showexpr relation{tuple{a 1},tuple{b 2}}"
  expectTutorialDErr sessionId dbconn (T.isPrefixOf "TupleAttributeTypeMismatchError") ":showexpr relation{tuple{a 1},tuple{a 2, b 3}}"
--  expectTutorialDErr sessionId dbconn (T.isPrefixOf "ParseErrorBundle") ":showexpr relation{tuple{a 2, a 3}}" --parse failure can't be validated with this function

testSelfReferencingUncommittedContext :: Test
testSelfReferencingUncommittedContext = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sessionId dbconn "s:=s union s"
  eS <- executeRelationalExpr sessionId dbconn (RelationVariable "s" ())
  _ <- rollback sessionId dbconn
  eSorig <- executeRelationalExpr sessionId dbconn (RelationVariable "s" ())  
  assertEqual "s=s'" eSorig eS

testUnionAndIntersectionAttributeExprs :: Test
testUnionAndIntersectionAttributeExprs = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sessionId dbconn "x:=s{intersection of {all but sname} {sname}}"
  xRel <- executeRelationalExpr sessionId dbconn (RelationVariable "x" ())
  assertEqual "intersection attrs" (Right relationTrue) xRel

  executeTutorialD sessionId dbconn "y:=s{union of {all but sname} {sname}}"
  yRel <- executeRelationalExpr sessionId dbconn (RelationVariable "y" ())
  assertEqual "union attrs" (Right suppliersRel) yRel

-- ensure that an undefined table triggers constraint validation #306
testUndefineConstraints :: Test
testUndefineConstraints = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  let expectedErr = InclusionDependencyCheckError "bad" (Just (RelVarNotDefinedError "x"))
  expectTutorialDErr sessionId dbconn (== T.pack (show expectedErr)) "x:=true;constraint bad x equals true;undefine x"

testQuotedRelVarNames :: Test
testQuotedRelVarNames = TestCase $ do
  let parseDBExpr = parse databaseContextExprP ""
      true = RelationVariable "true" ()
      assign rv = Right (Assign rv true)
  assertEqual "quoted rv" (assign "TEST") (parseDBExpr "`TEST` := true")
  assertEqual "quoted backtick rv" (assign "TEST`TEST") (parseDBExpr "`TEST\\`TEST`:=true")
  assertEqual "multibyte rv" (assign "漢字") (parseDBExpr "`漢字`:=true")
  let qAttrExpected = Right (Assign "test" (MakeRelationFromExprs Nothing (TupleExprs () [TupleExpr (M.singleton "\28450\23383" (NakedAtomExpr (TextAtom "test")))])))
  assertEqual "quoted attribute" qAttrExpected (parseDBExpr "test:=relation{tuple{`漢字` \"test\"}}")
  assertBool "invalid quoted rv" (isLeft (parseDBExpr "`TEST:=true"))

testScientific :: Test
testScientific = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  let getX = executeRelationalExpr sessionId dbconn (RelationVariable "x" ())
  executeTutorialD sessionId dbconn "x:=relation{tuple{a scientific(1,int(100))}}"
  createSciRel <- getX
  let expectedRel = mkRelationFromList attrs [[ScientificAtom (scientific 1 100)]]
      attrs = attributesFromList [Attribute "a" ScientificAtomType]
  assertEqual "scientific creation" expectedRel createSciRel
  --test some math functions
  executeTutorialD sessionId dbconn "x:=relation{tuple{a scientific_add(scientific(2,int(100)),scientific(1,int(100)))}}"
  addSciRel <- getX
  assertEqual "scientific add" (mkRelationFromList attrs [[ScientificAtom (scientific 3 100)]]) addSciRel

testExtendProcessorTuplePushdown :: Test
testExtendProcessorTuplePushdown = TestCase $ do
  --test that context-based tuples are pushed through to the tuple extend processor used commonly in OUTER-JOIN-equivalent extensions
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sessionId dbconn "x := (relation{tuple{p# \"P7\", city \"Reykjavik\", color \"Beige\", pname \"Widget\", weight 21}} union p : {suppliers := (sp rename {p# as pid} where p#=@pid) {s#}}) {p#,suppliers}"
  {-"
:showexpr relation{tuple{p# "P5", suppliers relation{tuple{s# "S1"}, tuple{s# "S4"}}}, tuple{p# "P4", suppliers relation{tuple{s# "S1"}, tuple{s# "S4"}}}, tuple{p# "P2", suppliers relation{tuple{s# "S1"}, tuple{s# "S2"}, tuple{s# "S3"}, tuple{s# "S4"}}}, tuple{p# "P3", suppliers relation{tuple{s# "S1"}}}, tuple{p# "P6", suppliers relation{tuple{s# "S1"}}}, tuple{p# "P1", suppliers relation{tuple{s# "S1"},tuple{s# "S2"}}}, tuple{p# "P7", suppliers relation{s# Text}}} 

-}
  executeTutorialD sessionId dbconn "y := relation{tuple{p# \"P5\", suppliers relation{tuple{s# \"S1\"}, tuple{s# \"S4\"}}}, tuple{p# \"P4\", suppliers relation{tuple{s# \"S1\"}, tuple{s# \"S4\"}}}, tuple{p# \"P2\", suppliers relation{tuple{s# \"S1\"}, tuple{s# \"S2\"}, tuple{s# \"S3\"}, tuple{s# \"S4\"}}}, tuple{p# \"P3\", suppliers relation{tuple{s# \"S1\"}}}, tuple{p# \"P6\", suppliers relation{tuple{s# \"S1\"}}}, tuple{p# \"P1\", suppliers relation{tuple{s# \"S1\"},tuple{s# \"S2\"}}}, tuple{p# \"P7\", suppliers relation{s# Text}}}"
  res <- executeRelationalExpr sessionId dbconn (Equals (RelationVariable "x" ()) (RelationVariable "y" ()))
  assertEqual "outer join grouping" (Right relationTrue) res 
  
testDDLHash :: Test
testDDLHash = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  Right hash1 <- getDDLHash sessionId dbconn
  -- add a new rv
  executeTutorialD sessionId dbconn "x:=true"
  Right hash2 <- getDDLHash sessionId dbconn  
  assertBool "add relvar" (hash1 /= hash2)
  -- the test should break if the hash is calculated differently
  assertEqual "static hash check" "Aoyq+ARAHMyXGYkLewesLxkIEEgBtpAEaYtzPOpIXo4=" (B64.encode (_unSecureHash hash1))
  -- remove an rv
  executeTutorialD sessionId dbconn "undefine x"
  Right hash3 <- getDDLHash sessionId dbconn
  assertEqual "undefine relvar" hash1 hash3
  -- add a constraint
  executeTutorialD sessionId dbconn "constraint s_status_less_than_50 (s where gte(@status,50)){} in false"
  Right hash4 <- getDDLHash sessionId dbconn
  assertBool "add relvar" (hash4 `notElem` [hash1, hash2, hash3])
  -- change an rv's attributes
  executeTutorialD sessionId dbconn "y:=relation{a Integer}"
  Right hash5 <- getDDLHash sessionId dbconn
  executeTutorialD sessionId dbconn "undefine y"
  executeTutorialD sessionId dbconn "y:=relation{b Integer}"
  Right hash6 <- getDDLHash sessionId dbconn
  assertBool "change rv attribute" (hash6 `notElem` [hash1, hash2, hash3, hash4, hash5])
  -- change an rv name
  executeTutorialD sessionId dbconn "undefine y"
  executeTutorialD sessionId dbconn "z:=relation{b Integer}"
  Right hash7 <- getDDLHash sessionId dbconn
  assertBool "change rv attribute" (hash7 `notElem` [hash1, hash2, hash3, hash4, hash5, hash6])  

testShowDDL :: Test
testShowDDL = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  Right ddl <- ddlAsRelation sessionId dbconn
  --save it back to the db to run tutd on it
  _ <- executeDatabaseContextExpr sessionId dbconn (Assign "x" (ExistingRelation ddl))
  executeTutorialD sessionId dbconn "y := ((x{relation_variables}) ungroup relation_variables){name}"
  rel1 <- executeRelationalExpr sessionId dbconn (RelationVariable "y" ())
  let expected = mkRelationFromList (attributesFromList [Attribute "name" TextAtomType]) (map ((:[]) . TextAtom) ["s","p","sp","true", "false"])
  assertEqual "rv names" expected rel1

testRegisteredQueries :: Test
testRegisteredQueries = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  -- test that an invalid query cannot be added
  Left err <- executeDatabaseContextExpr sessionId dbconn (AddRegisteredQuery "invalid_query" (RelationVariable "x" ()))
  assertEqual "invalid registered query" (RegisteredQueryValidationError "invalid_query" (RelVarNotDefinedError "x")) err
  -- add a query, then test that removing the rv triggers an error
  executeTutorialD sessionId dbconn "x:=relation{tuple{a 5}}"
  Right () <- executeDatabaseContextExpr sessionId dbconn (AddRegisteredQuery "protect_x" (RelationVariable "x" ()))
  Left err' <- executeDatabaseContextExpr sessionId dbconn (Undefine "x")
  assertEqual "prevent x deletion" (RegisteredQueryValidationError "protect_x" (RelVarNotDefinedError "x")) err'
  -- remove the query
  Right () <- executeDatabaseContextExpr sessionId dbconn (RemoveRegisteredQuery "protect_x")
  Right () <- executeDatabaseContextExpr sessionId dbconn (Undefine "x")  
  pure ()
  
testCrossJoin :: Test
testCrossJoin = TestCase $ do
  (session, dbconn) <- dateExamplesConnection emptyNotificationCallback
  -- Athens, London, Paris cross joined with 17, 12, 14, 19
  executeTutorialD session dbconn "x:=(s{city}) join (s{status})"
  eActual <- executeRelationalExpr session dbconn (RelationVariable "x" ())
  let eExpected = mkRelationFromList (attributesFromList [Attribute "city" TextAtomType, Attribute "status" IntegerAtomType]) [[city,status] | city <- map TextAtom ["Athens", "London", "Paris"], status <- map IntegerAtom [30,20,10]]
  assertBool "cross join error" (isRight eActual)
  assertEqual "cross join" eExpected eActual

  executeTutorialD session dbconn "y:=relation{tuple{a 1},tuple{a 2},tuple{a 3}} join relation{tuple{b 4}, tuple{b 5}, tuple{b 6}}"
  let eExpected' = mkRelationFromList (attributesFromList [Attribute "a" IntegerAtomType, Attribute "b" IntegerAtomType]) [[a,b] | a <- map IntegerAtom [1,2,3], b <- map IntegerAtom [4,5,6]]
  eActual' <- executeRelationalExpr session dbconn (RelationVariable "y" ())
  assertBool "cross join 2 error" (isRight eActual')  
  assertEqual "cross join 2" eExpected' eActual'
  
testIfThenExpr :: Test
testIfThenExpr = TestCase $ do
  (session, dbconn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD session dbconn "x:=(s:{islondon:=if eq(@city,\"London\") then True else False}){city,islondon} = relation{tuple{city \"London\", islondon True},tuple{city \"Paris\",islondon False},tuple{city \"Athens\", islondon False}}"
  eEqRel <- executeRelationalExpr session dbconn (RelationVariable "x" ())
  assertEqual "if-then" (Right relationTrue) eEqRel

testSubrelationAttributeAtomExpr :: Test
testSubrelationAttributeAtomExpr = TestCase $ do
  (session, dbconn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD session dbconn "x:=(s group ({all from s} as sub):{l:=sum(@sub.status)}){l}"
  executeTutorialD session dbconn "y:=relation{tuple{l 110}}"
  executeTutorialD session dbconn "z:= x = y"
  res <- executeRelationalExpr session dbconn (RelationVariable "z" ())
  assertEqual "sum" (Right relationTrue) res

testComplexTypeVarResolution :: Test
testComplexTypeVarResolution = TestCase $ do
  (session, dbconn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD session dbconn "data F = F (Maybe Integer) (Maybe Integer)"
  executeTutorialD session dbconn "x:=relation{x F}{tuple{x F Nothing (Just 4)}}"
  executeTutorialD session dbconn "y:=relation{x F}{tuple{x F Nothing Nothing}}"
  
  resX <- executeRelationalExpr session dbconn (RelationVariable "x" ())
  resY <- executeRelationalExpr session dbconn (RelationVariable "y" ())

  
  let expectedX = mkRelationFromList attrs
        [[ConstructedAtom "F"
         (ConstructedAtomType "F" mempty)
          [ConstructedAtom "Nothing" mint [],
            ConstructedAtom "Just" mint [IntegerAtom 4]]]]
      attrs = A.attributesFromList [Attribute "x" (ConstructedAtomType "F" mempty)]
      mint = ConstructedAtomType "Maybe" (M.singleton "a" IntegerAtomType)
  assertEqual "type var resolution x" expectedX resX

  let expectedY = mkRelationFromList attrs
                  [[ConstructedAtom "F"
                    (ConstructedAtomType "F" mempty)
                    [ConstructedAtom "Nothing" mint [],
                     ConstructedAtom "Nothing" mint []]]]
 
  assertEqual "type var resolution y" expectedY resY

testDBCFunctionAccessControl :: Test
testDBCFunctionAccessControl = TestCase $ do
  (session, conn) <- dateExamplesConnection emptyNotificationCallback
  -- add a non-privileged role
  let user1 = "user1"
  executeTutorialD session conn (":addloginrole " <> user1 <> " maynotlogin")
  let user1conn = setRoleName user1 conn  
  -- check that the non-privileged role cannot run the dbc function
  expectTutorialDErr session user1conn ("AccessDeniedError" `T.isInfixOf`) "execute deleteAll()"
  -- grant permission to a dbc function to the role
  executeTutorialD session conn $ "grant dbcfunction " <> user1 <> " deleteAll executefunction nogrant"
  executeTutorialD session conn $ "grant " <> user1 <> " executefunctions nogrant"
  -- check that the non-privileged role can run the dbc function
  executeTutorialD session user1conn "execute deleteAll()"
  
testAttributeTypeHintsFromExistingRelVar :: Test
testAttributeTypeHintsFromExistingRelVar = TestCase $ do
  (session, dbconn) <- dateExamplesConnection emptyNotificationCallback
  -- validate that declaring a relvar and then changing its type fails
  executeTutorialD session dbconn "x:=relation{a Integer}"
  let err1 = "RelationTypeMismatchError"
  expectTutorialDErr session dbconn (T.isPrefixOf err1) "x:=relation{tuple{a 3, b 4}}"


import TutorialD.Interpreter.DatabaseContextExpr
import TutorialD.Interpreter.TestBase
import TutorialD.Interpreter
import TutorialD.Interpreter.Base
import Test.HUnit
import ProjectM36.Relation
import ProjectM36.Tuple
import ProjectM36.TupleSet
import ProjectM36.Error
import ProjectM36.DatabaseContext
import ProjectM36.AtomFunctions.Primitive
import ProjectM36.DataTypes.Either
import ProjectM36.DateExamples
import ProjectM36.Base hiding (Finite)
import ProjectM36.TransactionGraph
import ProjectM36.Client
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
import Data.Text hiding (map)
import qualified Data.Text as T
import Data.Time.Clock.POSIX hiding (getCurrentTime)
import Data.Time.Clock (getCurrentTime)

main :: IO ()
main = do
  tcounts <- runTestTT (TestList tests)
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess
  where
    tests = map (\(tutd, expected) -> TestCase $ assertTutdEqual basicDatabaseContext expected tutd) simpleRelTests ++ map (\(tutd, expected) -> TestCase $ assertTutdEqual dateExamples expected tutd) dateExampleRelTests  ++ [
      transactionGraphBasicTest, 
      transactionGraphAddCommitTest, 
      transactionRollbackTest, 
      transactionJumpTest, 
      transactionBranchTest, 
      simpleJoinTest, 
      testNotification, 
      testTypeConstructors, 
      testMergeTransactions, 
      testComments, 
      testTransGraphRelationalExpr, 
      failJoinTest, 
      testMultiAttributeRename, 
      testSchemaExpr, 
      testRelationalExprStateTupleElems, 
      testFunctionalDependencies, 
      testEmptyCommits,
      testIntervalAtom
      ]
    simpleRelTests = [("x:=true", Right relationTrue),
                      ("x:=false", Right relationFalse),
                      ("x:=true union false", Right relationTrue),
                      ("x:=true minus false", Right relationTrue),
                      ("x:=false minus true", Right relationFalse),                      
                      ("x:=true; x:=false", Right relationFalse),
                      ("x:=relation{a Integer}{}", mkRelation simpleAAttributes emptyTupleSet),
                      ("x:=relation{c Integer}{} rename {c as d}", mkRelation simpleBAttributes emptyTupleSet),
                      ("y:=relation{b Integer, c Integer}{}; x:=y{c}", mkRelation simpleProjectionAttributes emptyTupleSet),
                      ("x:=relation{tuple{a \"spam\", b 5}}", mkRelation simpleCAttributes $ RelationTupleSet [RelationTuple simpleCAttributes (V.fromList [TextAtom "spam", IntegerAtom 5])]),
                      ("constraint failc true in false; x:=true", Left $ InclusionDependencyCheckError "failc"),
                      ("x:=y; x:=true", Left $ RelVarNotDefinedError "y"),
                      ("x:=relation{}{}", Right relationFalse),
                      ("x:=relation{tuple{}}", Right relationTrue),
                      ("x:=true where true", Right relationTrue),
                      ("x:=true where false", Right relationFalse),
                      ("x:=true where true or false", Right relationTrue),
                      ("x:=true where false or false", Right relationFalse),
                      ("x:=true where true and false", Right relationFalse),
                      ("x:=true where true and true", Right relationTrue),
                      ("x:=true=true", Right relationTrue),
                      ("x:=true=false", Right relationFalse),
                      ("x:=true; undefine x", Left (RelVarNotDefinedError "x")),
                      ("x:=relation {b Integer, a Text}{}; insert x relation{tuple{b 5, a \"spam\"}}", mkRelationFromTuples simpleCAttributes [RelationTuple simpleCAttributes $ V.fromList [TextAtom "spam", IntegerAtom 5]]),
                      -- test nested relation constructor
                      ("x:=relation{tuple{a 5, b relation{tuple{a 6}}}}", mkRelation nestedRelationAttributes $ RelationTupleSet [RelationTuple nestedRelationAttributes (V.fromList [IntegerAtom 5, RelationAtom (Relation simpleAAttributes $ RelationTupleSet [RelationTuple simpleAAttributes $ V.fromList [IntegerAtom 6]])])]),
                      ("x:=relation{tuple{b 5,a \"spam\"},tuple{b 6,a \"sam\"}}; delete x where b=6", mkRelation simpleCAttributes $ RelationTupleSet [RelationTuple simpleCAttributes (V.fromList [TextAtom "spam", IntegerAtom 5])]),
                      ("x:=relation{tuple{a 5}} : {b:=@a}", mkRelation simpleDAttributes $ RelationTupleSet [RelationTuple simpleDAttributes (V.fromList [IntegerAtom 5, IntegerAtom 5])]),
                      ("x:=relation{tuple{a 5}} : {b:=6}", mkRelationFromTuples simpleDAttributes [RelationTuple simpleDAttributes (V.fromList [IntegerAtom 5, IntegerAtom 6])]),
                      ("x:=relation{tuple{a 5}} : {b:=add(@a,5)}", mkRelationFromTuples simpleDAttributes [RelationTuple simpleDAttributes (V.fromList [IntegerAtom 5, IntegerAtom 10])]),
                      ("x:=relation{tuple{a 5}} : {b:=add(@a,\"spam\")}", Left (AtomFunctionTypeError "add" 2 IntegerAtomType TextAtomType)),
                      ("x:=relation{tuple{a 5}} : {b:=add(add(@a,2),5)}", mkRelationFromTuples simpleDAttributes [RelationTuple simpleDAttributes (V.fromList [IntegerAtom 5, IntegerAtom 12])])
                     ]
    simpleAAttributes = A.attributesFromList [Attribute "a" IntegerAtomType]
    simpleBAttributes = A.attributesFromList [Attribute "d" IntegerAtomType]
    simpleCAttributes = A.attributesFromList [Attribute "a" TextAtomType, Attribute "b" IntegerAtomType]
    simpleDAttributes = A.attributesFromList [Attribute "a" IntegerAtomType, Attribute "b" IntegerAtomType]
    maybeTextAtomType = ConstructedAtomType "Maybe" (M.singleton "a" TextAtomType)
    maybeIntegerAtomType = ConstructedAtomType "Maybe" (M.singleton "a" IntegerAtomType)
    simpleMaybeTextAttributes = A.attributesFromList [Attribute "a" maybeTextAtomType]
    simpleMaybeIntAttributes = A.attributesFromList [Attribute "a" maybeIntegerAtomType]
    simpleEitherIntTextAttributes = A.attributesFromList [Attribute "a" (eitherAtomType IntegerAtomType TextAtomType)]
    simpleProjectionAttributes = A.attributesFromList [Attribute "c" IntegerAtomType]
    nestedRelationAttributes = A.attributesFromList [Attribute "a" IntegerAtomType, Attribute "b" (RelationAtomType $ A.attributesFromList [Attribute "a" IntegerAtomType])]
    extendTestAttributes = A.attributesFromList [Attribute "a" IntegerAtomType, Attribute "b" $ RelationAtomType (attributes suppliersRel)]
    byteStringAttributes = A.attributesFromList [Attribute "y" ByteStringAtomType]    
    groupCountAttrs = A.attributesFromList [Attribute "z" IntegerAtomType]
    minMaxAttrs = A.attributesFromList [Attribute "s#" TextAtomType, Attribute "z" IntegerAtomType]
    updateParisPlus10 = relMap (\tuple -> do
                                   statusAtom <- atomForAttributeName "status" tuple
                                   cityAtom <- atomForAttributeName "city" tuple
                                   if cityAtom == TextAtom "Paris" then
                                     Right $ updateTupleWithAtoms (M.singleton "status" (IntegerAtom (castInteger statusAtom + 10))) tuple
                                     else Right tuple) suppliersRel
    dateExampleRelTests = [("x:=s where true", Right suppliersRel),
                           ("x:=s where city = \"London\"", restrict (\tuple -> pure $ atomForAttributeName "city" tuple == (Right $ TextAtom "London")) suppliersRel),
                           ("x:=s where false", Right $ Relation (attributes suppliersRel) emptyTupleSet),
                           ("x:=p where color=\"Blue\" and city=\"Paris\"", mkRelationFromList (attributes productsRel) [[TextAtom "P5", TextAtom "Cam", TextAtom "Blue", IntegerAtom 12, TextAtom "Paris"]]),
                           ("a:=s; update a (status:=50); x:=a{status}", mkRelation (A.attributesFromList [Attribute "status" IntegerAtomType]) (RelationTupleSet [mkRelationTuple (A.attributesFromList [Attribute "status" IntegerAtomType]) (V.fromList [IntegerAtom 50])])),
                           ("x:=s minus (s where status=20)", mkRelationFromList (attributes suppliersRel) [[TextAtom "S2", TextAtom "Jones", IntegerAtom 10, TextAtom "Paris"], [TextAtom "S3", TextAtom "Blake", IntegerAtom 30, TextAtom "Paris"], [TextAtom "S5", TextAtom "Adams", IntegerAtom 30, TextAtom "Athens"]]),
                           --atom function tests
                           ("x:=((s : {status2 := add(10,@status)}) where status2=add(10,@status)){city,s#,sname,status}", Right suppliersRel),
                           ("x:=relation{tuple{a 5}} : {b:=s}", mkRelation extendTestAttributes (RelationTupleSet [mkRelationTuple extendTestAttributes (V.fromList [IntegerAtom 5, RelationAtom suppliersRel])])),
                           ("x:=s; update x where sname=\"Blake\" (city:=\"Boston\")", relMap (\tuple -> if atomForAttributeName "sname" tuple == (Right $ TextAtom "Blake") then Right $ updateTupleWithAtoms (M.singleton "city" (TextAtom "Boston")) tuple else Right tuple) suppliersRel),
                           ("x:=s; update x where city=\"Paris\" (status:=add(@status,10))", updateParisPlus10),
                           --relatom function tests
                           ("x:=((s group ({city} as y)):{z:=count(@y)}){z}", mkRelation groupCountAttrs (RelationTupleSet [mkRelationTuple groupCountAttrs (V.singleton $ IntegerAtom 1)])),
                           ("x:=(sp group ({s#} as y)) ungroup y", Right supplierProductsRel),
                           ("x:=((sp{s#,qty}) group ({qty} as x):{z:=max(@x)}){s#,z}", mkRelationFromList minMaxAttrs (map (\(s,i) -> [TextAtom s,IntegerAtom i]) [("S1", 400), ("S2", 400), ("S3", 200), ("S4", 400)])),
                           ("x:=((sp{s#,qty}) group ({qty} as x):{z:=min(@x)}){s#,z}", mkRelationFromList minMaxAttrs (map (\(s,i) -> [TextAtom s,IntegerAtom i]) [("S1", 100), ("S2", 300), ("S3", 200), ("S4", 200)])),
                           ("x:=((sp{s#,qty}) group ({qty} as x):{z:=sum(@x)}){s#,z}", mkRelationFromList minMaxAttrs (map (\(s,i) -> [TextAtom s,IntegerAtom i]) [("S1", 1000), ("S2", 700), ("S3", 200), ("S4", 900)])),
                           --boolean function restriction
                           ("x:=s where ^lt(@status,20)", mkRelationFromList (attributes suppliersRel) [[TextAtom "S2", TextAtom "Jones", IntegerAtom 10, TextAtom "Paris"]]),
                           ("x:=s where ^gt(@status,20)", mkRelationFromList (attributes suppliersRel) [[TextAtom "S3", TextAtom "Blake", IntegerAtom 30, TextAtom "Paris"],
                                                                                                       [TextAtom "S5", TextAtom "Adams", IntegerAtom 30, TextAtom "Athens"]]),
                           ("x:=s where ^sum(@status)", Left $ AtomTypeMismatchError IntegerAtomType BoolAtomType),
                           ("x:=s where ^not(gte(@status,20))", mkRelationFromList (attributes suppliersRel) [[TextAtom "S2", TextAtom "Jones", IntegerAtom 10, TextAtom "Paris"]]),
                           --test "all but" attribute inversion syntax
                           ("x:=s{all but s#} = s{city,sname,status}", Right relationTrue),
                           --test key syntax
                           ("x:=s; key testconstraint {s#,city} x; insert x relation{tuple{city \"London\", s# \"S1\", sname \"gonk\", status 50}}", Left (InclusionDependencyCheckError "testconstraint")),
                           ("y:=s; key testconstraint {s#} y; insert y relation{tuple{city \"London\", s# \"S6\", sname \"gonk\", status 50}}; x:=y{s#} = s{s#} union relation{tuple{s# \"S6\"}}", Right relationTrue),
                           --test binary bytestring data type
                           ("x:=relation{tuple{y bytestring(\"dGVzdGRhdGE=\")}}", mkRelationFromList byteStringAttributes [[ByteStringAtom (TE.encodeUtf8 "testdata")]]),
                           --test Maybe Text
                           ("x:=relation{tuple{a Just \"spam\"}}", mkRelationFromList simpleMaybeTextAttributes [[ConstructedAtom "Just" maybeTextAtomType [TextAtom "spam"]]]),
                           --test Maybe Integer
                           ("x:=relation{tuple{a Just 3}}", mkRelationFromList simpleMaybeIntAttributes [[ConstructedAtom "Just" maybeIntegerAtomType [IntegerAtom 3]]]),
                           --test Either Integer Text
                           ("x:=relation{tuple{a Left 3}}",  Left (TypeConstructorTypeVarsMismatch (S.fromList ["a","b"]) (S.fromList ["a"]))), -- Left 3, alone is not enough information to imply the type
                           ("x:=relation{a Either Integer Text}{tuple{a Left 3}}", mkRelationFromList simpleEitherIntTextAttributes [[ConstructedAtom "Left" (eitherAtomType IntegerAtomType TextAtomType) [IntegerAtom 3]]]),
                           --test datetime constructor
                           ("x:=relation{tuple{a dateTimeFromEpochSeconds(1495199790)}}", mkRelationFromList (A.attributesFromList [Attribute "a" DateTimeAtomType]) [[DateTimeAtom (posixSecondsToUTCTime(realToFrac (1495199790 :: Int)))]])
                          ]

assertTutdEqual :: DatabaseContext -> Either RelationalError Relation -> Text -> Assertion
assertTutdEqual databaseContext expected tutd = assertEqual (unpack tutd) expected interpreted
  where
    interpreted = case interpretDatabaseContextExpr databaseContext tutd of
      Left err -> Left err
      Right context -> case M.lookup "x" (relationVariables context) of
        Nothing -> Left $ RelVarNotDefinedError "x"
        Just rel -> Right rel

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
        DisplayParseErrorResult _ _ -> assertFailure "displayparseerror?"
        DisplayErrorResult err -> assertFailure (show err)   
        QuietSuccessResult -> do
          commit sessionId dbconn >>= eitherFail
          discon <- disconnectedTransaction_ sessionId dbconn
          let context = Discon.concreteDatabaseContext discon
          assertEqual "ensure x was added" (M.lookup "x" (relationVariables context)) (Just suppliersRel)

transactionRollbackTest :: Test
transactionRollbackTest = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  graph <- transactionGraph_ dbconn
  executeDatabaseContextExpr sessionId dbconn (Assign "x" (RelationVariable "s" ())) >>= eitherFail
  rollback sessionId dbconn >>= eitherFail
  discon <- disconnectedTransaction_ sessionId dbconn
  graph' <- transactionGraph_ dbconn
  assertEqual "validate context" Nothing (M.lookup "x" (relationVariables (Discon.concreteDatabaseContext discon)))
  let graphEq graphArg = S.map transactionId (transactionsForGraph graphArg)
  assertEqual "validate graph" (graphEq graph) (graphEq graph')

--commit a new transaction with "x" relation, jump to first transaction, verify that "x" is not present
transactionJumpTest :: Test
transactionJumpTest = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  (DisconnectedTransaction firstUUID _ _) <- disconnectedTransaction_ sessionId dbconn
  executeDatabaseContextExpr sessionId dbconn (Assign "x" (RelationVariable "s" ())) >>= eitherFail
  commit sessionId dbconn >>= eitherFail
  --perform the jump
  executeGraphExpr sessionId dbconn (JumpToTransaction firstUUID) >>= eitherFail
  --check that the disconnected transaction does not include "x"
  discon <- disconnectedTransaction_ sessionId dbconn
  assertEqual "ensure x is not present" Nothing (M.lookup "x" (relationVariables (Discon.concreteDatabaseContext discon)))          
--branch from the first transaction and verify that there are two heads
transactionBranchTest :: Test
transactionBranchTest = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  mapM_ (>>= eitherFail) [executeGraphExpr sessionId dbconn (Branch "test"),
                                  executeDatabaseContextExpr sessionId dbconn (Assign "x" (RelationVariable "s" ())),
                                  commit sessionId dbconn,
                                  executeGraphExpr sessionId dbconn (JumpToHead "master"),
                                  executeDatabaseContextExpr sessionId dbconn (Assign "y" (RelationVariable "s" ()))
                  ]
  graph <- transactionGraph_ dbconn
  assertBool "master branch exists" $ isJust (transactionForHead "master" graph)
  assertBool "test branch exists" $ isJust (transactionForHead "test" graph)

-- test that overlapping attribute names with different types fail with an error
failJoinTest :: Test
failJoinTest = TestCase $ assertTutdEqual basicDatabaseContext err "x:=relation{tuple{test 4}} join relation{tuple{test \"test\"}}"
  where
    err = Left (TupleAttributeTypeMismatchError (A.attributesFromList [Attribute "test" IntegerAtomType]))

simpleJoinTest :: Test
simpleJoinTest = TestCase $ assertTutdEqual dateExamples joinedRel "x:=s join sp"
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
testNotification :: Test
testNotification = TestCase $ do
  notifmvar <- newEmptyMVar
  let notifCallback mvar _ _ = putMVar mvar ()
      relvarx = RelationVariable "x" ()
  (sess, conn) <- dateExamplesConnection (notifCallback notifmvar)
  executeDatabaseContextExpr sess conn (Assign "x" (ExistingRelation relationTrue)) >>= eitherFail
  executeDatabaseContextExpr sess conn (AddNotification "test notification" relvarx relvarx) >>= eitherFail
  commit sess conn >>= eitherFail
  executeDatabaseContextExpr sess conn (Assign "x" (ExistingRelation relationFalse)) >>= eitherFail
  commit sess conn >>= eitherFail
  takeMVar notifmvar
    
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
    ":branch testbranch --test comment\n",
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
  _ <- executeGraphExpr sessionId dbconn (WalkBackToTime now)
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
testMultiAttributeRename = TestCase $ assertTutdEqual dateExamples renamedRel "x:=s rename {city as town, status as price} where false"
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
    ":addschema test (isopassthrough \"true\", isopassthrough \"false\", isorename \"supplier\" \"s\", isorename \"supplier_product\" \"sp\", isounion \"heavy_product\" \"light_product\" \"p\" ^gte(17,@weight))",
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
testRelationalExprStateTupleElems :: Test
testRelationalExprStateTupleElems = TestCase $ do
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
  
  executeTutorialD sessionId dbconn "rv1:=relation{tuple{test 1}}"
  executeTutorialD sessionId dbconn "rv2:=relation{tuple{val 1},tuple{val 2}}"
  --check subexpression evaluation in restriction predicate
  -- "rv1 where ((rv2 where val=@test) {})"
  let correctSubexpr = Restrict (AttributeEqualityPredicate "val" (AttributeAtomExpr "test")) (RelationVariable "rv2" ())
      mainExpr subexpr = Restrict 
                         (RelationalExprPredicate
                          (Project AN.empty subexpr)) (RelationVariable "rv1" ())
  eRv2 <- executeRelationalExpr sessionId dbconn (mainExpr correctSubexpr)
  let expectedRel2 = mkRelationFromList (attributesFromList [Attribute "test" IntegerAtomType]) [[IntegerAtom 1]]
  assertEqual "validate sub-expression attribute" expectedRel2 eRv2
  
  --check error in subexpression
  let wrongSubexpr = Restrict (AttributeEqualityPredicate "nosuchattr" (AttributeAtomExpr "test")) (RelationVariable "rv2" ())
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
  
  --update no tuples
  Right () <- executeDatabaseContextExpr sessionId dbconn (Update "s" (M.singleton "sname" (NakedAtomExpr (TextAtom "Bob"))) (AttributeEqualityPredicate "sname" (NakedAtomExpr (TextAtom "Mike"))))
  dirty'' <- disconnectedTransactionIsDirty sessionId dbconn
  assertEqual "empty update empty commit" (Right False) dirty''
  Right () <- commit sessionId dbconn
  
  --delete no tuples
  Right () <- executeDatabaseContextExpr sessionId dbconn (Delete "s" (AttributeEqualityPredicate "sname" (NakedAtomExpr (TextAtom "Mike"))))
  dirty''' <- disconnectedTransactionIsDirty sessionId dbconn
  assertEqual "empty delete empty commit" (Right False) dirty'''
 
testIntervalAtom :: Test  
testIntervalAtom = TestCase $ do  
  --test interval creation
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback  
  executeTutorialD sessionId dbconn "x:=relation{tuple{n 1, a interval(3,4,f,f), b interval(4,5,f,f)}, tuple{n 2,a interval(3,4,t,t), b interval(4,5,t,t)}}"
  --test failed interval creation
  let err1 = "AtomFunctionUserError InvalidIntervalOrderingError"
      err2 = "AtomFunctionUserError (AtomTypeDoesNotSupportIntervalError \"Text\")"
  expectTutorialDErr sessionId dbconn (T.isPrefixOf err1) "z:=relation{tuple{a interval(4,3,f,f)}}"
  expectTutorialDErr sessionId dbconn (T.isPrefixOf err2) "z:=relation{tuple{a interval(\"s\",\"t\",f,f)}}"  

  --test interval_overlaps
  executeTutorialD sessionId dbconn "y:=x:{c:=interval_overlaps(@a,@b)}"
  eRv <- executeRelationalExpr sessionId dbconn (Project (AttributeNames (S.fromList ["n","c"])) (RelationVariable "y" ()))
  assertEqual "interval overlap check" (mkRelationFromList (attributesFromList [Attribute "c" BoolAtomType,Attribute "n" IntegerAtomType]) [[BoolAtom True, IntegerAtom 1], [BoolAtom False, IntegerAtom 2]]) eRv

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
import qualified ProjectM36.Session as Sess
import qualified ProjectM36.Attribute as A
import qualified Data.Map as M
import System.Exit
import Data.Maybe (isJust)
import qualified Data.Vector as V
import Data.Text.Encoding as TE
import Control.Concurrent.STM
import Control.Concurrent
import qualified STMContainers.Map as STMMap
import qualified Data.Set as S
import Data.Text hiding (map)

main :: IO ()
main = do
  tcounts <- runTestTT (TestList tests)
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess
  where
    tests = map (\(tutd, expected) -> TestCase $ assertTutdEqual basicDatabaseContext expected tutd) simpleRelTests ++ map (\(tutd, expected) -> TestCase $ assertTutdEqual dateExamples expected tutd) dateExampleRelTests ++ [transactionGraphBasicTest, transactionGraphAddCommitTest, transactionRollbackTest, transactionJumpTest, transactionBranchTest, simpleJoinTest, testNotification, testTypeConstructors, testMergeTransactions, testComments, testTransGraphRelationalExpr]
    simpleRelTests = [("x:=true", Right relationTrue),
                      ("x:=false", Right relationFalse),
                      ("x:=true union false", Right relationTrue),
                      ("x:=true minus false", Right relationTrue),
                      ("x:=false minus true", Right relationFalse),                      
                      ("x:=true; x:=false", Right relationFalse),
                      ("x:=relation{a Int}{}", mkRelation simpleAAttributes emptyTupleSet),
                      ("x:=relation{c Int}{} rename {c as d}", mkRelation simpleBAttributes emptyTupleSet),
                      ("y:=relation{b Int, c Int}{}; x:=y{c}", mkRelation simpleProjectionAttributes emptyTupleSet),
                      ("x:=relation{tuple{a \"spam\", b 5}}", mkRelation simpleCAttributes $ RelationTupleSet [(RelationTuple simpleCAttributes) (V.fromList [TextAtom "spam", IntAtom 5])]),
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
                      ("x:=relation {b Int, a Text}{}; insert x relation{tuple{b 5, a \"spam\"}}", mkRelationFromTuples simpleCAttributes [RelationTuple simpleCAttributes $ V.fromList[TextAtom "spam", IntAtom 5]]),
                      -- test nested relation constructor
                      ("x:=relation{tuple{a 5, b relation{tuple{a 6}}}}", mkRelation nestedRelationAttributes $ RelationTupleSet [RelationTuple nestedRelationAttributes (V.fromList [IntAtom 5, RelationAtom (Relation simpleAAttributes $ RelationTupleSet [RelationTuple simpleAAttributes $ V.fromList [IntAtom 6]])])]),
                      ("x:=relation{tuple{b 5,a \"spam\"},tuple{b 6,a \"sam\"}}; delete x where b=6", mkRelation simpleCAttributes $ RelationTupleSet [RelationTuple simpleCAttributes (V.fromList [TextAtom "spam", IntAtom 5])]),
                      ("x:=relation{tuple{a 5}} : {b:=@a}", mkRelation simpleDAttributes $ RelationTupleSet [RelationTuple simpleDAttributes (V.fromList [IntAtom 5, IntAtom 5])]),
                      ("x:=relation{tuple{a 5}} : {b:=6}", mkRelationFromTuples simpleDAttributes [RelationTuple simpleDAttributes (V.fromList [IntAtom 5, IntAtom 6])]),
                      ("x:=relation{tuple{a 5}} : {b:=add(@a,5)}", mkRelationFromTuples simpleDAttributes [RelationTuple simpleDAttributes (V.fromList [IntAtom 5, IntAtom 10])]),
                      ("x:=relation{tuple{a 5}} : {b:=add(@a,\"spam\")}", Left (AtomFunctionTypeError "add" 2 IntAtomType TextAtomType)),
                      ("x:=relation{tuple{a 5}} : {b:=add(add(@a,2),5)}", mkRelationFromTuples simpleDAttributes [RelationTuple simpleDAttributes (V.fromList [IntAtom 5, IntAtom 12])])
                     ]
    simpleAAttributes = A.attributesFromList [Attribute "a" IntAtomType]
    simpleBAttributes = A.attributesFromList [Attribute "d" IntAtomType]
    simpleCAttributes = A.attributesFromList [Attribute "a" TextAtomType, Attribute "b" IntAtomType]
    simpleDAttributes = A.attributesFromList [Attribute "a" IntAtomType, Attribute "b" IntAtomType]
    maybeTextAtomType = ConstructedAtomType "Maybe" (M.singleton "a" TextAtomType)
    maybeIntAtomType = ConstructedAtomType "Maybe" (M.singleton "a" IntAtomType)
    simpleMaybeTextAttributes = A.attributesFromList [Attribute "a" maybeTextAtomType]
    simpleMaybeIntAttributes = A.attributesFromList [Attribute "a" maybeIntAtomType]
    simpleEitherIntTextAttributes = A.attributesFromList [Attribute "a" (eitherAtomType IntAtomType TextAtomType)]
    simpleProjectionAttributes = A.attributesFromList [Attribute "c" IntAtomType]
    nestedRelationAttributes = A.attributesFromList [Attribute "a" IntAtomType, Attribute "b" (RelationAtomType $ A.attributesFromList [Attribute "a" IntAtomType])]
    extendTestAttributes = A.attributesFromList [Attribute "a" IntAtomType, Attribute "b" $ RelationAtomType (attributes suppliersRel)]
    byteStringAttributes = A.attributesFromList [Attribute "y" ByteStringAtomType]    
    groupCountAttrs = A.attributesFromList [Attribute "z" IntAtomType]
    minMaxAttrs = A.attributesFromList [Attribute "s#" TextAtomType, Attribute "z" IntAtomType]
    updateParisPlus10 = relMap (\tuple -> do
                                   statusAtom <- atomForAttributeName "status" tuple
                                   cityAtom <- atomForAttributeName "city" tuple
                                   if cityAtom == TextAtom "Paris" then
                                     Right $ updateTupleWithAtoms (M.singleton "status" (IntAtom ((castInt statusAtom) + 10))) tuple
                                     else Right tuple) suppliersRel
    dateExampleRelTests = [("x:=s where true", Right suppliersRel),
                           ("x:=s where city = \"London\"", restrict (\tuple -> atomForAttributeName "city" tuple == (Right $ TextAtom "London")) suppliersRel),
                           ("x:=s where false", Right $ Relation (attributes suppliersRel) emptyTupleSet),
                           ("x:=p where color=\"Blue\" and city=\"Paris\"", mkRelationFromList (attributes productsRel) [[TextAtom "P5", TextAtom "Cam", TextAtom "Blue", IntAtom 12, TextAtom "Paris"]]),
                           ("a:=s; update a (status:=50); x:=a{status}", mkRelation (A.attributesFromList [Attribute "status" IntAtomType]) (RelationTupleSet [mkRelationTuple (A.attributesFromList [Attribute "status" IntAtomType]) (V.fromList [IntAtom 50])])),
                           ("x:=s minus (s where status=20)", mkRelationFromList (attributes suppliersRel) [[TextAtom "S2", TextAtom "Jones", IntAtom 10, TextAtom "Paris"], [TextAtom "S3", TextAtom "Blake", IntAtom 30, TextAtom "Paris"], [TextAtom "S5", TextAtom "Adams", IntAtom 30, TextAtom "Athens"]]),
                           --atom function tests
                           ("x:=((s : {status2 := add(10,@status)}) where status2=add(10,@status)){city,s#,sname,status}", Right suppliersRel),
                           ("x:=relation{tuple{a 5}} : {b:=s}", mkRelation extendTestAttributes (RelationTupleSet [mkRelationTuple extendTestAttributes (V.fromList [IntAtom 5, RelationAtom suppliersRel])])),
                           ("x:=s; update x where sname=\"Blake\" (city:=\"Boston\")", relMap (\tuple -> if atomForAttributeName "sname" tuple == (Right $ TextAtom "Blake") then Right $ updateTupleWithAtoms (M.singleton "city" (TextAtom "Boston")) tuple else Right tuple) suppliersRel),
                           ("x:=s; update x where city=\"Paris\" (status:=add(@status,10))", updateParisPlus10),
                           --relatom function tests
                           ("x:=((s group ({city} as y)):{z:=count(@y)}){z}", mkRelation groupCountAttrs (RelationTupleSet [mkRelationTuple groupCountAttrs (V.singleton $ IntAtom 1)])),
                           ("x:=(sp group ({s#} as y)) ungroup y", Right supplierProductsRel),
                           ("x:=((sp{s#,qty}) group ({qty} as x):{z:=max(@x)}){s#,z}", mkRelationFromList minMaxAttrs (map (\(s,i) -> [TextAtom s,IntAtom i]) [("S1", 400), ("S2", 400), ("S3", 200), ("S4", 400)])),
                           ("x:=((sp{s#,qty}) group ({qty} as x):{z:=min(@x)}){s#,z}", mkRelationFromList minMaxAttrs (map (\(s,i) -> [TextAtom s,IntAtom i]) [("S1", 100), ("S2", 300), ("S3", 200), ("S4", 200)])),
                           ("x:=((sp{s#,qty}) group ({qty} as x):{z:=sum(@x)}){s#,z}", mkRelationFromList minMaxAttrs (map (\(s,i) -> [TextAtom s,IntAtom i]) [("S1", 1000), ("S2", 700), ("S3", 200), ("S4", 900)])),
                           --boolean function restriction
                           ("x:=s where ^lt(@status,20)", mkRelationFromList (attributes suppliersRel) [[TextAtom "S2", TextAtom "Jones", IntAtom 10, TextAtom "Paris"]]),
                           ("x:=s where ^gt(@status,20)", mkRelationFromList (attributes suppliersRel) [[TextAtom "S3", TextAtom "Blake", IntAtom 30, TextAtom "Paris"],
                                                                                                       [TextAtom "S5", TextAtom "Adams", IntAtom 30, TextAtom "Athens"]]),
                           ("x:=s where ^sum(@status)", Left $ AtomTypeMismatchError IntAtomType BoolAtomType),
                           ("x:=s where ^not(gte(@status,20))", mkRelationFromList (attributes suppliersRel) [[TextAtom "S2", TextAtom "Jones", IntAtom 10, TextAtom "Paris"]]),
                           --test "all but" attribute inversion syntax
                           ("x:=s{all but s#} = s{city,sname,status}", Right $ relationTrue),
                           --test key syntax
                           ("x:=s; key testconstraint {s#,city} x; insert x relation{tuple{city \"London\", s# \"S1\", sname \"gonk\", status 50}}", Left (InclusionDependencyCheckError "testconstraint")),
                           ("y:=s; key testconstraint {s#} y; insert y relation{tuple{city \"London\", s# \"S6\", sname \"gonk\", status 50}}; x:=y{s#} = s{s#} union relation{tuple{s# \"S6\"}}", Right $ relationTrue),
                           --test binary bytestring data type
                           ("x:=relation{tuple{y makeByteString(\"dGVzdGRhdGE=\")}}", mkRelationFromList byteStringAttributes [[ByteStringAtom (TE.encodeUtf8 "testdata")]]),
                           --test Maybe Text
                           ("x:=relation{tuple{a Just \"spam\"}}", mkRelationFromList simpleMaybeTextAttributes [[ConstructedAtom "Just" maybeTextAtomType [TextAtom "spam"]]]),
                           --test Maybe Int
                           ("x:=relation{tuple{a Just 3}}", mkRelationFromList simpleMaybeIntAttributes [[ConstructedAtom "Just" maybeIntAtomType [IntAtom 3]]]),
                           --test Either Int Text
                           ("x:=relation{tuple{a Left 3}}",  Left (TypeConstructorTypeVarsMismatch (S.fromList ["a","b"]) (S.fromList ["a"]))), -- Left 3, alone is not enough information to imply the type
                           ("x:=relation{a Either Int Text}{tuple{a Left 3}}", mkRelationFromList simpleEitherIntTextAttributes [[ConstructedAtom "Left" (eitherAtomType IntAtomType TextAtomType) [IntAtom 3]]])
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
  graph <- transactionGraph dbconn
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
          commit sessionId dbconn >>= maybeFail
          discon <- disconnectedTransaction sessionId dbconn
          let context = Discon.concreteDatabaseContext discon
          assertEqual "ensure x was added" (M.lookup "x" (relationVariables context)) (Just suppliersRel)

transactionRollbackTest :: Test
transactionRollbackTest = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  graph <- transactionGraph dbconn
  maybeErr <- executeDatabaseContextExpr sessionId dbconn (Assign "x" (RelationVariable "s" ()))
  case maybeErr of
    Just err -> assertFailure (show err)
    Nothing -> do
      rollback sessionId dbconn >>= maybeFail
      discon <- disconnectedTransaction sessionId dbconn
      graph' <- transactionGraph dbconn
      assertEqual "validate context" Nothing (M.lookup "x" (relationVariables (Discon.concreteDatabaseContext discon)))
      let graphEq graphArg = S.map transactionId (transactionsForGraph graphArg)
      assertEqual "validate graph" (graphEq graph) (graphEq graph')

--commit a new transaction with "x" relation, jump to first transaction, verify that "x" is not present
transactionJumpTest :: Test
transactionJumpTest = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  (DisconnectedTransaction firstUUID _) <- disconnectedTransaction sessionId dbconn
  maybeErr <- executeDatabaseContextExpr sessionId dbconn (Assign "x" (RelationVariable "s" ()))
  case maybeErr of
    Just err -> assertFailure (show err)
    Nothing -> do
      commit sessionId dbconn >>= maybeFail
      --perform the jump
      maybeErr2 <- executeGraphExpr sessionId dbconn (JumpToTransaction firstUUID)
      case maybeErr2 of
        Just err -> assertFailure (show err)
        Nothing -> do
          --check that the disconnected transaction does not include "x"
          discon <- disconnectedTransaction sessionId dbconn
          assertEqual "ensure x is not present" Nothing (M.lookup "x" (relationVariables (Discon.concreteDatabaseContext discon)))          
--branch from the first transaction and verify that there are two heads
transactionBranchTest :: Test
transactionBranchTest = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  mapM_ (\x -> x >>= maybeFail) [executeGraphExpr sessionId dbconn (Branch "test"),
                  executeDatabaseContextExpr sessionId dbconn (Assign "x" (RelationVariable "s" ())),
                  commit sessionId dbconn,
                  executeGraphExpr sessionId dbconn (JumpToHead "master"),
                  executeDatabaseContextExpr sessionId dbconn (Assign "y" (RelationVariable "s" ()))
                  ]
  graph <- transactionGraph dbconn
  assertBool "master branch exists" $ isJust (transactionForHead "master" graph)
  assertBool "test branch exists" $ isJust (transactionForHead "test" graph)

simpleJoinTest :: Test
simpleJoinTest = TestCase $ assertTutdEqual dateExamples joinedRel "x:=s join sp"
    where
        attrs = A.attributesFromList [Attribute "city" TextAtomType,
                                      Attribute "qty" IntAtomType,
                                      Attribute "p#" TextAtomType,
                                      Attribute "s#" TextAtomType,
                                      Attribute "sname" TextAtomType,
                                      Attribute "status" IntAtomType]
        joinedRel = mkRelationFromList attrs [[TextAtom "London", IntAtom 100, TextAtom "P6", TextAtom "S1", TextAtom "Smith", IntAtom 20],
                                              [TextAtom "London", IntAtom 400, TextAtom "P3", TextAtom "S1", TextAtom "Smith", IntAtom 20],
                                              [TextAtom "London", IntAtom 400, TextAtom "P5", TextAtom "S4", TextAtom "Clark", IntAtom 20],
                                              [TextAtom "London", IntAtom 300, TextAtom "P1", TextAtom "S1", TextAtom "Smith", IntAtom 20],
                                              [TextAtom "Paris", IntAtom 200, TextAtom "P2", TextAtom "S3", TextAtom "Blake", IntAtom 30],
                                              [TextAtom "Paris", IntAtom 300, TextAtom "P1", TextAtom "S2", TextAtom "Jones", IntAtom 10],
                                              [TextAtom "London", IntAtom 100, TextAtom "P5", TextAtom "S1", TextAtom "Smith", IntAtom 20],
                                              [TextAtom "London", IntAtom 300, TextAtom "P4", TextAtom "S4", TextAtom "Clark", IntAtom 20],
                                              [TextAtom "Paris", IntAtom 400, TextAtom "P2", TextAtom "S2", TextAtom "Jones", IntAtom 10],
                                              [TextAtom "London", IntAtom 200, TextAtom "P2", TextAtom "S1", TextAtom "Smith", IntAtom 20],
                                              [TextAtom "London", IntAtom 200, TextAtom "P4", TextAtom "S1", TextAtom "Smith", IntAtom 20],
                                              [TextAtom "London", IntAtom 200, TextAtom "P2", TextAtom "S4", TextAtom "Clark", IntAtom 20]
                                              ]
transactionGraph :: Connection -> IO TransactionGraph
transactionGraph (InProcessConnection _ _ _ tvar _) = atomically $ readTVar tvar
transactionGraph _ = error "remote connection used"

disconnectedTransaction :: SessionId -> Connection -> IO DisconnectedTransaction
disconnectedTransaction sessionId (InProcessConnection _ _ sessions _ _) = do
  mSession <- atomically $ do
    STMMap.lookup sessionId sessions
  case mSession of
    Nothing -> error "No such session"
    Just (Sess.Session discon _) -> pure discon
disconnectedTransaction _ _ = error "remote connection used"

{-
inclusionDependencies :: Connection -> M.Map IncDepName InclusionDependency
inclusionDependencies (InProcessConnection (DisconnectedTransaction _ context)) = inclusionDependencies context
inclusionDependencies _ = error "remote connection used"                       
-}
                           
-- test notifications over the InProcessConnection
testNotification :: Test
testNotification = TestCase $ do
  notifmvar <- newEmptyMVar
  let notifCallback mvar = \_ _ -> putMVar mvar ()
      relvarx = RelationVariable "x" ()
  (sess, conn) <- dateExamplesConnection (notifCallback notifmvar)
  let check' x = x >>= maybe (pure ()) (\err -> assertFailure (show err))  
  check' $ executeDatabaseContextExpr sess conn (Assign "x" (ExistingRelation relationTrue))
  check' $ executeDatabaseContextExpr sess conn (AddNotification "test notification" relvarx relvarx)  
  check' $ commit sess conn
  check' $ executeDatabaseContextExpr sess conn (Assign "x" (ExistingRelation relationFalse))
  check' $ commit sess conn
  takeMVar notifmvar
    
testTypeConstructors :: Test
testTypeConstructors = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  executeTutorialD sessionId dbconn "data Hair = Color Text | Bald | UserRefusesToSpecify"
  executeTutorialD sessionId dbconn "x:=relation{a Hair}{tuple{a Color \"Blonde\"},tuple{a Bald},tuple{a UserRefusesToSpecify}}"
  executeTutorialD sessionId dbconn "data Tree a = Node a (Tree a) (Tree a) | EmptyNode"
  executeTutorialD sessionId dbconn "y:=relation{a Tree Int}{tuple{a Node 3 (Node 4 EmptyNode EmptyNode) EmptyNode},tuple{a Node 4 EmptyNode EmptyNode}}"
  
testMergeTransactions :: Test
testMergeTransactions = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  mapM_ (executeTutorialD sessionId dbconn) [
   ":branch branchA",
   "conflictrv := relation{conflict Int}{tuple{conflict 1}}",
   ":commit",
   ":jumphead master",
   ":branch branchB",
   "conflictrv := relation{conflict Int}{tuple{conflict 2}}",
   ":commit",
   ":mergetrans union branchA branchB"
   ]
  case mkRelationFromList (attributesFromList [Attribute "conflict" IntAtomType]) [[IntAtom 1],[IntAtom 2]] of
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
                                   Attribute "status" IntAtomType]
      expectedRel = mkRelationFromList sattrs [[TextAtom "Boston",
                                                TextAtom "Smithers",
                                                TextAtom "S9",
                                                IntAtom 50]]
  diff <- executeTransGraphRelationalExpr sessionId dbconn (Difference (RelationVariable "s" testBranchMarker) (RelationVariable "s" masterMarker))
  assertEqual "difference in s" expectedRel diff 
  
  --test graph traversal (head backtracking
  let testBranchBacktrack = TransactionIdHeadNameLookup "testbranch" [TransactionIdHeadParentBacktrack 1]
  backtrackRel <- executeTransGraphRelationalExpr sessionId dbconn (Equals (RelationVariable "s" testBranchBacktrack) (RelationVariable "s" masterMarker))
  assertEqual "backtrack to master" (Right relationTrue) backtrackRel
  
{-# LANGUAGE OverloadedStrings #-}
import TutorialD.Interpreter.DatabaseContextExpr
import TutorialD.Interpreter
import TutorialD.Interpreter.Base
import Test.HUnit
import ProjectM36.Relation
import ProjectM36.Tuple
import ProjectM36.TupleSet
import ProjectM36.Error
import ProjectM36.DatabaseContext
import ProjectM36.DataTypes.Either
import ProjectM36.DataTypes.Primitive
import ProjectM36.DateExamples
import ProjectM36.Base hiding (Finite)
import qualified ProjectM36.Base as Base
import ProjectM36.TransactionGraph
import ProjectM36.Client
import ProjectM36.Atom
import ProjectM36.Session
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

main :: IO ()
main = do
  tcounts <- runTestTT (TestList tests)
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess
  where
    tests = map (\(tutd, expected) -> TestCase $ assertTutdEqual basicDatabaseContext expected tutd) simpleRelTests ++ map (\(tutd, expected) -> TestCase $ assertTutdEqual dateExamples expected tutd) dateExampleRelTests ++ [transactionGraphBasicTest, transactionGraphAddCommitTest, transactionRollbackTest, transactionJumpTest, transactionBranchTest, simpleJoinTest, testNotification, testTypeConstructors, testMergeTransactions]
    simpleRelTests = [("x:=true", Right relationTrue),
                      ("x:=false", Right relationFalse),
                      ("x:=true union false", Right relationTrue),
                      ("x:=true minus false", Right relationTrue),
                      ("x:=false minus true", Right relationFalse),                      
                      ("x:=true; x:=false", Right relationFalse),
                      ("x:=relation{a Int}{}", mkRelation simpleAAttributes emptyTupleSet),
                      ("x:=relation{c Int}{} rename {c as d}", mkRelation simpleBAttributes emptyTupleSet),
                      ("y:=relation{b Int, c Int}{}; x:=y{c}", mkRelation simpleProjectionAttributes emptyTupleSet),
                      ("x:=relation{tuple{a \"spam\", b 5}}", mkRelation simpleCAttributes $ RelationTupleSet [(RelationTuple simpleCAttributes) (V.fromList [textAtom "spam", intAtom 5])]),
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
                      ("x:=relation {b Int, a Text}{}; insert x relation{tuple{b 5, a \"spam\"}}", mkRelationFromTuples simpleCAttributes [RelationTuple simpleCAttributes $ V.fromList[textAtom "spam", intAtom 5]]),
                      -- test nested relation constructor
                      ("x:=relation{tuple{a 5, b relation{tuple{a 6}}}}", mkRelation nestedRelationAttributes $ RelationTupleSet [RelationTuple nestedRelationAttributes (V.fromList [intAtom 5, Atom (Relation simpleAAttributes $ RelationTupleSet [RelationTuple simpleAAttributes $ V.fromList [intAtom 6]])])]),
                      ("x:=relation{tuple{b 5,a \"spam\"},tuple{b 6,a \"sam\"}}; delete x where b=6", mkRelation simpleCAttributes $ RelationTupleSet [RelationTuple simpleCAttributes (V.fromList [textAtom "spam", intAtom 5])]),
                      ("x:=relation{tuple{a 5}} : {b:=@a}", mkRelation simpleDAttributes $ RelationTupleSet [RelationTuple simpleDAttributes (V.fromList [intAtom 5, intAtom 5])]),
                      ("x:=relation{tuple{a 5}} : {b:=6}", mkRelationFromTuples simpleDAttributes [RelationTuple simpleDAttributes (V.fromList [intAtom 5, intAtom 6])]),
                      ("x:=relation{tuple{a 5}} : {b:=add(@a,5)}", mkRelationFromTuples simpleDAttributes [RelationTuple simpleDAttributes (V.fromList [intAtom 5, intAtom 10])]),
                      ("x:=relation{tuple{a 5}} : {b:=add(@a,\"spam\")}", Left (AtomFunctionTypeError "add" 2 intAtomType textAtomType)),
                      ("x:=relation{tuple{a 5}} : {b:=add(add(@a,2),5)}", mkRelationFromTuples simpleDAttributes [RelationTuple simpleDAttributes (V.fromList [intAtom 5, intAtom 12])])
                     ]
    simpleAAttributes = A.attributesFromList [Attribute "a" intAtomType]
    simpleBAttributes = A.attributesFromList [Attribute "d" intAtomType]
    simpleCAttributes = A.attributesFromList [Attribute "a" textAtomType, Attribute "b" intAtomType]
    simpleDAttributes = A.attributesFromList [Attribute "a" intAtomType, Attribute "b" intAtomType]
    maybeTextAtomType = ConstructedAtomType "Maybe" (M.singleton "a" textAtomType)
    maybeIntAtomType = ConstructedAtomType "Maybe" (M.singleton "a" intAtomType)
    simpleMaybeTextAttributes = A.attributesFromList [Attribute "a" maybeTextAtomType]
    simpleMaybeIntAttributes = A.attributesFromList [Attribute "a" maybeIntAtomType]
    simpleEitherIntTextAttributes = A.attributesFromList [Attribute "a" (eitherAtomType intAtomType textAtomType)]
    simpleProjectionAttributes = A.attributesFromList [Attribute "c" intAtomType]
    nestedRelationAttributes = A.attributesFromList [Attribute "a" intAtomType, Attribute "b" (RelationAtomType $ A.attributesFromList [Attribute "a" intAtomType])]
    extendTestAttributes = A.attributesFromList [Attribute "a" intAtomType, Attribute "b" $ RelationAtomType (attributes suppliersRel)]
    byteStringAttributes = A.attributesFromList [Attribute "y" byteStringAtomType]    
    groupCountAttrs = A.attributesFromList [Attribute "z" intAtomType]
    minMaxAttrs = A.attributesFromList [Attribute "s#" textAtomType, Attribute "z" intAtomType]
    dateExampleRelTests = [("x:=s where true", Right suppliersRel),
                           ("x:=s where city = \"London\"", restrict (\tuple -> atomForAttributeName "city" tuple == (Right $ textAtom "London")) suppliersRel),
                           ("x:=s where false", Right $ Relation (attributes suppliersRel) emptyTupleSet),
                           ("x:=p where color=\"Blue\" and city=\"Paris\"", mkRelationFromList (attributes productsRel) [[textAtom "P5", textAtom "Cam", textAtom "Blue", intAtom 12, textAtom "Paris"]]),
                           ("a:=s; update a (status:=50); x:=a{status}", mkRelation (A.attributesFromList [Attribute "status" intAtomType]) (RelationTupleSet [mkRelationTuple (A.attributesFromList [Attribute "status" intAtomType]) (V.fromList [intAtom 50])])),
                           ("x:=s minus (s where status=20)", mkRelationFromList (attributes suppliersRel) [[textAtom "S2", textAtom "Jones", intAtom 10, textAtom "Paris"], [textAtom "S3", textAtom "Blake", intAtom 30, textAtom "Paris"], [textAtom "S5", textAtom "Adams", intAtom 30, textAtom "Athens"]]),
                           --atom function tests
                           ("x:=((s : {status2 := add(10,@status)}) where status2=add(10,@status)){city,s#,sname,status}", Right suppliersRel),
                           ("x:=s; update x where sname=\"Blake\" (city:=\"Boston\")", relMap (\tuple -> if atomForAttributeName "sname" tuple == (Right $ textAtom "Blake") then updateTuple (M.singleton "city" (textAtom "Boston")) tuple else tuple) suppliersRel),
                           ("x:=relation{tuple{a 5}} : {b:=s}", mkRelation extendTestAttributes (RelationTupleSet [mkRelationTuple extendTestAttributes (V.fromList [intAtom 5, Atom suppliersRel])])),
                           --relatom function tests
                           ("x:=((s group ({city} as y)):{z:=count(@y)}){z}", mkRelation groupCountAttrs (RelationTupleSet [mkRelationTuple groupCountAttrs (V.singleton $ intAtom 1)])),
                           ("x:=(sp group ({s#} as y)) ungroup y", Right supplierProductsRel),
                           ("x:=((sp{s#,qty}) group ({qty} as x):{z:=max(@x)}){s#,z}", mkRelationFromList minMaxAttrs (map (\(s,i) -> [textAtom s,intAtom i]) [("S1", 400), ("S2", 400), ("S3", 200), ("S4", 400)])),
                           ("x:=((sp{s#,qty}) group ({qty} as x):{z:=min(@x)}){s#,z}", mkRelationFromList minMaxAttrs (map (\(s,i) -> [textAtom s,intAtom i]) [("S1", 100), ("S2", 300), ("S3", 200), ("S4", 200)])),
                           ("x:=((sp{s#,qty}) group ({qty} as x):{z:=sum(@x)}){s#,z}", mkRelationFromList minMaxAttrs (map (\(s,i) -> [textAtom s,intAtom i]) [("S1", 1000), ("S2", 700), ("S3", 200), ("S4", 900)])),
                           --boolean function restriction
                           ("x:=s where ^lt(@status,20)", mkRelationFromList (attributes suppliersRel) [[textAtom "S2", textAtom "Jones", intAtom 10, textAtom "Paris"]]),
                           ("x:=s where ^gt(@status,20)", mkRelationFromList (attributes suppliersRel) [[textAtom "S3", textAtom "Blake", intAtom 30, textAtom "Paris"],
                                                                                                       [textAtom "S5", textAtom "Adams", intAtom 30, textAtom "Athens"]]),
                           ("x:=s where ^sum(@status)", Left $ AtomTypeMismatchError intAtomType boolAtomType),
                           ("x:=s where ^not(gte(@status,20))", mkRelationFromList (attributes suppliersRel) [[textAtom "S2", textAtom "Jones", intAtom 10, textAtom "Paris"]]),
                           --test "all but" attribute inversion syntax
                           ("x:=s{all but s#} = s{city,sname,status}", Right $ relationTrue),
                           --test key syntax
                           ("x:=s; key testconstraint {s#,city} x; insert x relation{tuple{city \"London\", s# \"S1\", sname \"gonk\", status 50}}", Left (InclusionDependencyCheckError "testconstraint")),
                           ("y:=s; key testconstraint {s#} y; insert y relation{tuple{city \"London\", s# \"S6\", sname \"gonk\", status 50}}; x:=y{s#} = s{s#} union relation{tuple{s# \"S6\"}}", Right $ relationTrue),
                           --test binary bytestring data type
                           ("x:=relation{tuple{y makeByteString(\"dGVzdGRhdGE=\")}}", mkRelationFromList byteStringAttributes [[Atom (TE.encodeUtf8 "testdata")]]),
                           --test Maybe Text
                           ("x:=relation{tuple{a Just \"spam\"}}", mkRelationFromList simpleMaybeTextAttributes [[ConstructedAtom "Just" maybeTextAtomType [textAtom "spam"]]]),
                           --test Maybe Int
                           ("x:=relation{tuple{a Just 3}}", mkRelationFromList simpleMaybeIntAttributes [[ConstructedAtom "Just" maybeIntAtomType [intAtom 3]]]),
                           --test Either Int Text
                           ("x:=relation{tuple{a Left 3}}",  Left (TypeConstructorTypeVarsMismatch (S.fromList ["a","b"]) (S.fromList ["a"]))), -- Left 3, alone is not enough information to imply the type
                           ("x:=relation{a Either Int Text}{tuple{a Left 3}}", mkRelationFromList simpleEitherIntTextAttributes [[ConstructedAtom "Left" (eitherAtomType intAtomType textAtomType) [intAtom 3]]])           
                          ]

assertTutdEqual :: DatabaseContext -> Either RelationalError Relation -> String -> Assertion
assertTutdEqual databaseContext expected tutd = assertEqual tutd expected interpreted
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
        DisplayRelationresult -> assertFailure "displayrelation?"
        DisplayErrorResult err -> assertFailure (show err)        
        QuietSuccessResult -> do
          commit sessionId dbconn >>= maybeFail
          (DisconnectedTransaction _ context) <- disconnectedTransaction sessionId dbconn
          assertEqual "ensure x was added" (M.lookup "x" (relationVariables context)) (Just suppliersRel)

transactionRollbackTest :: Test
transactionRollbackTest = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  graph <- transactionGraph dbconn
  maybeErr <- executeDatabaseContextExpr sessionId dbconn (Assign "x" (RelationVariable "s"))
  case maybeErr of
    Just err -> assertFailure (show err)
    Nothing -> do
      rollback sessionId dbconn >>= maybeFail
      (DisconnectedTransaction _ context') <- disconnectedTransaction sessionId dbconn
      graph' <- transactionGraph dbconn
      assertEqual "validate context" Nothing (M.lookup "x" (relationVariables context'))
      assertEqual "validate graph" graph graph'

--commit a new transaction with "x" relation, jump to first transaction, verify that "x" is not present
transactionJumpTest :: Test
transactionJumpTest = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  (DisconnectedTransaction firstUUID _) <- disconnectedTransaction sessionId dbconn
  maybeErr <- executeDatabaseContextExpr sessionId dbconn (Assign "x" (RelationVariable "s"))
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
          (DisconnectedTransaction _ context') <- disconnectedTransaction sessionId dbconn
          assertEqual "ensure x is not present" Nothing (M.lookup "x" (relationVariables context'))          

maybeFail :: (Show a) => Maybe a -> IO ()
maybeFail (Just err) = assertFailure (show err)
maybeFail Nothing = return ()

--branch from the first transaction and verify that there are two heads
transactionBranchTest :: Test
transactionBranchTest = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  mapM_ (\x -> x >>= maybeFail) [executeGraphExpr sessionId dbconn (Branch "test"),
                  executeDatabaseContextExpr sessionId dbconn (Assign "x" (RelationVariable "s")),
                  commit sessionId dbconn,
                  executeGraphExpr sessionId dbconn (JumpToHead "master"),
                  executeDatabaseContextExpr sessionId dbconn (Assign "y" (RelationVariable "s"))
                  ]
  graph <- transactionGraph dbconn
  assertBool "master branch exists" $ isJust (transactionForHead "master" graph)
  assertBool "test branch exists" $ isJust (transactionForHead "test" graph)

simpleJoinTest :: Test
simpleJoinTest = TestCase $ assertTutdEqual dateExamples joinedRel "x:=s join sp"
    where
        attrs = A.attributesFromList [Attribute "city" textAtomType,
                                      Attribute "qty" intAtomType,
                                      Attribute "p#" textAtomType,
                                      Attribute "s#" textAtomType,
                                      Attribute "sname" textAtomType,
                                      Attribute "status" intAtomType]
        joinedRel = mkRelationFromList attrs [[textAtom "London", intAtom 100, textAtom "P6", textAtom "S1", textAtom "Smith", intAtom 20],
                                              [textAtom "London", intAtom 400, textAtom "P3", textAtom "S1", textAtom "Smith", intAtom 20],
                                              [textAtom "London", intAtom 400, textAtom "P5", textAtom "S4", textAtom "Clark", intAtom 20],
                                              [textAtom "London", intAtom 300, textAtom "P1", textAtom "S1", textAtom "Smith", intAtom 20],
                                              [textAtom "Paris", intAtom 200, textAtom "P2", textAtom "S3", textAtom "Blake", intAtom 30],
                                              [textAtom "Paris", intAtom 300, textAtom "P1", textAtom "S2", textAtom "Jones", intAtom 10],
                                              [textAtom "London", intAtom 100, textAtom "P5", textAtom "S1", textAtom "Smith", intAtom 20],
                                              [textAtom "London", intAtom 300, textAtom "P4", textAtom "S4", textAtom "Clark", intAtom 20],
                                              [textAtom "Paris", intAtom 400, textAtom "P2", textAtom "S2", textAtom "Jones", intAtom 10],
                                              [textAtom "London", intAtom 200, textAtom "P2", textAtom "S1", textAtom "Smith", intAtom 20],
                                              [textAtom "London", intAtom 200, textAtom "P4", textAtom "S1", textAtom "Smith", intAtom 20],
                                              [textAtom "London", intAtom 200, textAtom "P2", textAtom "S4", textAtom "Clark", intAtom 20]
                                              ]
transactionGraph :: Connection -> IO TransactionGraph
transactionGraph (InProcessConnection _ _ _ tvar) = atomically $ readTVar tvar
transactionGraph _ = error "remote connection used"

disconnectedTransaction :: SessionId -> Connection -> IO DisconnectedTransaction
disconnectedTransaction sessionId (InProcessConnection _ _ sessions _) = do
  mSession <- atomically $ do
    STMMap.lookup sessionId sessions
  case mSession of
    Nothing -> error "No such session"
    Just (Session discon) -> pure discon
disconnectedTransaction _ _ = error "remote connection used"

{-
inclusionDependencies :: Connection -> M.Map IncDepName InclusionDependency
inclusionDependencies (InProcessConnection (DisconnectedTransaction _ context)) = inclusionDependencies context
inclusionDependencies _ = error "remote connection used"                       
-}
                           
dateExamplesConnection :: NotificationCallback -> IO (SessionId, Connection)
dateExamplesConnection callback = do
  dbconn <- connectProjectM36 (InProcessConnectionInfo NoPersistence callback)
  let incDeps = Base.inclusionDependencies dateExamples
  case dbconn of 
    Left err -> error (show err)
    Right conn -> do
      eSessionId <- createSessionAtHead "master" conn
      case eSessionId of
        Left err -> error (show err)
        Right sessionId -> do
          mapM_ (\(rvName,rvRel) -> executeDatabaseContextExpr sessionId conn (Assign rvName (ExistingRelation rvRel))) (M.toList (relationVariables dateExamples))
          mapM_ (\(idName,incDep) -> executeDatabaseContextExpr sessionId conn (AddInclusionDependency idName incDep)) (M.toList incDeps)
      --skipping atom functions for now- there are no atom function manipulation operators yet
          commit sessionId conn >>= maybeFail
          pure (sessionId, conn)

-- test notifications over the InProcessConnection
testNotification :: Test
testNotification = TestCase $ do
  notifmvar <- newEmptyMVar
  let notifCallback mvar = \_ _ -> putMVar mvar ()
      relvarx = RelationVariable "x"
  (sess, conn) <- dateExamplesConnection (notifCallback notifmvar)
  let check' x = x >>= maybe (pure ()) (\err -> assertFailure (show err))  
  check' $ executeDatabaseContextExpr sess conn (Assign "x" (ExistingRelation relationTrue))
  check' $ executeDatabaseContextExpr sess conn (AddNotification "test notification" relvarx relvarx)  
  check' $ commit sess conn
  check' $ executeDatabaseContextExpr sess conn (Assign "x" (ExistingRelation relationFalse))
  check' $ commit sess conn
  takeMVar notifmvar
  
executeTutorialD :: SessionId -> Connection -> String -> IO ()
executeTutorialD sessionId conn tutd = case parseTutorialD tutd of
    Left err -> assertFailure (show tutd ++ ": " ++ show err)
    Right parsed -> do 
      result <- evalTutorialD sessionId conn UnsafeEvaluation parsed
      case result of
        QuitResult -> assertFailure "quit?"
        DisplayResult _ -> assertFailure "display?"
        DisplayIOResult _ -> assertFailure "displayIO?"
        DisplayRelationResult _ -> assertFailure "displayrelation?"
        DisplayErrorResult err -> assertFailure (show err)        
        QuietSuccessResult -> pure ()
  
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
  case mkRelationFromList (attributesFromList [Attribute "conflict" intAtomType]) [[intAtom 1],[intAtom 2]] of
    Left err -> assertFailure (show err)
    Right conflictCheck -> do
      eRv <- executeRelationalExpr sessionId dbconn (RelationVariable "conflictrv")
      case eRv of
        Left err -> assertFailure (show err)
        Right conflictrv -> assertEqual "conflict union merge relvar" conflictCheck conflictrv
{-# LANGUAGE OverloadedStrings #-}
import TutorialD.Interpreter.DatabaseExpr
import TutorialD.Interpreter
import TutorialD.Interpreter.Base
import Test.HUnit
import ProjectM36.RelationalExpression
import ProjectM36.Relation
import ProjectM36.Tuple
import ProjectM36.TupleSet
import ProjectM36.Error
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
import Data.Typeable (Proxy(..))
import Data.Text (Text)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (fromGregorian)
import Data.Interval (interval, Interval, Extended(Finite))
import Control.Concurrent.STM
import qualified STMContainers.Map as STMMap

main :: IO ()
main = do
  tcounts <- runTestTT (TestList tests)
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess
  where
    tests = map (\(tutd, expected) -> TestCase $ assertTutdEqual basicDatabaseContext expected tutd) simpleRelTests ++ map (\(tutd, expected) -> TestCase $ assertTutdEqual dateExamples expected tutd) dateExampleRelTests ++ [transactionGraphBasicTest, transactionGraphAddCommitTest, transactionRollbackTest, transactionJumpTest, transactionBranchTest, simpleJoinTest]
    simpleRelTests = [("x:=true", Right relationTrue),
                      ("x:=false", Right relationFalse),
                      ("x:=true union false", Right relationTrue),
                      ("x:=true; x:=false", Right relationFalse),
                      ("x:=relation{a int}", mkRelation simpleAAttributes emptyTupleSet),
                      ("x:=relation{c int} rename {c as d}", mkRelation simpleBAttributes emptyTupleSet),
                      ("y:=relation{b int, c int}; x:=y{c}", mkRelation simpleProjectionAttributes emptyTupleSet),
                      ("x:=relation{tuple{a \"spam\", b 5}}", mkRelation simpleCAttributes $ RelationTupleSet [(RelationTuple simpleCAttributes) (V.fromList [stringAtom "spam", intAtom 5])]),
                      ("constraint failc true in false; x:=true", Left $ InclusionDependencyCheckError "failc"),
                      ("x:=y; x:=true", Left $ RelVarNotDefinedError "y"),
                      ("x:=relation{}", Right relationFalse),
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
                      ("x:=relation {b int, a char}; insert x relation{tuple{b 5, a \"spam\"}}", mkRelationFromTuples simpleCAttributes [RelationTuple simpleCAttributes $ V.fromList[stringAtom "spam", intAtom 5]]),
                      -- test nested relation constructor
                      ("x:=relation{tuple{a 5, b relation{tuple{a 6}}}}", mkRelation nestedRelationAttributes $ RelationTupleSet [RelationTuple nestedRelationAttributes (V.fromList [intAtom 5, Atom (Relation simpleAAttributes $ RelationTupleSet [RelationTuple simpleAAttributes $ V.fromList [intAtom 6]])])]),
                      ("x:=relation{tuple{b 5,a \"spam\"},tuple{b 6,a \"sam\"}}; delete x where b=6", mkRelation simpleCAttributes $ RelationTupleSet [RelationTuple simpleCAttributes (V.fromList [stringAtom "spam", intAtom 5])]),
                      ("x:=relation{tuple{a 5}} : {b:=@a}", mkRelation simpleDAttributes $ RelationTupleSet [RelationTuple simpleDAttributes (V.fromList [intAtom 5, intAtom 5])]),
                      ("x:=relation{tuple{a 5}} : {b:=6}", mkRelationFromTuples simpleDAttributes [RelationTuple simpleDAttributes (V.fromList [intAtom 5, intAtom 6])]),
                      ("x:=relation{tuple{a 5}} : {b:=add(@a,5)}", mkRelationFromTuples simpleDAttributes [RelationTuple simpleDAttributes (V.fromList [intAtom 5, intAtom 10])]),
                      ("x:=relation{tuple{a 5}} : {b:=add(@a,\"spam\")}", Left (AtomFunctionTypeError "add" 2 intAtomType stringAtomType)),
                      ("x:=relation{tuple{a 5}} : {b:=add(add(@a,2),5)}", mkRelationFromTuples simpleDAttributes [RelationTuple simpleDAttributes (V.fromList [intAtom 5, intAtom 12])])
                     ]
    simpleAAttributes = A.attributesFromList [Attribute "a" intAtomType]
    simpleBAttributes = A.attributesFromList [Attribute "d" intAtomType]
    simpleCAttributes = A.attributesFromList [Attribute "a" stringAtomType, Attribute "b" intAtomType]
    simpleDAttributes = A.attributesFromList [Attribute "a" intAtomType, Attribute "b" intAtomType]
    simpleMaybeTextAttributes = A.attributesFromList [Attribute "a" $ atomTypeForProxy (Proxy :: Proxy (Maybe Text))]
    simpleMaybeIntAttributes = A.attributesFromList [Attribute "a" $ atomTypeForProxy (Proxy :: Proxy (Maybe Int))]    
    simpleIntervalDateTimeAttributes = A.attributesFromList [Attribute "a" $ atomTypeForProxy (Proxy :: Proxy (Interval UTCTime))]
    simpleProjectionAttributes = A.attributesFromList [Attribute "c" intAtomType]
    nestedRelationAttributes = A.attributesFromList [Attribute "a" intAtomType, Attribute "b" (RelationAtomType $ A.attributesFromList [Attribute "a" intAtomType])]
    extendTestAttributes = A.attributesFromList [Attribute "a" intAtomType, Attribute "b" $ RelationAtomType (attributes suppliersRel)]
    byteStringAttributes = A.attributesFromList [Attribute "y" byteStringAtomType]    
    groupCountAttrs = A.attributesFromList [Attribute "z" intAtomType]
    minMaxAttrs = A.attributesFromList [Attribute "S#" stringAtomType, Attribute "z" intAtomType]
    dateExampleRelTests = [("x:=S where true", Right suppliersRel),
                           ("x:=S where CITY = \"London\"", restrict (\tuple -> atomForAttributeName "CITY" tuple == (Right $ stringAtom "London")) suppliersRel),
                           ("x:=S where false", Right $ Relation (attributes suppliersRel) emptyTupleSet),
                           ("x:=P where COLOR=\"Blue\" and CITY=\"Paris\"", mkRelationFromList (attributes productsRel) [[stringAtom "P5", stringAtom "Cam", stringAtom "Blue", intAtom 12, stringAtom "Paris"]]),
                           ("a:=S; update a (STATUS:=50); x:=a{STATUS}", mkRelation (A.attributesFromList [Attribute "STATUS" intAtomType]) (RelationTupleSet [mkRelationTuple (A.attributesFromList [Attribute "STATUS" intAtomType]) (V.fromList [intAtom 50])])),
                           --atom function tests
                           ("x:=((S : {STATUS2 := add(10,@STATUS)}) where STATUS2=add(10,@STATUS)){CITY,S#,SNAME,STATUS}", Right suppliersRel),
                           ("x:=S; update x where SNAME=\"Blake\" (CITY:=\"Boston\")", relMap (\tuple -> if atomForAttributeName "SNAME" tuple == (Right $ stringAtom "Blake") then updateTuple (M.singleton "CITY" (stringAtom "Boston")) tuple else tuple) suppliersRel),
                           ("x:=relation{tuple{a 5}} : {b:=S}", mkRelation extendTestAttributes (RelationTupleSet [mkRelationTuple extendTestAttributes (V.fromList [intAtom 5, Atom suppliersRel])])),
                           --relatom function tests
                           ("x:=((S group ({CITY} as y)):{z:=count(@y)}){z}", mkRelation groupCountAttrs (RelationTupleSet [mkRelationTuple groupCountAttrs (V.singleton $ intAtom 1)])),
                           ("x:=(SP group ({S#} as y)) ungroup y", Right supplierProductsRel),
                           ("x:=((SP{S#,QTY}) group ({QTY} as x):{z:=max(@x)}){S#,z}", mkRelationFromList minMaxAttrs (map (\(s,i) -> [stringAtom s,intAtom i]) [("S1", 400), ("S2", 400), ("S3", 200), ("S4", 400)])),
                           ("x:=((SP{S#,QTY}) group ({QTY} as x):{z:=min(@x)}){S#,z}", mkRelationFromList minMaxAttrs (map (\(s,i) -> [stringAtom s,intAtom i]) [("S1", 100), ("S2", 300), ("S3", 200), ("S4", 200)])),
                           ("x:=((SP{S#,QTY}) group ({QTY} as x):{z:=sum(@x)}){S#,z}", mkRelationFromList minMaxAttrs (map (\(s,i) -> [stringAtom s,intAtom i]) [("S1", 1000), ("S2", 700), ("S3", 200), ("S4", 900)])),
                           --boolean function restriction
                           ("x:=S where ^lt(@STATUS,20)", mkRelationFromList (attributes suppliersRel) [[stringAtom "S2", stringAtom "Jones", intAtom 10, stringAtom "Paris"]]),
                           ("x:=S where ^gt(@STATUS,20)", mkRelationFromList (attributes suppliersRel) [[stringAtom "S3", stringAtom "Blake", intAtom 30, stringAtom "Paris"],
                                                                                                       [stringAtom "S5", stringAtom "Adams", intAtom 30, stringAtom "Athens"]]),
                           ("x:=S where ^sum(@STATUS)", Left $ AtomTypeMismatchError intAtomType boolAtomType),
                           ("x:=S where ^not(gte(@STATUS,20))", mkRelationFromList (attributes suppliersRel) [[stringAtom "S2", stringAtom "Jones", intAtom 10, stringAtom "Paris"]]),
                           --test "all but" attribute inversion syntax
                           ("x:=S{all but S#} = S{CITY,SNAME,STATUS}", Right $ relationTrue),
                           --test key syntax
                           ("x:=S; key testconstraint {S#,CITY} x; insert x relation{tuple{CITY \"London\", S# \"S1\", SNAME \"gonk\", STATUS 50}}", Left (InclusionDependencyCheckError "testconstraint")),
                           ("y:=S; key testconstraint {S#} y; insert y relation{tuple{CITY \"London\", S# \"S6\", SNAME \"gonk\", STATUS 50}}; x:=y{S#} = S{S#} union relation{tuple{S# \"S6\"}}", Right $ relationTrue),
                           --test binary bytestring data type
                           ("x:=relation{tuple{y \"dGVzdGRhdGE=\"::bytestring}}", mkRelationFromList byteStringAttributes [[Atom (TE.encodeUtf8 "testdata")]]),
                           --test Maybe Text
                           ("x:=relation{tuple{a Just \"spam\"::maybe char}}", mkRelationFromList simpleMaybeTextAttributes [[Atom (Just "spam" :: Maybe Text)]]),
                           --test Maybe Int
                           ("x:=relation{tuple{a Just 3::maybe int}}", mkRelationFromList simpleMaybeIntAttributes [[Atom (Just 3 :: Maybe Int)]]),
                           --test datetime interval
                           ("x:=relation{tuple{a interval_datetime((\"2015-01-01 00:00:00\",\"2016-01-01 00:00:00\"])}}", mkRelationFromList simpleIntervalDateTimeAttributes [
                               [Atom $ interval (Finite (UTCTime (fromGregorian 2015 1 1) 0), False)
                                                (Finite (UTCTime (fromGregorian 2016 1 1) 0), True)]
                           ])
                          ]

assertTutdEqual :: DatabaseContext -> Either RelationalError Relation -> String -> Assertion
assertTutdEqual databaseContext expected tutd = assertEqual tutd expected interpreted
  where
    interpreted = case interpretDatabaseExpr databaseContext tutd of
      Left err -> Left err
      Right context -> case M.lookup "x" (relationVariables context) of
        Nothing -> Left $ RelVarNotDefinedError "x"
        Just rel -> Right rel

transactionGraphBasicTest :: Test
transactionGraphBasicTest = TestCase $ do
  (_, dbconn) <- dateExamplesConnection
  graph <- transactionGraph dbconn
  assertEqual "validate bootstrapped graph" (validateGraph graph) Nothing

--add a new transaction to the graph, validate it is in the graph
transactionGraphAddCommitTest :: Test
transactionGraphAddCommitTest = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection
  case parseTutorialD "x:=S" of
    Left err -> assertFailure (show err)
    Right parsed -> do 
      result <- evalTutorialD sessionId dbconn parsed
      case result of
        QuitResult -> assertFailure "quit?"
        DisplayResult _ -> assertFailure "display?"
        DisplayIOResult _ -> assertFailure "displayIO?"
        DisplayErrorResult err -> assertFailure (show err)        
        QuietSuccessResult -> do
          commit sessionId dbconn >>= maybeFail
          (DisconnectedTransaction _ context) <- disconnectedTransaction sessionId dbconn
          assertEqual "ensure x was added" (M.lookup "x" (relationVariables context)) (Just suppliersRel)

transactionRollbackTest :: Test
transactionRollbackTest = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection
  graph <- transactionGraph dbconn
  maybeErr <- executeDatabaseContextExpr sessionId dbconn (Assign "x" (RelationVariable "S"))
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
  (sessionId, dbconn) <- dateExamplesConnection
  (DisconnectedTransaction firstUUID _) <- disconnectedTransaction sessionId dbconn
  maybeErr <- executeDatabaseContextExpr sessionId dbconn (Assign "x" (RelationVariable "S"))
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
  (sessionId, dbconn) <- dateExamplesConnection
  mapM_ (\x -> x >>= maybeFail) [executeGraphExpr sessionId dbconn (Branch "test"),
                  executeDatabaseContextExpr sessionId dbconn (Assign "x" (RelationVariable "S")),
                  commit sessionId dbconn,
                  executeGraphExpr sessionId dbconn (JumpToHead "master"),
                  executeDatabaseContextExpr sessionId dbconn (Assign "y" (RelationVariable "S"))
                  ]
  graph <- transactionGraph dbconn
  assertBool "master branch exists" $ isJust (transactionForHead "master" graph)
  assertBool "test branch exists" $ isJust (transactionForHead "test" graph)

simpleJoinTest :: Test
simpleJoinTest = TestCase $ assertTutdEqual dateExamples joinedRel "x:=S join SP"
    where
        attrs = A.attributesFromList [Attribute "CITY" stringAtomType,
                                      Attribute "QTY" intAtomType,
                                      Attribute "P#" stringAtomType,
                                      Attribute "S#" stringAtomType,
                                      Attribute "SNAME" stringAtomType,
                                      Attribute "STATUS" intAtomType]
        joinedRel = mkRelationFromList attrs [[stringAtom "London", intAtom 100, stringAtom "P6", stringAtom "S1", stringAtom "Smith", intAtom 20],
                                              [stringAtom "London", intAtom 400, stringAtom "P3", stringAtom "S1", stringAtom "Smith", intAtom 20],
                                              [stringAtom "London", intAtom 400, stringAtom "P5", stringAtom "S4", stringAtom "Clark", intAtom 20],
                                              [stringAtom "London", intAtom 300, stringAtom "P1", stringAtom "S1", stringAtom "Smith", intAtom 20],
                                              [stringAtom "Paris", intAtom 200, stringAtom "P2", stringAtom "S3", stringAtom "Blake", intAtom 30],
                                              [stringAtom "Paris", intAtom 300, stringAtom "P1", stringAtom "S2", stringAtom "Jones", intAtom 10],
                                              [stringAtom "London", intAtom 100, stringAtom "P5", stringAtom "S1", stringAtom "Smith", intAtom 20],
                                              [stringAtom "London", intAtom 300, stringAtom "P4", stringAtom "S4", stringAtom "Clark", intAtom 20],
                                              [stringAtom "Paris", intAtom 400, stringAtom "P2", stringAtom "S2", stringAtom "Jones", intAtom 10],
                                              [stringAtom "London", intAtom 200, stringAtom "P2", stringAtom "S1", stringAtom "Smith", intAtom 20],
                                              [stringAtom "London", intAtom 200, stringAtom "P4", stringAtom "S1", stringAtom "Smith", intAtom 20],
                                              [stringAtom "London", intAtom 200, stringAtom "P2", stringAtom "S4", stringAtom "Clark", intAtom 20]
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
                           
dateExamplesConnection :: IO (SessionId, Connection)
dateExamplesConnection = do
  dbconn <- connectProjectM36 (InProcessConnectionInfo NoPersistence emptyNotificationCallback)
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
      


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
import ProjectM36.Base
import ProjectM36.TransactionGraph
import ProjectM36.Client
import qualified ProjectM36.Attribute as A
import qualified Data.HashSet as HS
import qualified Data.Map as M
import System.Exit
import Data.Maybe (isJust)
import System.IO
import qualified Data.Vector as V

--urgent: add group and ungroup tests- I missed the group relation type bug
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
                      ("x:=relation{tuple{a \"spam\", b 5}}", mkRelation simpleCAttributes $ HS.singleton ((RelationTuple simpleCAttributes) (V.fromList [StringAtom "spam", IntAtom 5]))),
                      ("constraint failc true in false; x:=true", Left $ InclusionDependencyCheckError "failc"),
                      ("x:=y; x:=true", Left $ RelVarNotDefinedError "y"),
                      ("x:=true where true", Right relationTrue),
                      ("x:=true where false", Right relationFalse),
                      ("x:=true where true or false", Right relationTrue),
                      ("x:=true where false or false", Right relationFalse),
                      ("x:=true where true and false", Right relationFalse),
                      ("x:=true where true and true", Right relationTrue),
                      ("x:=true=true", Right relationTrue),
                      ("x:=true=false", Right relationFalse),
                      ("x:=true; undefine x", Left (RelVarNotDefinedError "x")),
                      ("x:=relation {b int, a char}; insert x relation{tuple{b 5, a \"spam\"}}", mkRelation simpleCAttributes (HS.fromList [RelationTuple simpleCAttributes $ V.fromList [StringAtom "spam", IntAtom 5]])),
                      -- test nested relation constructor
                      ("x:=relation{tuple{a 5, b relation{tuple{a 6}}}}", mkRelation nestedRelationAttributes $ HS.singleton $ RelationTuple nestedRelationAttributes (V.fromList [IntAtom 5, RelationAtom (Relation simpleAAttributes (HS.singleton $ RelationTuple simpleAAttributes $ V.fromList [IntAtom 6]))])),
                      ("x:=relation{tuple{b 5,a \"spam\"},tuple{b 6,a \"sam\"}}; delete x where b=6", mkRelation simpleCAttributes $ HS.singleton $ RelationTuple simpleCAttributes (V.fromList [StringAtom "spam", IntAtom 5])),
                      ("x:=relation{tuple{a 5}} : {b:=@a}", mkRelation simpleDAttributes $ HS.singleton $ RelationTuple simpleDAttributes (V.fromList [IntAtom 5, IntAtom 5])),
                      ("x:=relation{tuple{a 5}} : {b:=6}", mkRelation simpleDAttributes $ HS.singleton $ RelationTuple simpleDAttributes (V.fromList [IntAtom 5, IntAtom 6])),
                      ("x:=relation{tuple{a 5}} : {b:=add(@a,5)}", mkRelation simpleDAttributes $ HS.singleton $ RelationTuple simpleDAttributes (V.fromList [IntAtom 5, IntAtom 10])),
                      ("x:=relation{tuple{a 5}} : {b:=add(@a,\"spam\")}", Left (AtomFunctionTypeError "add" 2 IntAtomType StringAtomType)),
                      ("x:=relation{tuple{a 5}} : {b:=add(add(@a,2),5)}", mkRelation simpleDAttributes $ HS.singleton $ RelationTuple simpleDAttributes (V.fromList [IntAtom 5, IntAtom 12]))
                     ]
    simpleAAttributes = A.attributesFromList [Attribute "a" IntAtomType]
    simpleBAttributes = A.attributesFromList [Attribute "d" IntAtomType]
    simpleCAttributes = A.attributesFromList [Attribute "a" StringAtomType, Attribute "b" IntAtomType]
    simpleDAttributes = A.attributesFromList [Attribute "a" IntAtomType, Attribute "b" IntAtomType]
    simpleProjectionAttributes = A.attributesFromList [Attribute "c" IntAtomType]
    nestedRelationAttributes = A.attributesFromList [Attribute "a" IntAtomType, Attribute "b" (RelationAtomType $ A.attributesFromList [Attribute "a" IntAtomType])]
    extendTestAttributes = A.attributesFromList [Attribute "a" IntAtomType, Attribute "b" $ RelationAtomType (attributes suppliersRel)]
    groupCountAttrs = A.attributesFromList [Attribute "z" IntAtomType]
    minMaxAttrs = A.attributesFromList [Attribute "S#" StringAtomType, Attribute "z" IntAtomType]
    dateExampleRelTests = [("x:=S where true", Right suppliersRel),
                           ("x:=S where CITY = \"London\"", restrict (\tuple -> atomForAttributeName "CITY" tuple == (Right $ StringAtom "London")) suppliersRel),
                           ("x:=S where false", Right $ Relation (attributes suppliersRel) emptyTupleSet),
                           ("a:=S; update a (STATUS:=50); x:=a{STATUS}", mkRelation (A.attributesFromList [ Attribute "STATUS" IntAtomType]) (HS.singleton $ mkRelationTuple (A.attributesFromList [Attribute "STATUS" IntAtomType]) (V.fromList [IntAtom 50]))),
                           --atom function tests
                           ("x:=((S : {STATUS2 := add(10,@STATUS)}) where STATUS2=add(10,@STATUS)){CITY,S#,SNAME,STATUS}", Right suppliersRel),
                           ("x:=S; update x where SNAME=\"Blake\" (CITY:=\"Boston\")", relMap (\tuple -> if atomForAttributeName "SNAME" tuple == (Right $ StringAtom "Blake") then updateTuple (M.singleton "CITY" (StringAtom "Boston")) tuple else tuple) suppliersRel),
                           ("x:=relation{tuple{a 5}} : {b:=S}", mkRelation extendTestAttributes (HS.singleton $ mkRelationTuple extendTestAttributes (V.fromList [IntAtom 5, RelationAtom suppliersRel]))),
                           --relatom function tests
                           ("x:=((S group ({CITY} as y)):{z:=count(@y)}){z}", mkRelation groupCountAttrs (HS.singleton $ mkRelationTuple groupCountAttrs (V.singleton $ IntAtom 1))),
                           ("x:=(SP group ({S#} as y)) ungroup y", Right supplierProductsRel),
                           ("x:=((SP{S#,QTY}) group ({QTY} as x):{z:=max(@x)}){S#,z}", mkRelationFromList minMaxAttrs (map (\(s,i) -> [StringAtom s,IntAtom i]) [("S1", 400), ("S2", 400), ("S3", 200), ("S4", 400)])),
                           ("x:=((SP{S#,QTY}) group ({QTY} as x):{z:=min(@x)}){S#,z}", mkRelationFromList minMaxAttrs (map (\(s,i) -> [StringAtom s,IntAtom i]) [("S1", 100), ("S2", 300), ("S3", 200), ("S4", 200)])),
                           ("x:=((SP{S#,QTY}) group ({QTY} as x):{z:=sum(@x)}){S#,z}", mkRelationFromList minMaxAttrs (map (\(s,i) -> [StringAtom s,IntAtom i]) [("S1", 1000), ("S2", 700), ("S3", 200), ("S4", 900)])),
			   --boolean function restriction
			   ("x:=S where ^lt(@STATUS,20)", mkRelationFromList (attributes suppliersRel) [[StringAtom "S2", StringAtom "Jones", IntAtom 10, StringAtom "Paris"]]),
			   ("x:=S where ^gt(@STATUS,20)", mkRelationFromList (attributes suppliersRel) [[StringAtom "S3", StringAtom "Blake", IntAtom 30, StringAtom "Paris"],
			   	  			  		     		 	        [StringAtom "S5", StringAtom "Adams", IntAtom 30, StringAtom "Athens"]]),
			   ("x:=S where ^sum(@STATUS)", Left $ AtomTypeMismatchError IntAtomType BoolAtomType),
			   ("x:=S where ^not(gte(@STATUS,20))", mkRelationFromList (attributes suppliersRel) [[StringAtom "S2", StringAtom "Jones", IntAtom 10, StringAtom "Paris"]]),
			   --test "all but" attribute inversion syntax
			   ("x:=S{all but S#} = S{CITY,SNAME,STATUS}", Right $ relationTrue),
                           --test key syntax
                           ("x:=S; key testconstraint {S#,CITY} x; insert x relation{tuple{CITY \"London\", S# \"S1\", SNAME \"gonk\", STATUS 50}}", Left (InclusionDependencyCheckError "testconstraint")),
                           ("y:=S; key testconstraint {S#} y; insert y relation{tuple{CITY \"London\", S# \"S6\", SNAME \"gonk\", STATUS 50}}; x:=y{S#} = S{S#} union relation{tuple{S# \"S6\"}}", Right $ relationTrue)

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
  dbconn <- dateExamplesConnection
  graph <- transactionGraph dbconn
  assertEqual "validate bootstrapped graph" (validateGraph graph) Nothing

--add a new transaction to the graph, validate it is in the graph
transactionGraphAddCommitTest :: Test
transactionGraphAddCommitTest = TestCase $ do
  dbconn <- dateExamplesConnection
  case parseTutorialD dbconn "x:=S" of
    Left err -> assertFailure (show err)
    Right parsed -> do 
      result <- evalTutorialD dbconn parsed
      case result of
        QuitResult -> assertFailure "quit?"
        DisplayResult _ -> assertFailure "display?"
        DisplayIOResult _ -> assertFailure "displayIO?"
        DisplayErrorResult err -> assertFailure (show err)        
        QuietSuccessResult -> do
          commit dbconn >>= maybeFail
          (DisconnectedTransaction _ context) <- disconnectedTransaction dbconn
          assertEqual "ensure x was added" (M.lookup "x" (relationVariables context)) (Just suppliersRel)

transactionRollbackTest :: Test
transactionRollbackTest = TestCase $ do
  dbconn <- dateExamplesConnection
  graph <- transactionGraph dbconn
  maybeErr <- executeDatabaseContextExpr dbconn (Assign "x" (RelationVariable "S"))
  case maybeErr of
    Just err -> assertFailure (show err)
    Nothing -> do
      rollback dbconn >>= maybeFail
      (DisconnectedTransaction _ context') <- disconnectedTransaction dbconn
      graph' <- transactionGraph dbconn
      assertEqual "validate context" Nothing (M.lookup "x" (relationVariables context'))
      assertEqual "validate graph" graph graph'

--commit a new transaction with "x" relation, jump to first transaction, verify that "x" is not present
transactionJumpTest :: Test
transactionJumpTest = TestCase $ do
  dbconn <- dateExamplesConnection
  (DisconnectedTransaction firstUUID _) <- disconnectedTransaction dbconn
  maybeErr <- executeDatabaseContextExpr dbconn (Assign "x" (RelationVariable "S"))
  case maybeErr of
    Just err -> assertFailure (show err)
    Nothing -> do
      commit dbconn >>= maybeFail
      --perform the jump
      maybeErr2 <- executeGraphExpr dbconn (JumpToTransaction firstUUID)
      case maybeErr2 of
        Just err -> assertFailure (show err)
        Nothing -> do
          --check that the disconnected transaction does not include "x"
          (DisconnectedTransaction _ context') <- disconnectedTransaction dbconn
          assertEqual "ensure x is not present" Nothing (M.lookup "x" (relationVariables context'))          

maybeFail :: (Show a) => Maybe a -> IO ()
maybeFail (Just err) = assertFailure (show err)
maybeFail Nothing = return ()

--branch from the first transaction and verify that there are two heads
transactionBranchTest :: Test
transactionBranchTest = TestCase $ do
  dbconn <- dateExamplesConnection
  mapM_ (\x -> x >>= maybeFail) [executeGraphExpr dbconn (Branch "test"),
                  executeDatabaseContextExpr dbconn (Assign "x" (RelationVariable "S")),
                  commit dbconn,
                  executeGraphExpr dbconn (JumpToHead "master"),
                  executeDatabaseContextExpr dbconn (Assign "y" (RelationVariable "S"))
                  ]
  graph <- transactionGraph dbconn
  assertBool "master branch exists" $ isJust (transactionForHead "master" graph)
  assertBool "test branch exists" $ isJust (transactionForHead "test" graph)

simpleJoinTest :: Test
simpleJoinTest = TestCase $ assertTutdEqual dateExamples joinedRel "x:=S join SP"
    where
        attrs = A.attributesFromList [Attribute "CITY" StringAtomType,
                                      Attribute "QTY" IntAtomType,
                                      Attribute "P#" StringAtomType,
                                      Attribute "S#" StringAtomType,
                                      Attribute "SNAME" StringAtomType,
                                      Attribute "STATUS" IntAtomType]
        joinedRel = mkRelationFromList attrs [[StringAtom "London", IntAtom 100, StringAtom "P6", StringAtom "S1", StringAtom "Smith", IntAtom 20],
                                              [StringAtom "London", IntAtom 400, StringAtom "P3", StringAtom "S1", StringAtom "Smith", IntAtom 20],
                                              [StringAtom "London", IntAtom 400, StringAtom "P5", StringAtom "S4", StringAtom "Clark", IntAtom 20],
                                              [StringAtom "London", IntAtom 300, StringAtom "P1", StringAtom "S1", StringAtom "Smith", IntAtom 20],
                                              [StringAtom "Paris", IntAtom 200, StringAtom "P2", StringAtom "S3", StringAtom "Blake", IntAtom 30],
                                              [StringAtom "Paris", IntAtom 300, StringAtom "P1", StringAtom "S2", StringAtom "Jones", IntAtom 10],
                                              [StringAtom "London", IntAtom 100, StringAtom "P5", StringAtom "S1", StringAtom "Smith", IntAtom 20],
                                              [StringAtom "London", IntAtom 300, StringAtom "P4", StringAtom "S4", StringAtom "Clark", IntAtom 20],
                                              [StringAtom "Paris", IntAtom 400, StringAtom "P2", StringAtom "S2", StringAtom "Jones", IntAtom 10],
                                              [StringAtom "London", IntAtom 200, StringAtom "P2", StringAtom "S1", StringAtom "Smith", IntAtom 20],
                                              [StringAtom "London", IntAtom 200, StringAtom "P4", StringAtom "S1", StringAtom "Smith", IntAtom 20],
                                              [StringAtom "London", IntAtom 200, StringAtom "P2", StringAtom "S4", StringAtom "Clark", IntAtom 20]
                                              ]

dateExamplesConnection :: IO (Connection)
dateExamplesConnection = do
  dbconn <- connectProjectM36 (InProcessConnectionInfo NoPersistence)
  case dbconn of 
    Left err -> do
      hPutStrLn stderr (show err)
      exitFailure
    Right conn -> do
      mapM_ (\(rvName,rvRel) -> executeDatabaseContextExpr conn (Assign rvName (ExistingRelation rvRel))) (M.toList (relationVariables dateExamples))
      mapM_ (\(idName,incDep) -> executeDatabaseContextExpr conn (AddInclusionDependency idName incDep)) (M.toList (inclusionDependencies dateExamples))
      --skipping atom functions for now- there are no atom function manipulation operators yet
      commit conn >>= maybeFail
      return conn 
      

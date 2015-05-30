{-# LANGUAGE OverloadedStrings #-}
import TutorialD.Interpreter
import Test.HUnit
import ProjectM36.RelationalExpression
import ProjectM36.Relation
import ProjectM36.Tuple
import ProjectM36.TupleSet
import ProjectM36.Error
import ProjectM36.Base
import ProjectM36.Transaction
import qualified ProjectM36.Attribute as A
import qualified Data.HashSet as HS
import qualified Data.Map as M
import qualified Data.Set as S
import System.Exit
import Data.UUID.V4 (nextRandom)
import Data.UUID (nil)
import Data.Either (isRight)
import qualified Data.Vector as V

--urgent: add group and ungroup tests- I missed the group relation type bug
main :: IO ()
main = do 
  tcounts <- runTestTT (TestList tests)
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess
  where
    tests = map (\(tutd, expected) -> TestCase $ assertTutdEqual basicDatabaseContext expected tutd) simpleRelTests ++ map (\(tutd, expected) -> TestCase $ assertTutdEqual dateExamples expected tutd) dateExampleRelTests ++ [transactionGraphBasicTest, transactionGraphAddCommitTest, transactionRollbackTest, transactionJumpTest, transactionBranchTest]
    simpleRelTests = [("x:=true", Right relationTrue),
                      ("x:=false", Right relationFalse),
                      ("x:=true union false", Right relationTrue),
                      ("x:=true; x:=false", Right relationFalse),
                      ("x:=relation{a int}", mkRelation simpleAAttributes emptyTupleSet),
                      ("x:=relation{c int} rename {c as d}", mkRelation simpleBAttributes emptyTupleSet),
                      ("y:=relation{b int, c int}; x:=y{c}", mkRelation simpleProjectionAttributes emptyTupleSet),
                      ("x:=relation{tuple{a char(\"spam\"), b int(5)}}", mkRelation simpleCAttributes $ HS.singleton ((RelationTuple simpleCAttributes) (V.fromList [StringAtom "spam", IntAtom 5]))),
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
                      ("x:=relation {b int, a char}; insert x relation{tuple{b int(5), a char(\"spam\")}}", mkRelation simpleCAttributes (HS.fromList [RelationTuple simpleCAttributes $ V.fromList [StringAtom "spam", IntAtom 5]])),
                      ("x:=relation{tuple{b int(5),a char(\"spam\")},tuple{b int(6),a char(\"sam\")}}; delete x where b=6", mkRelation simpleCAttributes $ HS.singleton $ RelationTuple simpleCAttributes (V.fromList [StringAtom "spam", IntAtom 5])),
                      ("x:=relation{tuple{a int(5)}} : {b:=@a}", mkRelation simpleDAttributes $ HS.singleton $ RelationTuple simpleDAttributes (V.fromList [IntAtom 5, IntAtom 5])),
                      ("x:=relation{tuple{a int(5)}} : {b:=6}", mkRelation simpleDAttributes $ HS.singleton $ RelationTuple simpleDAttributes (V.fromList [IntAtom 5, IntAtom 6])),
                      ("x:=relation{tuple{a int(5)}} : {b:=add(@a,5)}", mkRelation simpleDAttributes $ HS.singleton $ RelationTuple simpleDAttributes (V.fromList [IntAtom 5, IntAtom 10])),
                      ("x:=relation{tuple{a int(5)}} : {b:=add(@a,\"spam\")}", Left (AtomFunctionTypeError "add" 2 IntAtomType StringAtomType)),
                      ("x:=relation{tuple{a int(5)}} : {b:=add(add(@a,2),5)}", mkRelation simpleDAttributes $ HS.singleton $ RelationTuple simpleDAttributes (V.fromList [IntAtom 5, IntAtom 12]))
                     ]
    simpleAAttributes = A.attributesFromList [Attribute "a" IntAtomType]
    simpleBAttributes = A.attributesFromList [Attribute "d" IntAtomType]
    simpleCAttributes = A.attributesFromList [Attribute "a" StringAtomType, Attribute "b" IntAtomType]
    simpleDAttributes = A.attributesFromList [Attribute "a" IntAtomType, Attribute "b" IntAtomType]
    simpleProjectionAttributes = A.attributesFromList [Attribute "c" IntAtomType]
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
                           ("x:=relation{tuple{a int(5)}} : {b:=S}", mkRelation extendTestAttributes (HS.singleton $ mkRelationTuple extendTestAttributes (V.fromList [IntAtom 5, RelationAtom suppliersRel]))),
                           --relatom function tests                           
                           ("x:=((S group ({CITY} as y)):{z:=count(@y)}){z}", mkRelation groupCountAttrs (HS.singleton $ mkRelationTuple groupCountAttrs (V.singleton $ IntAtom 1))),
                           ("x:=(SP group ({S#} as y)) ungroup y", Right supplierProductsRel),
                           ("x:=((SP{S#,QTY}) group ({QTY} as x):{z:=max(@x)}){S#,z}", mkRelationFromList minMaxAttrs (map (\(s,i) -> [StringAtom s,IntAtom i]) [("S1", 400), ("S2", 400), ("S3", 200), ("S4", 400)])),
                           ("x:=((SP{S#,QTY}) group ({QTY} as x):{z:=min(@x)}){S#,z}", mkRelationFromList minMaxAttrs (map (\(s,i) -> [StringAtom s,IntAtom i]) [("S1", 100), ("S2", 300), ("S3", 200), ("S4", 200)])),
                           ("x:=((SP{S#,QTY}) group ({QTY} as x):{z:=sum(@x)}){S#,z}", mkRelationFromList minMaxAttrs (map (\(s,i) -> [StringAtom s,IntAtom i]) [("S1", 1000), ("S2", 700), ("S3", 200), ("S4", 900)]))
                           ]


assertTutdEqual :: DatabaseContext -> Either RelationalError Relation -> String -> Assertion
assertTutdEqual databaseContext expected tutd = assertEqual tutd expected interpreted
  where
    interpreted = case interpret databaseContext tutd of 
      (Just err, _) -> Left err
      (Nothing, context) -> case M.lookup "x" (relationVariables context) of
        Nothing -> Left $ RelVarNotDefinedError "x"
        Just rel -> Right rel 
      
transactionGraphBasicTest :: Test
transactionGraphBasicTest = TestCase $ do
    (_, graph) <-  dateExamplesGraph
    assertEqual "validate bootstrapped graph" (validateGraph graph) Nothing
    
--add a new transaction to the graph, validate it is in the graph    
transactionGraphAddCommitTest :: Test
transactionGraphAddCommitTest = TestCase $ do    
    ((DisconnectedTransaction firstUUID context), origGraph) <- dateExamplesGraph
    freshUUID <- nextRandom
    case interpret context "x:=S" of
      (Just err, _) -> assertFailure (show err)
      (Nothing, newContext) -> do
        let discon = newDisconnectedTransaction firstUUID newContext
        let addTrans = addDisconnectedTransaction freshUUID "master" discon origGraph
        case addTrans of
          Left err -> assertFailure (show err)
          Right (newTrans, graph) -> do
            assertBool "transaction in graph" (isRight $ transactionForUUID freshUUID graph)
            assertEqual "validate fresh commit with deleted S" (validateGraph graph) Nothing
            assertEqual "ensure S was deleted in newContext" (M.lookup "x" (relationVariables (transactionContext newTrans))) (Just suppliersRel)
    
transactionRollbackTest :: Test
transactionRollbackTest = TestCase $ do            
    ((DisconnectedTransaction firstUUID origContext), graph) <- dateExamplesGraph
    freshUUID <- nextRandom
    case transactionForUUID firstUUID graph of 
      Left err -> assertFailure (show err)
      Right _ -> do
        case interpret origContext "x:=S" of
          (Just err, _) -> assertFailure (show err)
          (Nothing, newContext) -> do
            let discon = newDisconnectedTransaction firstUUID newContext
            let ((DisconnectedTransaction _ newContext2), newGraph, _) = interpretOps freshUUID discon graph ":rollback"
            assertEqual "validate context" (M.lookup "x" (relationVariables newContext2)) Nothing
            assertEqual "validate graph" graph newGraph

--commit a new transaction with "x" relation, jump to first transaction, verify that "x" is not present
transactionJumpTest :: Test            
transactionJumpTest = TestCase $ do
    ((DisconnectedTransaction firstUUID origContext), origGraph) <- dateExamplesGraph 
    freshUUID <- nextRandom
    case transactionForUUID firstUUID origGraph of
      Left err -> assertFailure (show err)
      Right _ -> do
        case interpret origContext "x:=S" of
          (Just err, _) -> assertFailure (show err)
          (Nothing, newContext) -> do
            --modify the second transaction
            case interpret newContext "x:=S" of
              (Just err, _) -> assertFailure (show err)
              (Nothing, newContext2) -> do
                --add the transaction
                let discon = newDisconnectedTransaction firstUUID newContext2
                let addTrans = addDisconnectedTransaction freshUUID "master" discon origGraph
                case addTrans of
                  Left err -> assertFailure (show err)
                  Right (_, graph) -> do
                    --jump to the first transaction
                    let ((DisconnectedTransaction parentUUID newContext3), newGraph, _) = interpretOps freshUUID discon graph (":jump " ++ show firstUUID)
                    assertEqual "validate discon" parentUUID firstUUID
                    assertEqual "validate discon2" Nothing (M.lookup "x" (relationVariables newContext3))
                    assertEqual "validate graph" transactionUUIDs (S.map transactionUUID (transactionsForGraph newGraph))
                    where
                      transactionUUIDs = S.fromList [firstUUID, freshUUID]
                      
--branch from the first transaction and verify that there are two heads
transactionBranchTest :: Test                     
transactionBranchTest = TestCase $ do
    (origDiscon@(DisconnectedTransaction firstUUID origContext), origGraph) <- dateExamplesGraph
    freshUUID1 <- nextRandom
    freshUUID2 <- nextRandom
    case transactionForUUID firstUUID origGraph of    
      Left err -> assertFailure (show err)
      Right _ -> do
        let disconMaster = newDisconnectedTransaction firstUUID origContext
        let addTransMaster = addDisconnectedTransaction freshUUID1 "master" disconMaster origGraph
        --add a second transaction to the "master" branch
        case addTransMaster of
          Left err -> assertFailure (show err)
          Right (_, graph) -> do
            --add a third transaction to the "test" branch
            let (_, newGraph, _) = interpretOps freshUUID2 origDiscon graph ":branch test"
            assertEqual "verify test head" freshUUID2 $ maybeTransUUID (transactionForHead "test" newGraph)
            assertEqual "verify master head" freshUUID1 $ maybeTransUUID (transactionForHead "master" newGraph)
    where
      maybeTransUUID t = maybe nil transactionUUID t
        
dateExamplesGraph :: IO (DisconnectedTransaction, TransactionGraph)
dateExamplesGraph = do
  firstTransactionUUID <- nextRandom                    
  let graph = bootstrapTransactionGraph firstTransactionUUID dateExamples
  let discon = newDisconnectedTransaction firstTransactionUUID dateExamples
  return (discon, graph)


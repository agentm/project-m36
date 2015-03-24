import TutorialDInterpreter
import Test.HUnit
import RelationExpr
import Relation
import RelationalError
import RelationType
import RelationalTransaction
import qualified Data.HashSet as HS
import qualified Data.Map as M
import System.Exit
import Data.UUID.V4 (nextRandom)
import Data.Either (isRight)

main = do 
  counts <- runTestTT (TestList tests)
  if errors counts + failures counts > 0 then exitFailure else exitSuccess
  where
    tests = map (\(tutd, expected) -> TestCase $ assertTutdEqual basicDatabaseContext tutd expected) simpleRelTests ++ map (\(tutd, expected) -> TestCase $ assertTutdEqual dateExamples tutd expected) dateExampleRelTests ++ [transactionGraphBasicTest, transactionGraphAddCommitTest]
    simpleRelTests = [("x:=true", Right relationTrue),
                      ("x:=false", Right relationFalse),
                      ("x:=true union false", Right relationTrue),
                      ("x:=true; x:=false", Right relationFalse),
                      ("x:=relation{a int}", mkRelation simpleAAttributes HS.empty),
                      ("x:=relation{c int} rename {c as d}", mkRelation simpleBAttributes HS.empty),
                      ("y:=relation{b int, c int}; x:=y{c}", mkRelation simpleProjectionAttributes HS.empty),
                      ("x:=relation{tuple{a char(\"spam\"), b int(5)}}", mkRelation simpleCAttributes (HS.fromList $ map RelationTuple [M.fromList [("a", StringAtom "spam"),("b",IntAtom 5)]])),
                      ("constraint failc true in false; x:=true", Left $ InclusionDependencyCheckError "failc"),
                      ("x:=true where true", Right relationTrue),
                      ("x:=true where false", Right relationFalse),
                      ("x:=true where true or false", Right relationTrue),
                      ("x:=true where false or false", Right relationFalse),
                      ("x:=true where true and false", Right relationFalse),
                      ("x:=true where true and true", Right relationTrue),
                      ("x:=true=true", Right relationTrue),
                      ("x:=true=false", Right relationFalse),
                      ("x:=relation {b int, a char}; insert x relation{tuple{b int(5), a char(\"spam\")}}", mkRelation simpleCAttributes (HS.fromList [RelationTuple $ M.fromList [("a", StringAtom "spam"), ("b", IntAtom 5)]]))
                     ]
    simpleAAttributes = M.fromList [("a", Attribute "a" IntAtomType)]
    simpleBAttributes = M.fromList [("d", Attribute "d" IntAtomType)]
    simpleCAttributes = M.fromList [("a", Attribute "a" StringAtomType), ("b", Attribute "b" IntAtomType)]
    simpleProjectionAttributes = M.fromList [("c", Attribute "c" IntAtomType)]
    dateExampleRelTests = [("x:=S where true", Right s),
                           ("x:=S where CITY = \"London\"", restrict (\(RelationTuple tupMap) -> tupMap M.! "CITY" == StringAtom "London") s),
                           ("x:=S where false", Right $ Relation (attributes s) HS.empty),
                           ("a:=S; update a (STATUS:=50); x:=a{STATUS}", mkRelation (M.fromList [("STATUS", Attribute "STATUS" IntAtomType)]) (HS.fromList [RelationTuple $ M.fromList [("STATUS", IntAtom 50)]])),
                           ("x:=S; update x where SNAME=\"Blake\" (CITY:=\"Boston\")", relMap (\(RelationTuple tupMap) -> RelationTuple $ if tupMap M.! "SNAME" == StringAtom "Blake" then M.insert "CITY" (StringAtom "Boston") tupMap else tupMap) s)
                          ]

testSimple1 = TestCase $ assertTutdEqual basicDatabaseContext "true" (Right relationTrue)

assertTutdEqual databaseContext tutd expected = assertEqual tutd interpreted expected
  where
    interpreted = case interpret databaseContext tutd of 
      (Just err, _) -> Left err
      (Nothing, context) -> Right $ (relationVariables context) M.! "x"
      
transactionGraphBasicTest = TestCase $ do
    (discon, graph) <-  dateExamplesGraph
    assertEqual "validate bootstrapped graph" (validateGraph graph) Nothing
    
--add a new transaction to the graph, validate it is in the graph    
transactionGraphAddCommitTest = TestCase $ do    
    (discon@(DisconnectedTransaction firstUUID context), graph) <- dateExamplesGraph
    freshUUID <- nextRandom
    case interpret context "x:=S" of
      (Just err, newContext) -> assertFailure (show err)
      (Nothing, newContext) -> do
        let discon = newDisconnectedTransaction firstUUID newContext
        let addTrans = addDisconnectedTransaction freshUUID "master" discon graph
        case addTrans of
          Left err -> assertFailure (show err)
          Right (newTrans, graph) -> do
            assertBool "transaction in graph" (isRight $ transactionForUUID freshUUID graph)
            assertEqual "validate fresh commit with deleted S" (validateGraph graph) Nothing
            assertEqual "ensure S was deleted in newContext" (M.lookup "x" (relationVariables (transactionContext newTrans))) (Just s)
    
--test rollback, jump, branch
    
dateExamplesGraph :: IO (DisconnectedTransaction, TransactionGraph)
dateExamplesGraph = do
  firstTransactionUUID <- nextRandom                    
  let graph = bootstrapTransactionGraph firstTransactionUUID dateExamples
  let discon = newDisconnectedTransaction firstTransactionUUID dateExamples
  return (discon, graph)


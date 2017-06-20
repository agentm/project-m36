{-# LANGUAGE LambdaCase #-}
import Test.HUnit
import ProjectM36.Base
import ProjectM36.Persist (DiskSync(NoDiskSync))
import ProjectM36.TransactionGraph.Persist
import ProjectM36.TransactionGraph
import ProjectM36.Transaction
import ProjectM36.DateExamples
import ProjectM36.Client
import ProjectM36.Relation
import System.IO.Temp
import System.Exit
import Data.Either
import Data.UUID.V4 (nextRandom)
import System.FilePath
import TutorialD.Interpreter.DatabaseContextExpr
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()           
main = do 
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess
  
testList :: Test
testList = TestList [testBootstrapDB, 
                     testDBSimplePersistence, 
                     testFunctionPersistence]

{- bootstrap a database, ensure that it can be read -}
testBootstrapDB :: Test
testBootstrapDB = TestCase $ withSystemTempDirectory "m36testdb" $ \tempdir -> do
  let dbdir = tempdir </> "dbdir"
  freshId <- nextRandom
  _ <- bootstrapDatabaseDir NoDiskSync dbdir (bootstrapTransactionGraph freshId dateExamples)
  loadedGraph <- transactionGraphLoad dbdir emptyTransactionGraph Nothing
  assertBool "transactionGraphLoad" $ isRight loadedGraph

{- create a database with several transactions, ensure that all transactions can be read -}
testDBSimplePersistence :: Test
testDBSimplePersistence = TestCase $ withSystemTempDirectory "m36testdb" $ \tempdir -> do
  let dbdir = tempdir </> "dbdir"
  freshId <- nextRandom
  let graph = bootstrapTransactionGraph freshId dateExamples
  _ <- bootstrapDatabaseDir NoDiskSync dbdir graph
  case transactionForHead "master" graph of
    Nothing -> assertFailure "Failed to retrieve head transaction for master branch."
    Just headTrans -> do
          case interpretDatabaseContextExpr (concreteDatabaseContext headTrans) "x:=s" of
            Left err -> assertFailure (show err)
            Right context' -> do
              freshId' <- nextRandom
              let newdiscon = DisconnectedTransaction (transactionId headTrans) (Schemas context' M.empty) True
                  addTrans = addDisconnectedTransaction freshId' "master" newdiscon graph
              --add a transaction to the graph
              case addTrans of
                Left err -> assertFailure (show err)
                Right (_, graph') -> do
                  --persist the new graph
                  _ <- transactionGraphPersist NoDiskSync dbdir graph'
                  --reload the graph from the filesystem and confirm that the transaction is present
                  graphErr <- transactionGraphLoad dbdir emptyTransactionGraph Nothing
                  let mapEq graphArg = S.map transactionId (transactionsForGraph graphArg)
                  case graphErr of
                    Left err -> assertFailure (show err)
                    Right graph'' -> assertBool "graph equality" (mapEq graph'' == mapEq graph')
      

--only Haskell-scripted dbc and atom functions can be serialized                   
testFunctionPersistence :: Test
testFunctionPersistence = TestCase $ withSystemTempDirectory "m36testdb" $ \tempdir -> do
  let dbdir = tempdir </> "dbdir"
      connInfo = InProcessConnectionInfo (MinimalPersistence dbdir) emptyNotificationCallback []
  Right conn <- connectProjectM36 connInfo
  Right sess <- createSessionAtHead "master" conn
  let intTCons = PrimitiveTypeConstructor "Int" IntAtomType
      addfunc = AddAtomFunction "testdisk" [
        intTCons, 
        ADTypeConstructor "Either" [ADTypeConstructor "AtomFunctionError" [],
                                    intTCons]] "(\\(x:_) -> pure x) :: [Atom] -> Either AtomFunctionError Atom"
  Nothing <- executeDatabaseContextIOExpr sess conn addfunc
  Nothing <- commit sess conn
  close conn
  --re-open the connection to reload the graph
  Right conn2 <- connectProjectM36 connInfo
  Right sess2 <- createSessionAtHead "master" conn2
  
  res <- executeRelationalExpr sess2 conn2 (MakeRelationFromExprs Nothing [TupleExpr (M.singleton "a" (FunctionAtomExpr "testdisk" [NakedAtomExpr (IntAtom 3)] ()))])
  let expectedRel = mkRelationFromList (attributesFromList [Attribute "a" IntAtomType]) [[IntAtom 3]]
  assertEqual "testdisk dbc function run" expectedRel res
    
                                                                                       
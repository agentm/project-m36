{-# LANGUAGE CPP #-}
import Test.HUnit
import ProjectM36.Base
import ProjectM36.Persist (DiskSync(NoDiskSync))
import ProjectM36.RelationalExpression (resolveDBC')
import ProjectM36.TransactionGraph.Persist
import ProjectM36.IsomorphicSchema.Types
import ProjectM36.Transaction.Types as T
import ProjectM36.TransactionGraph as TG
import ProjectM36.TransactionGraph.Types
import ProjectM36.DatabaseContext
import ProjectM36.DisconnectedTransaction
import ProjectM36.DateExamples
import ProjectM36.ValueMarker
import System.IO.Temp
import System.Exit
import System.Directory
import Data.Either
import Data.UUID.V4 (nextRandom)
import System.FilePath
import TutorialD.Interpreter.DatabaseContextExpr
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time.Clock
import Data.Time.Calendar
import ProjectM36.Client as C
import ProjectM36.Relation
import ProjectM36.Transaction.Persist
import ProjectM36.DatabaseContext.Types hiding (DatabaseContextFunctionUtils(..))
import Control.Monad (forM)
import System.Random

main :: IO ()           
main = do 
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess
  
testList :: Test
testList = TestList [testBootstrapDB, 
                     testDBSimplePersistence,
                     testFunctionPersistence,
                     testMerkleHashValidation,
                     testTransactionDirectorySizeGrowth
                     ]

stamp' :: UTCTime
stamp' = UTCTime (fromGregorian 1980 01 01) (secondsToDiffTime 1000)

{- bootstrap a database, ensure that it can be read -}
testBootstrapDB :: Test
testBootstrapDB = TestCase $ withSystemTempDirectory "m36testdb" $ \tempdir -> do
  let dbdir = tempdir </> "dbdir"
  freshId <- nextRandom

  _ <- bootstrapDatabaseDir NoDiskSync dbdir (bootstrapTransactionGraph stamp' freshId (toDatabaseContext dateExamples))
  loadedGraph <- transactionGraphLoad dbdir emptyTransactionGraph Nothing
  assertBool "transactionGraphLoad" $ isRight loadedGraph

{- create a database with several transactions, ensure that all transactions can be read -}
testDBSimplePersistence :: Test
testDBSimplePersistence = TestCase $ withSystemTempDirectory "m36testdb" $ \tempdir -> do
  let dbdir = tempdir </> "dbdir"
  freshId <- nextRandom

  let graph = bootstrapTransactionGraph stamp' freshId (toDatabaseContext dateExamples)
  _ <- bootstrapDatabaseDir NoDiskSync dbdir graph
  case transactionForHead "master" graph of
    Nothing -> assertFailure "Failed to retrieve head transaction for master branch."
    Just headTrans -> 
          case interpretDatabaseContextExpr (T.concreteDatabaseContext headTrans) (transactionId headTrans) graph "x:=s" of
            Left err -> assertFailure (show err)
            Right context' -> do
              freshId' <- nextRandom
              let newdiscon = DisconnectedTransaction (transactionId headTrans) (Schemas context' emptyValue) (CurrentHeadBranch "master")
                  addTrans = addDisconnectedTransaction stamp' freshId' newdiscon graph
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

testMerkleHashValidation :: Test
testMerkleHashValidation = TestCase $
  -- add a commit and validate the hashes successfully
  withSystemTempDirectory "m36testdb" $ \tempdir -> do
  rando <- initStdGen
  let dbdir = tempdir </> "dbdir"
      connInfo = InProcessConnectionInfo (MinimalPersistence dbdir) emptyNotificationCallback [] basicDatabaseContext rando adminRoleName
  conn <- assertIOEither $ connectProjectM36 connInfo
  sess <- assertIOEither $ createSessionAtHead conn "master"
  Right _ <- executeDatabaseContextExpr sess conn (Assign "x" (ExistingRelation relationTrue))
  -- add a notification because we forgot to read/write it as part of the transaction before
  let relX = RelationVariable "x" ()
  Right _ <- executeDatabaseContextExpr sess conn (AddNotification "testnotif" relX relX relX)
  Right _ <- commit sess conn
  val <- C.validateMerkleHashes sess conn
  assertEqual "merkle success" (Right ()) val

  --read graph from disk
  conn' <- assertIOEither $ connectProjectM36 connInfo
  sess' <- assertIOEither $ createSessionAtHead conn' "master"

  val' <- C.validateMerkleHashes sess' conn'
  assertEqual "merkle read again success" (Right ()) val'

  --alter the on-disk representation and check that the hash validation fails
  eGraph <- transactionGraphLoad dbdir emptyTransactionGraph Nothing
  case eGraph of
    Left err -> assertFailure $ "failed to load graph" ++ show err
    Right graph -> do
      let trans = transactionHeadsForGraph graph M.! "master"
          updatedTrans = Transaction (transactionId trans) (transactionInfo trans) updatedSchemas
          transactionDir' = dbdir </> show (transactionId trans)
          updatedSchemas =
            case T.schemas trans of
              Schemas ctx sschemas ->
                let relVars = case resolveDBC' graph ctx relationVariables of
                                Left err -> error ("resolveDBC' failure: " <> show err)
                                Right rv -> rv
                    updatedContext = ctx {
                      relationVariables = ValueMarker (M.insert "malicious" (ExistingRelation relationFalse) relVars) }
                      in
                Schemas updatedContext sschemas
          maliciousGraph = TransactionGraph malHeads malTransSet
          malHeads = M.insert "master" updatedTrans (transactionHeadsForGraph graph)
          malTransSet = S.insert updatedTrans (transactionsForGraph graph)
          malMerkleHash = calculateMerkleHash updatedTrans maliciousGraph
          regMerkleHash = merkleHash (transactionInfo trans)
      --validate and fail
      let val'' = TG.validateMerkleHashes maliciousGraph
      assertEqual "loaded graph merkle hashes" (Left [MerkleValidationError (transactionId trans) regMerkleHash malMerkleHash]) val''
      --delete existing transaction directory
      removeDirectoryRecursive transactionDir'
      writeTransaction NoDiskSync dbdir (UncommittedTransaction updatedTrans)

      --read graph from disk
      eConnFail <- connectProjectM36 connInfo
      case eConnFail of
        Left err -> 
          assertEqual "open connection merkle validation" (DatabaseValidationError [MerkleValidationError (transactionId trans) regMerkleHash malMerkleHash]) err
        Right _ -> assertFailure "open connection validation" 

--only Haskell-scripted dbc and atom functions can be serialized                   
testFunctionPersistence :: Test
#if !defined(PM36_HASKELL_SCRIPTING)
testFunctionPersistence = TestCase $ pure ()
#else
testFunctionPersistence = TestCase $
 withSystemTempDirectory "m36testdb" $ \tempdir -> do
  let dbdir = tempdir </> "dbdir"
      connInfo = InProcessConnectionInfo (MinimalPersistence dbdir) emptyNotificationCallback []
  conn <- assertIOEither $ connectProjectM36 connInfo
  sess <- assertIOEither $ createSessionAtHead conn "master"
  let intTCons = PrimitiveTypeConstructor "Int" IntAtomType
      addfunc = AddAtomFunction "testdisk" [
        intTCons, 
        ADTypeConstructor "Either" [ADTypeConstructor "AtomFunctionError" [],
                                    intTCons]] "(\\(x:_) -> pure x) :: [Atom] -> Either AtomFunctionError Atom"
  _ <- assertIOEither $ executeDatabaseContextIOExpr sess conn addfunc
  _ <- assertIOEither $ commit sess conn
  --close conn - pauses indefinitely on Windows
  --re-open the connection to reload the graph
  conn2 <- assertIOEither $ connectProjectM36 connInfo
  sess2 <- assertIOEither $ createSessionAtHead conn2 "master"
  
  res <- executeRelationalExpr sess2 conn2 (MakeRelationFromExprs Nothing (TupleExprs () [TupleExpr (M.singleton "a" (FunctionAtomExpr "testdisk" [NakedAtomExpr (IntAtom 3)] ()))]))
  let expectedRel = mkRelationFromList (attributesFromList [Attribute "a" IntAtomType]) [[IntAtom 3]]
  assertEqual "testdisk dbc function run" expectedRel res

#endif

assertIOEither :: (Show a) => IO (Either a b) -> IO b
assertIOEither x = do
  ret <- x
  case ret of
    Left err -> assertFailure (show err) >> undefined
    Right val -> pure val

-- test that the transaction graph sizes on disk are within expected bounds
-- create an empty database, check its size, add a commit, check the difference in size
testTransactionDirectorySizeGrowth :: Test
testTransactionDirectorySizeGrowth = TestCase $ do
  withSystemTempDirectory "m36testdb" $ \tempdir -> do
    rando <- initStdGen
    Right db <- C.connectProjectM36 (InProcessConnectionInfo (MinimalPersistence (tempdir </> "db")) emptyNotificationCallback [] basicDatabaseContext rando adminRoleName)
    Right sessionId <- C.createSessionAtHead db "master"

    originalDBSize <- getDirectorySize tempdir
    let expectedOriginalSize = 40960 {- sqlite3 loginroles.db -} + 1415
    
    assertBool ("graph with one transaction size- expected: " <> show expectedOriginalSize <> ", actual: " <> show originalDBSize) (originalDBSize <= expectedOriginalSize)

    Right () <- C.executeDatabaseContextExpr sessionId db (Assign "x" (RelationVariable "true" ()))
    Right () <- C.commit sessionId db

    postUpdateDBSize <- getDirectorySize tempdir
    let postUpdateExpectedDiffSize = 1591
        sizeDiff = postUpdateDBSize - originalDBSize 
    assertBool ("graph with two transaction size: " <> show postUpdateDBSize) (sizeDiff <= postUpdateExpectedDiffSize)
        

getDirectorySize :: FilePath -> IO Integer
getDirectorySize dir = do
    contents <- getDirectoryContents dir
    let filteredContents = filter (`notElem` [".", ".."]) contents
    sizes <- forM filteredContents $ \name -> do
        let path' = dir </> name
        isDir <- doesDirectoryExist path'
        if isDir
            then getDirectorySize path'
            else getFileSize path' 
    pure $ sum sizes        

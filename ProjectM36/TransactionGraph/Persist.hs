{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.TransactionGraph.Persist where
import ProjectM36.Error
import ProjectM36.TransactionGraph
import ProjectM36.Transaction
import ProjectM36.Transaction.Persist
import ProjectM36.Base
import System.Directory
import System.Posix.Files
import System.FilePath
import System.IO.Temp
import Data.List
import qualified Data.UUID as U
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad (foldM)
import Data.Either (isRight)
import Data.Maybe (catMaybes)

{-
The "m36v1" file at the top-level of the destination directory contains the the transaction graph as a set of UUIDs referencing their parents (1 or more)
Each Transaction is written to it own directory named by its UUID. Partially written transactions UUIDs are prefixed with a "." to indicate incompleteness in the graph.

-}

transactionLogPath :: FilePath -> FilePath
transactionLogPath = (</>) "m36v1"

headsDir :: FilePath -> FilePath
headsDir = (</>) "heads"

{-
verify that the database directory is valid or bootstrap it 
-note: checking for the existence of every transaction may be prohibitively expensive
-}

setupDatabaseDir :: FilePath -> TransactionGraph -> IO (Maybe PersistenceError)
setupDatabaseDir dbdir bootstrapGraph = do
  dbdirExists <- doesDirectoryExist dbdir
  m36exists <- doesFileExist (transactionLogPath dbdir)  
  if dbdirExists && m36exists then
    return Nothing
    else if not m36exists then do
           bootstrapDatabaseDir dbdir bootstrapGraph
           return Nothing
         else
           return $ Just (InvalidDirectoryError dbdir)

{- 
initialize a database directory with the graph from which to bootstrap
-}
bootstrapDatabaseDir :: FilePath -> TransactionGraph -> IO ()
bootstrapDatabaseDir dbdir bootstrapGraph = do
  createDirectory dbdir
  transactionGraphPersist dbdir bootstrapGraph

{- 
incrementally updates an existing database directory
--algorithm: 
-check that all heads have been written
-assume that all non-head transactions have already been written because this is an incremental (and concurrent!) write method
--store the head names with a symlink to the transaction under "heads"
-}
transactionGraphPersist :: FilePath -> TransactionGraph -> IO ()
transactionGraphPersist destDirectory graph = do
  mapM_ (writeTransaction destDirectory) $ M.elems (transactionHeadsForGraph graph)
  writeGraphUUIDFile destDirectory graph
  transactionGraphHeadsPersist destDirectory graph
  return ()
  
{- 
write a head datum to the file system
1. create a symlink in a temporary directory
2. atomically rename it
-}
--should updating all the heads be atomic (create temp directory and new symlinks for every persistence?
transactionGraphHeadsPersist :: FilePath -> TransactionGraph -> IO ()
transactionGraphHeadsPersist dbdir graph = do
  let writeHead headsPath (headName, trans) = do
        let symlinkPath = ".." </> U.toString (transactionUUID trans)
        createSymbolicLink symlinkPath (headsPath </> T.unpack headName)
      finalHeadsDir = headsDir dbdir
  withTempDirectory dbdir ".headsdir.XXXXX" $ \newHeadsDir -> do
                                                             let tempHeadsDir = (newHeadsDir </> "heads")
                                                             createDirectory tempHeadsDir
                                                             mapM_ (writeHead tempHeadsDir) $ M.toList (transactionHeadsForGraph graph)
                                                             --move the new heads dir into place atomically
                                                             renameDirectory tempHeadsDir finalHeadsDir
                                                        
  
{-  
load any transactions which are not already part of the incoming transaction graph
-}

--ALERT I need to figure out how to manage the transaction heads (branch names)
transactionGraphLoad :: FilePath -> TransactionGraph -> IO (Either PersistenceError TransactionGraph)
transactionGraphLoad dbdir graphIn = do
  --optimization: perform tail-bisection search to find last-recorded transaction in the existing stream- replay the rest
  --read in all missing transactions from transaction directories and add to graph
  uuidInfo <- readGraphUUIDFile dbdir
  case uuidInfo of
    Left err -> return $ Left err
    Right info -> do  
      let folder = \eitherGraph transUUID -> case eitherGraph of
            Left err -> return $ Left err
            Right graph -> readTransactionIfNecessary dbdir transUUID graph
      foldM folder (Right graphIn) (map fst info)
  
{-  
if the transaction with the UUID argument is not yet part of the graph, then read the transaction and add it - this does not update the heads
-}
readTransactionIfNecessary :: FilePath -> U.UUID -> TransactionGraph -> IO (Either PersistenceError TransactionGraph)  
readTransactionIfNecessary dbdir transUUID graphIn = do
  if isRight $ transactionForUUID transUUID graphIn then
    --the transaction is already known and loaded- done
    return $ Right graphIn
    else do
    trans <- readTransaction dbdir transUUID
    case trans of
      Left err -> return $ Left err
      Right trans' -> return $ Right $ TransactionGraph (transactionHeadsForGraph graphIn) (S.insert trans' (transactionsForGraph graphIn))
  
writeGraphUUIDFile :: FilePath -> TransactionGraph -> IO ()
writeGraphUUIDFile destDirectory (TransactionGraph _ transSet) = writeFile graphFile uuidInfo 
  where
    graphFile = destDirectory </> "m36v1"
    uuidInfo = intercalate "\n" graphLines
    graphLines = S.toList $ S.map graphLine transSet 
    graphLine trans = U.toString (transactionUUID trans) ++ " " ++ intercalate " " (S.toList (S.map U.toString $ transactionParentUUIDs trans))
    
readGraphUUIDFile :: FilePath -> IO (Either PersistenceError [(U.UUID, [U.UUID])])
readGraphUUIDFile dbdir = do
  --read in all transactions' uuids
  let grapher line = let uuids = catMaybes (map U.fromString (words line)) in
        (head uuids, tail uuids)
  --warning: uses lazy IO
  graphUUIDData <- readFile (transactionLogPath dbdir)
  return $ Right (map grapher $ lines graphUUIDData)


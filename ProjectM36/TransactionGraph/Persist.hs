{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.TransactionGraph.Persist where
import ProjectM36.Error
import ProjectM36.TransactionGraph
import ProjectM36.Transaction
import ProjectM36.Transaction.Persist
import ProjectM36.Base
import ProjectM36.Persist (writeFileSync, renameSync, DiskSync)
import System.Directory
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

Persistence requires a POSIX-compliant, journaled-metadata filesystem.
-}

transactionLogPath :: FilePath -> FilePath
transactionLogPath dbdir = dbdir </> "m36v1"

headsPath :: FilePath -> FilePath
headsPath dbdir = dbdir </> "heads"

{-
verify that the database directory is valid or bootstrap it 
-note: checking for the existence of every transaction may be prohibitively expensive
-}

setupDatabaseDir :: DiskSync -> FilePath -> TransactionGraph -> IO (Maybe PersistenceError)
setupDatabaseDir sync dbdir bootstrapGraph = do
  dbdirExists <- doesDirectoryExist dbdir
  m36exists <- doesFileExist (transactionLogPath dbdir)  
  if dbdirExists && m36exists then
    return Nothing
    else if not m36exists then do
           bootstrapDatabaseDir sync dbdir bootstrapGraph
           return Nothing
         else
           return $ Just (InvalidDirectoryError dbdir)

{- 
initialize a database directory with the graph from which to bootstrap
-}
bootstrapDatabaseDir :: DiskSync -> FilePath -> TransactionGraph -> IO ()
bootstrapDatabaseDir sync dbdir bootstrapGraph = do
  createDirectory dbdir
  transactionGraphPersist sync dbdir bootstrapGraph
  putStrLn "Bootstrapped DB."


{- 
incrementally updates an existing database directory
--algorithm: 
-check that all heads have been written
-assume that all non-head transactions have already been written because this is an incremental (and concurrent!) write method
--store the head names with a symlink to the transaction under "heads"
-}
transactionGraphPersist :: DiskSync -> FilePath -> TransactionGraph -> IO ()
transactionGraphPersist sync destDirectory graph = do
  mapM_ (writeTransaction sync destDirectory) $ M.elems (transactionHeadsForGraph graph)
  writeGraphUUIDFile sync destDirectory graph
  transactionGraphHeadsPersist sync destDirectory graph
  return ()
  
{- 
write graph heads to a file which can be atomically swapped
-}
--writing the heads in a directory is a synchronization nightmare, so just write the binary to a file and swap atomically
transactionGraphHeadsPersist :: DiskSync -> FilePath -> TransactionGraph -> IO ()
transactionGraphHeadsPersist sync dbdir graph = do
  let headFileStr :: (HeadName, Transaction) -> String
      headFileStr (headName, trans) =  T.unpack headName ++ " " ++ U.toString (transactionUUID trans)
  withTempDirectory dbdir ".heads.tmp" $ \tempHeadsDir -> do
    let tempHeadsPath = tempHeadsDir </> "heads"
        headsStrLines = map headFileStr $ M.toList (transactionHeadsForGraph graph)
    writeFileSync sync tempHeadsPath $ intercalate "\n" headsStrLines
    renameSync sync tempHeadsPath (headsPath dbdir)
                                                             
transactionGraphHeadsLoad :: FilePath -> IO [(HeadName,U.UUID)]
transactionGraphHeadsLoad dbdir = do
  headsData <- readFile (headsPath dbdir)
  let headsAssocs = map (\l -> let headName:uuidStr:[] = words l in
                          (headName,uuidStr)
                          ) (lines headsData)
  return [(T.pack headName, uuid) | (headName, Just uuid) <- map (\(h,u) -> (h, U.fromString u)) headsAssocs]
  
{-  
load any transactions which are not already part of the incoming transaction graph
-}

--ALERT I need to figure out how to manage the transaction heads (branch names)
transactionGraphLoad :: FilePath -> TransactionGraph -> IO (Either PersistenceError TransactionGraph)
transactionGraphLoad dbdir graphIn = do
  --optimization: perform tail-bisection search to find last-recorded transaction in the existing stream- replay the rest
  --read in all missing transactions from transaction directories and add to graph
  uuidInfo <- readGraphUUIDFile dbdir
  freshHeadsAssoc <- transactionGraphHeadsLoad dbdir
  case uuidInfo of
    Left err -> return $ Left err
    Right info -> do  
      let folder = \eitherGraph transUUID -> case eitherGraph of
            Left err -> return $ Left err
            Right graph -> readTransactionIfNecessary dbdir transUUID graph
      loadedGraph <- foldM folder (Right graphIn) (map fst info)
      case loadedGraph of 
        Left err -> return $ Left err
        Right freshGraph -> do
          let maybeTransHeads = [(headName, transactionForUUID uuid freshGraph) | (headName, uuid) <- freshHeadsAssoc]
              freshHeads = M.fromList [(headName,trans) | (headName, Right trans) <- maybeTransHeads]
          return $ Right $ TransactionGraph freshHeads (transactionsForGraph freshGraph)
  
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
  
writeGraphUUIDFile :: DiskSync -> FilePath -> TransactionGraph -> IO ()
writeGraphUUIDFile sync destDirectory (TransactionGraph _ transSet) = writeFileSync sync graphFile uuidInfo 
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


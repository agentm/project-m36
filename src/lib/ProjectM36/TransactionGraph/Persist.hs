module ProjectM36.TransactionGraph.Persist where
import ProjectM36.Error
import ProjectM36.TransactionGraph
import ProjectM36.Transaction
import ProjectM36.Transaction.Persist
import ProjectM36.Base
import ProjectM36.ScriptSession
import ProjectM36.Persist (writeFileSync, renameSync, DiskSync)
import ProjectM36.FileLock
import System.Directory
import System.FilePath
import System.IO.Temp
import System.IO
import qualified Data.UUID as U
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Encoding
import Control.Monad (foldM)
import Data.Either (isRight)
import Data.Maybe (mapMaybe)
import Control.Exception.Base
import qualified Data.Text.IO as TIO
import Data.ByteString (ByteString)
import Data.Monoid
import qualified Crypto.Hash.SHA256 as SHA256
import Control.Arrow

type LockFileHash = ByteString

{-
The "m36v1" file at the top-level of the destination directory contains the the transaction graph as a set of transaction ids referencing their parents (1 or more)
Each Transaction is written to it own directory named by its transaction id. Partially written transactions ids are prefixed with a "." to indicate incompleteness in the graph.

Persistence requires a POSIX-compliant, journaled-metadata filesystem.
-}

transactionLogPath :: FilePath -> FilePath
transactionLogPath dbdir = dbdir </> "m36v1"

headsPath :: FilePath -> FilePath
headsPath dbdir = dbdir </> "heads"

lockFilePath :: FilePath -> FilePath
lockFilePath dbdir = dbdir </> "lockFile"

{-
verify that the database directory is valid or bootstrap it 
-note: checking for the existence of every transaction may be prohibitively expensive
- return error or lock file handle which is already locked with a read lock
-}

setupDatabaseDir :: DiskSync -> FilePath -> TransactionGraph -> IO (Either PersistenceError (Handle, LockFileHash))
setupDatabaseDir sync dbdir bootstrapGraph = do
  dbdirExists <- doesDirectoryExist dbdir
  m36exists <- doesFileExist (transactionLogPath dbdir)  
  if dbdirExists && m36exists then do
      --no directories to write, just 
      lockFileH <- openLockFile dbdir
      gDigest <- bracket_ (lockFile lockFileH WriteLock) (unlockFile lockFileH) (readGraphTransactionIdFileDigest dbdir)
      pure (Right (lockFileH, gDigest))
    else if not m36exists then do
    locks <- bootstrapDatabaseDir sync dbdir bootstrapGraph
    pure (Right locks)
         else
           pure (Left (InvalidDirectoryError dbdir))
{- 
initialize a database directory with the graph from which to bootstrap- return lock file handle
-}
bootstrapDatabaseDir :: DiskSync -> FilePath -> TransactionGraph -> IO (Handle, LockFileHash)
bootstrapDatabaseDir sync dbdir bootstrapGraph = do
  createDirectory dbdir
  lockFileH <- openLockFile dbdir
  let allTransIds = map transactionId (S.toList (transactionsForGraph bootstrapGraph))
  digest  <- bracket_ (lockFile lockFileH WriteLock) (unlockFile lockFileH) (transactionGraphPersist sync dbdir allTransIds bootstrapGraph)
  pure (lockFileH, digest)
  
openLockFile :: FilePath -> IO Handle
openLockFile dbdir = do
  lockFileH <- openFile (lockFilePath dbdir) WriteMode
  pure lockFileH

{- 
incrementally updates an existing database directory
--algorithm: 
--assume that all transaction data has already been written
-assume that all non-head transactions have already been written because this is an incremental (and concurrent!) write method
--store the head names with a symlink to the transaction under "heads"
-}
transactionGraphPersist :: DiskSync -> FilePath -> [TransactionId] -> TransactionGraph -> IO LockFileHash
transactionGraphPersist sync destDirectory transIds graph = do
  transactionsPersist sync transIds destDirectory graph
  --write graph file
  newDigest <- writeGraphTransactionIdFile sync destDirectory graph
  --write heads file
  transactionGraphHeadsPersist sync destDirectory graph
  pure newDigest
  
-- | The incremental writer writes the transactions ids specified by the second argument.

-- There was a bug here via #128 because automerge added multiple transactions to the graph but this function used to only write the head transactions from the graph. Automerge creates multiple transactions, so these are now passed in as the second argument.
transactionsPersist :: DiskSync -> [TransactionId] -> FilePath -> TransactionGraph -> IO ()
transactionsPersist sync transIds destDirectory graphIn = mapM_ writeTrans transIds
  where writeTrans tid =
          case transactionForId tid graphIn of 
            Left err -> error ("writeTransaction: " ++ show err)
            Right trans -> writeTransaction sync destDirectory trans

{- 
write graph heads to a file which can be atomically swapped
-}
--writing the heads in a directory is a synchronization nightmare, so just write the binary to a file and swap atomically
transactionGraphHeadsPersist :: DiskSync -> FilePath -> TransactionGraph -> IO ()
transactionGraphHeadsPersist sync dbdir graph = do
  let headFileStr :: (HeadName, Transaction) -> T.Text
      headFileStr (headName, trans) =  headName <> " " <> U.toText (transactionId trans)
  withTempDirectory dbdir ".heads.tmp" $ \tempHeadsDir -> do
    let tempHeadsPath = tempHeadsDir </> "heads"
        headsStrLines = map headFileStr $ M.toList (transactionHeadsForGraph graph)
    writeFileSync sync tempHeadsPath $ T.intercalate "\n" headsStrLines
    renameSync sync tempHeadsPath (headsPath dbdir)
                                                             
transactionGraphHeadsLoad :: FilePath -> IO [(HeadName,TransactionId)]
transactionGraphHeadsLoad dbdir = do
  headsData <- readFile (headsPath dbdir)
  let headsAssocs = map (\l -> let [headName, uuidStr] = words l in
                          (headName,uuidStr)
                          ) (lines headsData)
  return [(T.pack headName, uuid) | (headName, Just uuid) <- map (second U.fromString) headsAssocs]
  
{-  
load any transactions which are not already part of the incoming transaction graph
-}

transactionGraphLoad :: FilePath -> TransactionGraph -> Maybe ScriptSession -> IO (Either PersistenceError TransactionGraph)
transactionGraphLoad dbdir graphIn mScriptSession = do
  --optimization: perform tail-bisection search to find last-recorded transaction in the existing stream- replay the rest
  --read in all missing transactions from transaction directories and add to graph
  uuidInfo <- readGraphTransactionIdFile dbdir
  freshHeadsAssoc <- transactionGraphHeadsLoad dbdir
  case uuidInfo of
    Left err -> return $ Left err
    Right info -> do  
      let folder eitherGraph transId = case eitherGraph of
            Left err -> return $ Left err
            Right graph -> readTransactionIfNecessary dbdir transId mScriptSession graph
      loadedGraph <- foldM folder (Right graphIn) (map fst info)
      case loadedGraph of 
        Left err -> return $ Left err
        Right freshGraph -> do
          let maybeTransHeads = [(headName, transactionForId uuid freshGraph) | (headName, uuid) <- freshHeadsAssoc]
              freshHeads = M.fromList [(headName,trans) | (headName, Right trans) <- maybeTransHeads]
          return $ Right $ TransactionGraph freshHeads (transactionsForGraph freshGraph)
  
{-  
if the transaction with the TransactionId argument is not yet part of the graph, then read the transaction and add it - this does not update the heads
-}
readTransactionIfNecessary :: FilePath -> TransactionId -> Maybe ScriptSession -> TransactionGraph -> IO (Either PersistenceError TransactionGraph)  
readTransactionIfNecessary dbdir transId mScriptSession graphIn =
  if isRight $ transactionForId transId graphIn then
    --the transaction is already known and loaded- done
    return $ Right graphIn
    else do
    trans <- readTransaction dbdir transId mScriptSession
    case trans of
      Left err -> return $ Left err
      Right trans' -> return $ Right $ TransactionGraph (transactionHeadsForGraph graphIn) (S.insert trans' (transactionsForGraph graphIn))
  
writeGraphTransactionIdFile :: DiskSync -> FilePath -> TransactionGraph -> IO LockFileHash
writeGraphTransactionIdFile sync destDirectory (TransactionGraph _ transSet) = writeFileSync sync graphFile uuidInfo >> pure digest
  where
    graphFile = destDirectory </> "m36v1"
    uuidInfo = T.intercalate "\n" graphLines
    digest = SHA256.hash (encodeUtf8 uuidInfo)
    graphLines = S.toList $ S.map graphLine transSet 
    graphLine trans = U.toText (transactionId trans) <> " " <> T.intercalate " " (S.toList (S.map U.toText $ transactionParentIds trans))
    
readGraphTransactionIdFileDigest :: FilePath -> IO LockFileHash
readGraphTransactionIdFileDigest dbdir = do
  graphTransactionIdData <- TIO.readFile (transactionLogPath dbdir)
  pure (SHA256.hash (encodeUtf8 graphTransactionIdData))
    
readGraphTransactionIdFile :: FilePath -> IO (Either PersistenceError [(TransactionId, [TransactionId])])
readGraphTransactionIdFile dbdir = do
  --read in all transactions' uuids
  let grapher line = let tids = mapMaybe U.fromText (T.words line) in
        (head tids, tail tids)
  --warning: uses lazy IO
  graphTransactionIdData <- TIO.readFile (transactionLogPath dbdir)
  return $ Right (map grapher $ T.lines graphTransactionIdData)


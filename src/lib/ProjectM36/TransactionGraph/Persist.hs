module ProjectM36.TransactionGraph.Persist where
import ProjectM36.Error
import ProjectM36.Transaction
import ProjectM36.Transaction.Persist
import ProjectM36.RelationalExpression
import ProjectM36.Base
import ProjectM36.ScriptSession
import ProjectM36.Persist (writeFileSync, renameSync, DiskSync)
import ProjectM36.FileLock
import System.Directory
import System.FilePath
import System.IO.Temp
import Data.Time.Clock.POSIX
import qualified Data.UUID as U
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Encoding
import Control.Monad (foldM)
import Data.Either (isRight)
import Data.Maybe (fromMaybe)
import qualified Data.List as L
import Control.Exception.Base
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import qualified Crypto.Hash.SHA256 as SHA256
import Control.Arrow
import Data.Time.Clock
import Data.Text.Read
import System.FilePath.Glob
#if __GLASGOW_HASKELL__ <= 802
import Data.Monoid
#endif

type LockFileHash = ByteString

{-
The "m36vX" file at the top-level of the destination directory contains the the transaction graph as a set of transaction ids referencing their parents (1 or more)
Each Transaction is written to it own directory named by its transaction id. Partially written transactions ids are prefixed with a "." to indicate incompleteness in the graph.

Persistence requires a POSIX-compliant, journaled-metadata filesystem.
-}

expectedVersion :: Int
expectedVersion = 5

transactionLogFileName :: FilePath 
transactionLogFileName = "m36v" ++ show expectedVersion

transactionLogPath :: FilePath -> FilePath
transactionLogPath dbdir = dbdir </> transactionLogFileName

headsPath :: FilePath -> FilePath
headsPath dbdir = dbdir </> "heads"

lockFilePath :: FilePath -> FilePath
lockFilePath dbdir = dbdir </> "lockFile"

{-
verify that the database directory is valid or bootstrap it 
-note: checking for the existence of every transaction may be prohibitively expensive
- return error or lock file handle which is already locked with a read lock
-}

checkForOtherVersions :: FilePath -> IO (Either PersistenceError ())
checkForOtherVersions dbdir = do
  versionMatches <- globDir1 (compile "m36v*") dbdir
  let otherVersions = L.delete transactionLogFileName (map takeFileName versionMatches)
  if not (null otherVersions) then
     pure (Left (WrongDatabaseFormatVersionError transactionLogFileName (head otherVersions)))
     else
     pure (Right ())
 

setupDatabaseDir :: DiskSync -> FilePath -> TransactionGraph -> IO (Either PersistenceError (LockFile, LockFileHash))
setupDatabaseDir sync dbdir bootstrapGraph = do
  dbdirExists <- doesDirectoryExist dbdir
  eWrongVersion <- checkForOtherVersions dbdir
  case eWrongVersion of
    Left err -> pure (Left err)
    Right () -> do
      m36exists <- doesFileExist (transactionLogPath dbdir)  
      if dbdirExists && m36exists then do
        --no directories to write, just 
        locker <- openLockFile (lockFilePath dbdir)
        gDigest <- bracket_ (lockFile locker WriteLock) (unlockFile locker) (readGraphTransactionIdFileDigest dbdir)
        pure (Right (locker, gDigest))
      else if not m36exists then 
        Right <$> bootstrapDatabaseDir sync dbdir bootstrapGraph
         else
           pure (Left (InvalidDirectoryError dbdir))
{- 
initialize a database directory with the graph from which to bootstrap- return lock file handle which must be closed by the caller
-}
bootstrapDatabaseDir :: DiskSync -> FilePath -> TransactionGraph -> IO (LockFile, LockFileHash)
bootstrapDatabaseDir sync dbdir bootstrapGraph = do
  createDirectory dbdir
  locker <- openLockFile (lockFilePath dbdir)
  let allTransIds = map transactionId (S.toList (transactionsForGraph bootstrapGraph))
  digest  <- bracket_ (lockFile locker WriteLock) (unlockFile locker) (transactionGraphPersist sync dbdir allTransIds bootstrapGraph)
  pure (locker, digest)
  
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
      loadedGraph <- foldM folder (Right graphIn) (map (\(tid,_,_) -> tid) info)
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
    graphFile = transactionLogPath destDirectory
    uuidInfo = T.intercalate "\n" graphLines
    digest = SHA256.hash (encodeUtf8 uuidInfo)
    graphLines = S.toList $ S.map graphLine transSet 
    epochTime = realToFrac . utcTimeToPOSIXSeconds . timestamp :: Transaction -> Double
    graphLine trans = U.toText (transactionId trans) 
                      <> " " 
                      <> T.pack (show (epochTime trans))
                      <> " "
                      <> T.intercalate " " (S.toList (S.map U.toText $ parentIds trans))
    
readGraphTransactionIdFileDigest :: FilePath -> IO LockFileHash
readGraphTransactionIdFileDigest dbdir = do
  let graphTransactionIdData = readUTF8FileOrError (transactionLogPath dbdir)
  SHA256.hash . encodeUtf8 <$> graphTransactionIdData
    
readGraphTransactionIdFile :: FilePath -> IO (Either PersistenceError [(TransactionId, UTCTime, [TransactionId])])
readGraphTransactionIdFile dbdir = do
  --read in all transactions' uuids
  let grapher line = let tid:epochText:parentIds' = T.words line in
        (readUUID tid, readEpoch epochText, map readUUID parentIds')
      readUUID uuidText = fromMaybe (error "failed to read uuid") (U.fromText uuidText)
      readEpoch t = posixSecondsToUTCTime (realToFrac (either (error "failed to read epoch") fst (double t)))
  Right . map grapher . T.lines <$> readUTF8FileOrError (transactionLogPath dbdir)

--rationale- reading essential database files must fail hard
readUTF8FileOrError :: FilePath -> IO T.Text
readUTF8FileOrError pathIn = do
  eFileBytes <- try (BS.readFile pathIn) :: IO (Either IOError BS.ByteString)
  case eFileBytes of 
    Left err -> error (show err)
    Right fileBytes ->
      case TE.decodeUtf8' fileBytes of
        Left err -> error (show err)
        Right utf8Bytes -> pure utf8Bytes


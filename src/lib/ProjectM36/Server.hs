{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module ProjectM36.Server where

import ProjectM36.Client
import ProjectM36.Server.EntryPoints 
import ProjectM36.Server.RemoteCallTypes
import ProjectM36.Server.Config (ServerConfig(..))
import ProjectM36.FSType
import ProjectM36.Server.Types

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (putMVar, MVar)
import System.IO (stderr, hPutStrLn)
import System.FilePath (takeDirectory)
import System.Directory (doesDirectoryExist)
import Network.RPC.Curryer.Server
import Network.Socket
import qualified StmContainers.Map as StmMap

-- the state should be a mapping of remote connection to the disconnected transaction- the graph should be the same, so discon must be removed from the stm tuple
--trying to refactor this for less repetition is very challenging because the return type cannot be polymorphic or the distributed-process call gets confused and drops messages
type TestMode = Bool

requestHandlers :: TestMode -> Maybe Timeout -> RequestHandlers ServerState
requestHandlers testBool ti =
  [
    RequestHandler $ \conn (ExecuteHeadName sessionId) -> handleExecuteHeadName ti sessionId conn,
     RequestHandler (\conn (ExecuteRelationalExpr sessionId expr) -> handleExecuteRelationalExpr ti sessionId conn expr),
     RequestHandler (\conn (ExecuteDataFrameExpr sessionId expr) -> handleExecuteDataFrameExpr ti sessionId conn expr),     
     RequestHandler (\conn (ExecuteDatabaseContextExpr sessionId expr) -> handleExecuteDatabaseContextExpr ti sessionId conn expr),
     RequestHandler (\conn (ExecuteDatabaseContextIOExpr sessionId expr) -> handleExecuteDatabaseContextIOExpr ti sessionId conn expr),
     RequestHandler (\conn (ExecuteGraphExpr sessionId expr) -> handleExecuteGraphExpr ti sessionId conn expr),
     RequestHandler (\conn (ExecuteTransGraphRelationalExpr sessionId expr) -> handleExecuteTransGraphRelationalExpr ti sessionId conn expr),     
     RequestHandler (\conn (ExecuteTypeForRelationalExpr sessionId expr) -> handleExecuteTypeForRelationalExpr ti sessionId conn expr),
     RequestHandler (\conn (RetrieveInclusionDependencies sessionId) -> handleRetrieveInclusionDependencies ti sessionId conn),
     RequestHandler (\conn (RetrievePlanForDatabaseContextExpr sessionId dbExpr) -> handleRetrievePlanForDatabaseContextExpr ti sessionId conn dbExpr),
     RequestHandler (\conn (RetrieveHeadTransactionId sessionId) -> handleRetrieveHeadTransactionId ti sessionId conn),
     RequestHandler (\conn (RetrieveTransactionGraph sessionId) -> handleRetrieveTransactionGraph ti sessionId conn),
     RequestHandler (\conn (Login dbName) -> handleLogin dbName ti conn),
     RequestHandler (\conn (CreateSessionAtHead headn) -> handleCreateSessionAtHead ti conn headn),
     RequestHandler (\conn (CreateSessionAtCommit commitId) -> handleCreateSessionAtCommit ti conn commitId),
     RequestHandler (\conn (CloseSession sessionId) -> handleCloseSession ti sessionId conn),
     RequestHandler (\conn (RetrieveAtomTypesAsRelation sessionId) -> handleRetrieveAtomTypesAsRelation ti sessionId conn),
     RequestHandler (\conn (RetrieveRelationVariableSummary sessionId) -> handleRetrieveRelationVariableSummary ti sessionId conn),
     RequestHandler (\conn (RetrieveAtomFunctionSummary sessionId) -> handleRetrieveAtomFunctionSummary ti sessionId conn),
     RequestHandler (\conn (RetrieveDatabaseContextFunctionSummary sessionId) -> handleRetrieveDatabaseContextFunctionSummary ti sessionId conn),     
     RequestHandler (\conn (RetrieveCurrentSchemaName sessionId) -> handleRetrieveCurrentSchemaName ti sessionId conn),
     RequestHandler (\conn (ExecuteSchemaExpr sessionId schemaExpr) -> handleExecuteSchemaExpr ti sessionId conn schemaExpr),
     RequestHandler (\conn (RetrieveSessionIsDirty sessionId) -> handleRetrieveSessionIsDirty ti sessionId conn),
     RequestHandler (\conn (ExecuteAutoMergeToHead sessionId strat headName') -> handleExecuteAutoMergeToHead ti sessionId conn strat headName'),
     RequestHandler (\conn (RetrieveTypeConstructorMapping sessionId) -> handleRetrieveTypeConstructorMapping ti sessionId conn),
     RequestHandler (\conn (ExecuteValidateMerkleHashes sessionId) -> handleValidateMerkleHashes ti sessionId conn),
     RequestHandler (\conn Logout -> handleLogout ti conn)
     ] ++ if testMode then testModeHandlers else []
    
testModeHandlers :: Maybe Timeout -> RequestHandlers ServerState
testModeHandlers ti = [RequestHandler (\conn (TestTimeout sessionId) -> handleTestTimeout ti sessionId conn)]
    
                 
-- | A notification callback which logs the notification to stderr and does nothing else.
loggingNotificationCallback :: NotificationCallback
loggingNotificationCallback notName evaldNot = hPutStrLn stderr $ "Notification received \"" ++ show notName ++ "\": " ++ show evaldNot

checkFSType :: Bool -> PersistenceStrategy -> IO Bool  
checkFSType performCheck strat = 
  case strat of 
    NoPersistence -> pure True
    MinimalPersistence _ -> pure True
    CrashSafePersistence path -> 
      if performCheck then do
        -- if the path does not (yet) exist, then walk back a step- the db directory may not yet have been created
        fullpathexists <- doesDirectoryExist path
        let fscheckpath = if fullpathexists then
                           path
                          else
                           takeDirectory path
        fsTypeSupportsJournaling fscheckpath
      else
        pure True
        
checkFSErrorMsg :: String        
checkFSErrorMsg = "The filesystem does not support journaling so writes may not be crash-safe. Use --disable-fscheck to disable this fatal error."

type ServerState = StmMap.Map DatabaseName InProcessConnectionConf

-- | A synchronous function to start the project-m36 daemon given an appropriate 'ServerConfig'. Note that this function only returns if the server exits. Returns False if the daemon exited due to an error. If the second argument is not Nothing, the port is put after the server is ready to service the port.
launchServer :: ServerConfig -> Maybe (MVar SockAddr) -> IO Bool
launchServer daemonConfig mAddr = do
  checkFSResult <- checkFSType (checkFS daemonConfig) (persistenceStrategy daemonConfig)
  if not checkFSResult then do
    hPutStrLn stderr checkFSErrorMsg
    pure False
    else do
      econn <- connectProjectM36 (InProcessConnectionInfo (persistenceStrategy daemonConfig) loggingNotificationCallback (ghcPkgPaths daemonConfig))
      case econn of 
        Left err -> do      
          hPutStrLn stderr ("Failed to create database connection: " ++ show err)
          pure False
        Right conn -> do
          let hostname = bindHost daemonConfig
              port = bindPort daemonConfig

          --curryer only supports IPv4 for now
          let addrHints = defaultHints { addrSocketType = Stream, addrFamily = AF_INET }
          hostAddrs <- getAddrInfo (Just addrHints) (Just hostname) Nothing
          case hostAddrs of
            [] -> hPutStrLn stderr ("Failed to resolve: " <> hostname) >> pure False
            (AddrInfo _ _ _ _ (SockAddrInet _ addr32) _):_ -> do
              let hostAddr = hostAddressToTuple addr32
              initialServerState <- StmMap.newIO @DatabaseName @InProcessConnectionConf
              serve requestHandlers initialServerState hostAddr port mAddr



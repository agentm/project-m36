{-# LANGUAGE ScopedTypeVariables #-}
module ProjectM36.Server where

import ProjectM36.Client
import ProjectM36.Server.EntryPoints 
import ProjectM36.Server.RemoteCallTypes
import ProjectM36.Server.Config (ServerConfig(..))
import ProjectM36.FSType

import Control.Monad.IO.Class (liftIO)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Network.Transport (EndPointAddress(..), newEndPoint, address)
import Control.Distributed.Process.Node (initRemoteTable, runProcess, newLocalNode, initRemoteTable)
import Control.Distributed.Process.Extras.Time (Delay(..))
import Control.Distributed.Process (Process, register, getSelfPid)
import Control.Distributed.Process.ManagedProcess (defaultProcess, UnhandledMessagePolicy(..), ProcessDefinition(..), handleCall, serve, InitHandler, InitResult(..))
import Control.Concurrent.MVar (putMVar, MVar)
import System.IO (stderr, hPutStrLn)
import System.FilePath (takeDirectory)
import System.Directory (doesDirectoryExist)

-- the state should be a mapping of remote connection to the disconnected transaction- the graph should be the same, so discon must be removed from the stm tuple
--trying to refactor this for less repetition is very challenging because the return type cannot be polymorphic or the distributed-process call gets confused and drops messages
serverDefinition :: Bool -> Timeout -> ProcessDefinition Connection
serverDefinition testBool ti = defaultProcess {
  apiHandlers = [                 
     handleCall (\conn (ExecuteHeadName sessionId) -> handleExecuteHeadName ti sessionId conn),
     handleCall (\conn (ExecuteRelationalExpr sessionId expr) -> handleExecuteRelationalExpr ti sessionId conn expr),
     handleCall (\conn (ExecuteDatabaseContextExpr sessionId expr) -> handleExecuteDatabaseContextExpr ti sessionId conn expr),
     handleCall (\conn (ExecuteDatabaseContextIOExpr sessionId expr) -> handleExecuteDatabaseContextIOExpr ti sessionId conn expr),
     handleCall (\conn (ExecuteGraphExpr sessionId expr) -> handleExecuteGraphExpr ti sessionId conn expr),
     handleCall (\conn (ExecuteTransGraphRelationalExpr sessionId expr) -> handleExecuteTransGraphRelationalExpr ti sessionId conn expr),     
     handleCall (\conn (ExecuteTypeForRelationalExpr sessionId expr) -> handleExecuteTypeForRelationalExpr ti sessionId conn expr),
     handleCall (\conn (RetrieveInclusionDependencies sessionId) -> handleRetrieveInclusionDependencies ti sessionId conn),
     handleCall (\conn (RetrievePlanForDatabaseContextExpr sessionId dbExpr) -> handleRetrievePlanForDatabaseContextExpr ti sessionId conn dbExpr),
     handleCall (\conn (RetrieveHeadTransactionId sessionId) -> handleRetrieveHeadTransactionId ti sessionId conn),
     handleCall (\conn (RetrieveTransactionGraph sessionId) -> handleRetrieveTransactionGraph ti sessionId conn),
     handleCall (\conn (Login procId) -> handleLogin ti conn procId),
     handleCall (\conn (CreateSessionAtHead headn) -> handleCreateSessionAtHead ti conn headn),
     handleCall (\conn (CreateSessionAtCommit commitId) -> handleCreateSessionAtCommit ti conn commitId),
     handleCall (\conn (CloseSession sessionId) -> handleCloseSession ti sessionId conn),
     handleCall (\conn (RetrieveAtomTypesAsRelation sessionId) -> handleRetrieveAtomTypesAsRelation ti sessionId conn),
     handleCall (\conn (RetrieveRelationVariableSummary sessionId) -> handleRetrieveRelationVariableSummary ti sessionId conn),
     handleCall (\conn (RetrieveCurrentSchemaName sessionId) -> handleRetrieveCurrentSchemaName ti sessionId conn),
     handleCall (\conn (ExecuteSchemaExpr sessionId schemaExpr) -> handleExecuteSchemaExpr ti sessionId conn schemaExpr),
     handleCall (\conn (RetrieveSessionIsDirty sessionId) -> handleRetrieveSessionIsDirty ti sessionId conn),
     handleCall (\conn (ExecuteAutoMergeToHead sessionId strat headName') -> handleExecuteAutoMergeToHead ti sessionId conn strat headName'),
     handleCall (\conn (RetrieveTypeConstructorMapping sessionId) -> handleRetrieveTypeConstructorMapping ti sessionId conn),
     handleCall (\conn Logout -> handleLogout ti conn)
     ] ++ testModeHandlers,
  unhandledMessagePolicy = Terminate
  --unhandledMessagePolicy = Log
  }
  where
    testModeHandlers = if not testBool then
                         []
                       else
                         [handleCall (\conn (TestTimeout sessionId) -> handleTestTimeout ti sessionId conn)]
                               
                 
initServer :: InitHandler (Connection, DatabaseName, Maybe (MVar EndPointAddress), EndPointAddress) Connection
initServer (conn, dbname, mAddressMVar, saddress) = do
  registerDB dbname
  case mAddressMVar of
       Nothing -> pure ()
       Just addressMVar -> liftIO $ putMVar addressMVar saddress
  --traceShowM ("server started on " ++ show saddress)
  pure $ InitOk conn Infinity

registerDB :: DatabaseName -> Process ()
registerDB dbname = do
  self <- getSelfPid
  let dbname' = remoteDBLookupName dbname  
  register dbname' self
  --liftIO $ putStrLn $ "registered " ++ (show self) ++ " " ++ dbname'
  
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

-- | A synchronous function to start the project-m36 daemon given an appropriate 'ServerConfig'. Note that this function only returns if the server exits. Returns False if the daemon exited due to an error. If the second argument is not Nothing, the port is put after the server is ready to service the port.
launchServer :: ServerConfig -> Maybe (MVar EndPointAddress) -> IO Bool
launchServer daemonConfig mAddressMVar = do  
  econn <- connectProjectM36 (InProcessConnectionInfo (persistenceStrategy daemonConfig) loggingNotificationCallback (ghcPkgPaths daemonConfig))
  case econn of 
    Left err -> do      
      hPutStrLn stderr ("Failed to create database connection: " ++ show err)
      pure False
    Right conn -> do
      let hostname = bindHost daemonConfig
          port = bindPort daemonConfig
      etransport <- createTransport hostname (show port) (\sn -> (hostname, sn)) defaultTCPParameters
      case etransport of
        Left err -> error ("failed to create transport: " ++ show err)
        Right transport -> do
          eEndpoint <- newEndPoint transport
          case eEndpoint of 
            Left err -> hPutStrLn stderr ("Failed to create transport: " ++ show err) >> pure False
            Right endpoint -> do
              localTCPNode <- newLocalNode transport initRemoteTable
              --traceShowM ("newLocalNode in Server " ++ show (localNodeId localTCPNode))
              runProcess localTCPNode $ do
                let testBool = testMode daemonConfig
                    reqTimeout = perRequestTimeout daemonConfig
                serve (conn, databaseName daemonConfig, mAddressMVar, address endpoint) initServer (serverDefinition testBool reqTimeout)
              liftIO $ putStrLn "serve returned"
              pure True

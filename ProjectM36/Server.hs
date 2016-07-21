{-# LANGUAGE ScopedTypeVariables #-}
module ProjectM36.Server where

import ProjectM36.Client
import ProjectM36.Server.EntryPoints 
import ProjectM36.Server.RemoteCallTypes
import ProjectM36.Server.Config (ServerConfig(..))

import Control.Monad.IO.Class (liftIO)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process.Node (initRemoteTable, runProcess, newLocalNode)
import Control.Distributed.Process.Extras.Time (Delay(..))
import Control.Distributed.Process (Process, register, RemoteTable, getSelfPid)
import Control.Distributed.Process.ManagedProcess (defaultProcess, UnhandledMessagePolicy(..), ProcessDefinition(..), handleCall, serve, InitHandler, InitResult(..))
import System.IO (hPutStrLn, stderr)
import qualified Control.Distributed.Process.Extras.Internal.Types as DIT
import Control.Concurrent.MVar (putMVar, MVar)

-- the state should be a mapping of remote connection to the disconnected transaction- the graph should be the same, so discon must be removed from the stm tuple
serverDefinition :: ProcessDefinition Connection
serverDefinition = defaultProcess {
  apiHandlers = [                 
     handleCall (\conn (ExecuteHeadName sessionId) -> handleExecuteHeadName sessionId conn),
     handleCall (\conn (ExecuteRelationalExpr sessionId expr) -> handleExecuteRelationalExpr sessionId conn expr),
     handleCall (\conn (ExecuteDatabaseContextExpr sessionId expr) -> handleExecuteDatabaseContextExpr sessionId conn expr),
     handleCall (\conn (ExecuteGraphExpr sessionId expr) -> handleExecuteGraphExpr sessionId conn expr),
     handleCall (\conn (ExecuteTypeForRelationalExpr sessionId expr) -> handleExecuteTypeForRelationalExpr sessionId conn expr),
     handleCall (\conn (RetrieveInclusionDependencies sessionId) -> handleRetrieveInclusionDependencies sessionId conn),
     handleCall (\conn (RetrievePlanForDatabaseContextExpr sessionId dbExpr) -> handleRetrievePlanForDatabaseContextExpr sessionId conn dbExpr),
     handleCall (\conn (RetrieveHeadTransactionId sessionId) -> handleRetrieveHeadTransactionId sessionId conn),
     handleCall (\conn (RetrieveTransactionGraph sessionId) -> handleRetrieveTransactionGraph sessionId conn),
     handleCall (\conn (Login procId) -> handleLogin conn procId),
     handleCall (\conn (CreateSessionAtHead headn) -> handleCreateSessionAtHead headn conn),
     handleCall (\conn (CreateSessionAtCommit commitId) -> handleCreateSessionAtCommit commitId conn),
     handleCall (\conn (CloseSession sessionId) -> handleCloseSession sessionId conn),
     handleCall (\conn (RetrieveAtomTypesAsRelation sessionId) -> handleRetrieveAtomTypesAsRelation sessionId conn),
     handleCall (\conn (RetrieveRelationVariableSummary sessionId) -> handleRetrieveRelationVariableSummary sessionId conn)
     ],
  unhandledMessagePolicy = Log
  }
                 
initServer :: InitHandler (Connection, DatabaseName, Maybe (MVar Port), Port) Connection
initServer (conn, dbname, mPortMVar, portNum) = do
  registerDB dbname
  case mPortMVar of
       Nothing -> pure ()
       Just portMVar -> liftIO $ putMVar portMVar portNum
  pure $ InitOk conn Infinity

remoteTable :: RemoteTable
remoteTable = DIT.__remoteTable initRemoteTable

registerDB :: DatabaseName -> Process ()
registerDB dbname = do
  self <- getSelfPid
  let dbname' = remoteDBLookupName dbname  
  register dbname' self
  liftIO $ putStrLn $ "registered " ++ (show self) ++ " " ++ dbname'

-- | A notification callback which logs the notification to stderr and does nothing else.
loggingNotificationCallback :: NotificationCallback
loggingNotificationCallback notName evaldNot = hPutStrLn stderr $ "Notification received \"" ++ show notName ++ "\": " ++ show evaldNot

-- | A synchronous function to start the project-m36 daemon given an appropriate 'ServerConfig'. Note that this function only returns if the server exits. Returns False if the daemon exited due to an error. If the second argument is not Nothing, the port is put after the server is ready to service the port.
launchServer :: ServerConfig -> Maybe (MVar Port) -> IO (Bool)
launchServer daemonConfig mPortMVar = do  
  econn <- connectProjectM36 (InProcessConnectionInfo (persistenceStrategy daemonConfig) loggingNotificationCallback)
  case econn of 
    Left err -> do      
      hPutStrLn stderr ("Failed to create database connection: " ++ show err)
      pure False
    Right conn -> do
      let hostname = bindHost daemonConfig
          port = bindPort daemonConfig
      etransport <- createTransport hostname (show port) defaultTCPParameters
      case etransport of
        Left err -> error (show err)
        Right transport -> do
          localTCPNode <- newLocalNode transport remoteTable
          runProcess localTCPNode $ do
            serve (conn, databaseName daemonConfig, mPortMVar, port) initServer serverDefinition
            liftIO $ putStrLn "serve returned"
          pure True
  

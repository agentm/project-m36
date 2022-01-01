{-# LANGUAGE ScopedTypeVariables #-}
module ProjectM36.Server where

import ProjectM36.Client
import ProjectM36.Server.EntryPoints 
import ProjectM36.Server.RemoteCallTypes
import ProjectM36.Server.Config (ServerConfig(..))
import ProjectM36.FSType

import Control.Concurrent.MVar (MVar)
import System.IO (stderr, hPutStrLn)
import System.FilePath (takeDirectory)
import System.Directory (doesDirectoryExist)
import Network.RPC.Curryer.Server
import Network.Socket
import qualified StmContainers.Map as StmMap
import Control.Concurrent.STM

type TestMode = Bool

requestHandlers :: TestMode -> Maybe Timeout -> RequestHandlers ServerState
requestHandlers testFlag ti =
  [
    RequestHandler (\sState (Login dbName) -> do
                       addClientLogin dbName sState
                       conn <- getConn sState
                       handleLogin conn (connectionSocket sState)),
     RequestHandler (\sState Logout -> do
                        conn <- getConn sState                        
                        handleLogout ti conn),
    RequestHandler $ \sState (ExecuteHeadName sessionId) -> do
      --socket -> dbname --maybe create a socket->client state mapping in the server state, too
      conn <- getConn sState
      handleExecuteHeadName ti sessionId conn,
    RequestHandler (\sState (ExecuteRelationalExpr sessionId expr) -> do
                       conn <- getConn sState                        
                       handleExecuteRelationalExpr ti sessionId conn expr),
     RequestHandler (\sState (ExecuteDataFrameExpr sessionId expr) -> do
                        conn <- getConn sState
                        handleExecuteDataFrameExpr ti sessionId conn expr),     
     RequestHandler (\sState (ExecuteDatabaseContextExpr sessionId expr) -> do
                        conn <- getConn sState
                        handleExecuteDatabaseContextExpr ti sessionId conn expr),
     RequestHandler (\sState (ExecuteDatabaseContextIOExpr sessionId expr) -> do
                        conn <- getConn sState
                        handleExecuteDatabaseContextIOExpr ti sessionId conn expr),
     RequestHandler (\sState (ExecuteGraphExpr sessionId expr) -> do
                        conn <- getConn sState
                        handleExecuteGraphExpr ti sessionId conn expr),
     RequestHandler (\sState (ExecuteTransGraphRelationalExpr sessionId expr) -> do
                       conn <- getConn sState
                       handleExecuteTransGraphRelationalExpr ti sessionId conn expr),
     RequestHandler (\sState (ExecuteTypeForRelationalExpr sessionId expr) -> do
                       conn <- getConn sState                        
                       handleExecuteTypeForRelationalExpr ti sessionId conn expr),
     RequestHandler (\sState (RetrieveInclusionDependencies sessionId) -> do
                        conn <- getConn sState
                        handleRetrieveInclusionDependencies ti sessionId conn),
     RequestHandler (\sState (RetrievePlanForDatabaseContextExpr sessionId dbExpr) -> do
                       conn <- getConn sState                        
                       handleRetrievePlanForDatabaseContextExpr ti sessionId conn dbExpr),
     RequestHandler (\sState (RetrieveHeadTransactionId sessionId) -> do
                       conn <- getConn sState                        
                       handleRetrieveHeadTransactionId ti sessionId conn),
     RequestHandler (\sState (RetrieveTransactionGraph sessionId) -> do
                       conn <- getConn sState                        
                       handleRetrieveTransactionGraph ti sessionId conn),
     RequestHandler (\sState (CreateSessionAtHead headn) -> do
                       conn <- getConn sState                        
                       handleCreateSessionAtHead ti conn headn),
     RequestHandler (\sState (CreateSessionAtCommit commitId) -> do
                        conn <- getConn sState
                        handleCreateSessionAtCommit ti conn commitId),
     RequestHandler (\sState (CloseSession sessionId) -> do
                        conn <- getConn sState                 
                        handleCloseSession sessionId conn),
     RequestHandler (\sState (RetrieveAtomTypesAsRelation sessionId) -> do
                        conn <- getConn sState                                         
                        handleRetrieveAtomTypesAsRelation ti sessionId conn),
     RequestHandler (\sState (RetrieveRelationVariableSummary sessionId) -> do
                        conn <- getConn sState                        
                        handleRetrieveRelationVariableSummary ti sessionId conn),
     RequestHandler (\sState (RetrieveAtomFunctionSummary sessionId) -> do
                        conn <- getConn sState
                        handleRetrieveAtomFunctionSummary ti sessionId conn),
     RequestHandler (\sState (RetrieveDatabaseContextFunctionSummary sessionId) -> do
                        conn <- getConn sState
                        handleRetrieveDatabaseContextFunctionSummary ti sessionId conn),     RequestHandler (\sState (RetrieveCurrentSchemaName sessionId) -> do
                       conn <- getConn sState
                       handleRetrieveCurrentSchemaName ti sessionId conn),
     RequestHandler (\sState (ExecuteSchemaExpr sessionId schemaExpr) -> do
                        conn <- getConn sState
                        handleExecuteSchemaExpr ti sessionId conn schemaExpr),
     RequestHandler (\sState (RetrieveSessionIsDirty sessionId) -> do
                        conn <- getConn sState
                        handleRetrieveSessionIsDirty ti sessionId conn),
     RequestHandler (\sState (ExecuteAutoMergeToHead sessionId strat headName') -> do
                        conn <- getConn sState                        
                        handleExecuteAutoMergeToHead ti sessionId conn strat headName'),
     RequestHandler (\sState (RetrieveTypeConstructorMapping sessionId) -> do
                        conn <- getConn sState
                        handleRetrieveTypeConstructorMapping ti sessionId conn),
     RequestHandler (\sState (ExecuteValidateMerkleHashes sessionId) -> do
                        conn <- getConn sState                        
                        handleValidateMerkleHashes ti sessionId conn)
     ] ++ if testFlag then testModeHandlers ti else []

getConn :: ConnectionState ServerState -> IO Connection
getConn connState = do
  let sock = lockless (connectionSocket connState)
      sState = connectionServerState connState
  mConn <- connectionForClient sock sState
  case mConn of
    Nothing -> error "failed to find socket in client map"
    Just conn -> pure conn

testModeHandlers :: Maybe Timeout -> RequestHandlers ServerState
testModeHandlers ti = [RequestHandler (\sState (TestTimeout sessionId) -> do
                                          conn <- getConn sState
                                          handleTestTimeout ti sessionId conn)]

                 
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

-- Sockets do not implement hashable, so we just use their string values as keys
type SocketString = String

data ServerState =
  ServerState {
  --map available databases to local database configurations
  stateDBMap :: StmMap.Map DatabaseName Connection,
  --map clients to database names- after logging in, clients are afixed to specific database names
  stateClientMap :: StmMap.Map SocketString DatabaseName
  }

-- add a client socket to the database mapping
addClientLogin :: DatabaseName -> ConnectionState ServerState -> IO ()
addClientLogin dbName cState = do
  let clientMap = stateClientMap (connectionServerState cState)
      sock = lockless (connectionSocket cState)
  atomically $ do
    mVal <- StmMap.lookup (show sock) clientMap
    case mVal of
      Nothing -> StmMap.insert dbName (show sock) clientMap
      Just _ -> pure () --TODO: throw exception- user already logged in
  
connectionForClient :: Socket -> ServerState -> IO (Maybe Connection)
connectionForClient sock sState =
  atomically $ do
    mdbname <- StmMap.lookup (show sock) (stateClientMap sState)
    case mdbname of
      Nothing -> pure Nothing
      Just dbname -> 
        StmMap.lookup dbname (stateDBMap sState)

initialServerState :: DatabaseName -> Connection -> IO ServerState
initialServerState dbName conn = 
  atomically $ do
  dbmap <- StmMap.new
  clientMap <- StmMap.new
  StmMap.insert conn dbName dbmap
  pure (ServerState { stateDBMap = dbmap, stateClientMap = clientMap })
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
              port = fromIntegral (bindPort daemonConfig)


          --curryer only supports IPv4 for now
          let addrHints = defaultHints { addrSocketType = Stream, addrFamily = AF_INET }
          hostAddrs <- getAddrInfo (Just addrHints) (Just hostname) Nothing
          case hostAddrs of
            [] -> hPutStrLn stderr ("Failed to resolve: " <> hostname) >> pure False
            (AddrInfo _ _ _ _ (SockAddrInet _ addr32) _):_ -> do
              let hostAddr = hostAddressToTuple addr32
                  mTimeout = fromIntegral <$> case perRequestTimeout daemonConfig of
                                              0 -> Nothing
                                              v -> Just v
                  
              sState <- initialServerState (databaseName daemonConfig) conn
              serve (requestHandlers (testMode daemonConfig) mTimeout) sState hostAddr port mAddr
            _ -> error "unsupported socket addressing mode (IPv4 only currently)"


{-# LANGUAGE DeriveAnyClass, DeriveGeneric, ScopedTypeVariables, BangPatterns, PackageImports #-}
{-|
Module: ProjectM36.Client

Client interface to local and remote Project:M36 databases. To get started, connect with 'connectProjectM36', then run some database changes with 'executeDatabaseContextExpr', and issue queries using 'executeRelationalExpr'.
-}
module ProjectM36.Client
       (ConnectionInfo(..),
       Connection(..),
       Port,
       Hostname,
       DatabaseName,
       ConnectionError(..),
       connectProjectM36,
       close,
       closeRemote_,
       executeRelationalExpr,
       executeDatabaseContextExpr,
       executeDatabaseContextIOExpr,
       executeDataFrameExpr,
       executeGraphExpr,
       executeSchemaExpr,
       executeTransGraphRelationalExpr,
       commit,
       rollback,
       typeForRelationalExpr,
       inclusionDependencies,
       ProjectM36.Client.typeConstructorMapping,
       ProjectM36.Client.databaseContextFunctionsAsRelation,      
       planForDatabaseContextExpr,
       currentSchemaName,
       SchemaName,
       HeadName,
       setCurrentSchemaName,
       transactionGraphAsRelation,
       relationVariablesAsRelation,
       ProjectM36.Client.atomFunctionsAsRelation,
       disconnectedTransactionIsDirty,
       headName,
       remoteDBLookupName,
       defaultServerPort,
       headTransactionId,
       defaultDatabaseName,
       defaultRemoteConnectionInfo,
       defaultHeadName,
       PersistenceStrategy(..),
       RelationalExpr,
       RelationalExprBase(..),
       DatabaseContextExprBase(..),
       DatabaseContextExpr,
       DatabaseContextIOExpr(..),
       Attribute(..),
       MergeStrategy(..),
       attributesFromList,
       createNodeId,
       createSessionAtCommit,
       createSessionAtHead,
       closeSession,
       addClientNode,
       callTestTimeout_,
       RelationCardinality(..),
       TransactionGraphOperator(..),
       ProjectM36.Client.autoMergeToHead,
       transactionGraph_,
       disconnectedTransaction_,
       TransGraphRelationalExpr,
       TransactionIdLookup(..),
       TransactionIdHeadBacktrack(..),
       NodeId(..),
       Atom(..),
       Session,
       SessionId,
       NotificationCallback,
       emptyNotificationCallback,
       EvaluatedNotification(..),
       atomTypesAsRelation,
       AttributeExpr,
       inclusionDependencyForKey,
       databaseContextExprForUniqueKey,
       databaseContextExprForForeignKey,
       createScriptedAtomFunction,
       AttributeExprBase(..),
       TypeConstructorBase(..),
       TypeConstructorDef(..),
       DataConstructorDef(..),
       AttributeNamesBase(..),
       RelVarName,
       IncDepName,
       InclusionDependency(..),
       AttributeName,
       DF.DataFrame,
       DF.DataFrameExpr,
       DF.AttributeOrderExpr,
       DF.Order(..),
       RelationalError(..),
       RequestTimeoutException(..),
       RemoteProcessDiedException(..),
       AtomType(..),
       Atomable(..),
       TupleExprBase(..),
       AtomExprBase(..),
       RestrictionPredicateExprBase(..),
       withTransaction
       ) where
import ProjectM36.Base hiding (inclusionDependencies) --defined in this module as well
import qualified ProjectM36.Base as B
import ProjectM36.Error
import ProjectM36.Atomable
import ProjectM36.AtomFunction as AF
import ProjectM36.StaticOptimizer
import ProjectM36.Key
import qualified ProjectM36.DataFrame as DF
import ProjectM36.DatabaseContextFunction as DCF
import qualified ProjectM36.IsomorphicSchema as Schema
import Control.Monad.State
import Control.Monad.Trans.Reader
import qualified ProjectM36.RelationalExpression as RE
import ProjectM36.DatabaseContext (basicDatabaseContext)
import qualified ProjectM36.TransactionGraph as Graph
import ProjectM36.TransactionGraph
import qualified ProjectM36.Transaction as Trans
import ProjectM36.TransactionGraph.Persist
import ProjectM36.Attribute
import ProjectM36.TransGraphRelationalExpression (TransGraphRelationalExpr, evalTransGraphRelationalExpr)
import ProjectM36.Persist (DiskSync(..))
import ProjectM36.FileLock
import ProjectM36.Notifications
import ProjectM36.Server.RemoteCallTypes
import qualified ProjectM36.DisconnectedTransaction as Discon
import ProjectM36.Relation (typesAsRelation)
import ProjectM36.ScriptSession (initScriptSession, ScriptSession)
import qualified ProjectM36.Relation as R
import qualified ProjectM36.DatabaseContext as DBC
import Control.Exception.Base
import GHC.Conc.Sync

import Network.Transport (Transport(closeTransport))

#if MIN_VERSION_network_transport_tcp(0,6,0)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Network.Transport.TCP.Internal (encodeEndPointAddress)
#else
import Network.Transport.TCP (encodeEndPointAddress, createTransport, defaultTCPParameters)
#endif

import Control.Distributed.Process.Node (newLocalNode, initRemoteTable, runProcess, LocalNode, forkProcess, closeLocalNode)
import Control.Distributed.Process.Extras.Internal.Types (whereisRemote)
import Control.Distributed.Process.ManagedProcess.Client (call, safeCall)
import Data.Either (isRight)
import Data.UUID.V4 (nextRandom)
import Data.Word
import Control.Distributed.Process (ProcessId, Process, receiveWait, send, match, NodeId(..), reconnect)
import Control.Exception (IOException, handle, AsyncException, throwIO, fromException, Exception)
import Control.Concurrent.MVar
import qualified Data.Map as M
#if MIN_VERSION_stm_containers(1,0,0)
import qualified StmContainers.Map as StmMap
import qualified StmContainers.Set as StmSet
#else
import qualified STMContainers.Map as StmMap
import qualified STMContainers.Set as StmSet
#endif
import qualified ProjectM36.Session as Sess
import ProjectM36.Session
import ProjectM36.Sessions
--import "list-t" ListT
import Data.Binary (Binary)
import GHC.Generics (Generic)
import Control.DeepSeq (force)
import System.IO
import Data.Time.Clock
import Data.Typeable

type Hostname = String

type Port = Word16

type DatabaseName = String

-- | The type for notifications callbacks in the client. When a registered notification fires due to a changed relational expression evaluation, the server propagates the notifications to the clients in the form of the callback.
type NotificationCallback = NotificationName -> EvaluatedNotification -> IO ()

-- | The empty notification callback ignores all callbacks.
emptyNotificationCallback :: NotificationCallback
emptyNotificationCallback _ _ = pure ()

type GhcPkgPath = String

data RemoteProcessDiedException = RemoteProcessDiedException
                                  deriving (Show, Eq)
                                           
instance Exception RemoteProcessDiedException                                          
  
data RequestTimeoutException = RequestTimeoutException
                             deriving (Show, Eq)

instance Exception RequestTimeoutException

-- | Construct a 'ConnectionInfo' to describe how to make the 'Connection'. The database can be run within the current process or running remotely via distributed-process.
data ConnectionInfo = InProcessConnectionInfo PersistenceStrategy NotificationCallback [GhcPkgPath]|
                      RemoteProcessConnectionInfo DatabaseName NodeId NotificationCallback
                      
type EvaluatedNotifications = M.Map NotificationName EvaluatedNotification

-- | Used for callbacks from the server when monitored changes have been made.
newtype NotificationMessage = NotificationMessage EvaluatedNotifications
                           deriving (Binary, Eq, Show, Generic)

-- | When a notification is fired, the 'reportOldExpr' is evaluated in the commit's pre-change context while the 'reportNewExpr' is evaluated in the post-change context and they are returned along with the original notification.
data EvaluatedNotification = EvaluatedNotification {
  notification :: Notification,
  reportOldRelation :: Either RelationalError Relation,
  reportNewRelation :: Either RelationalError Relation
  }
                           deriving(Binary, Eq, Show, Generic)
                      

-- | Create a 'NodeId' for use in connecting to a remote server using distributed-process.
createNodeId :: Hostname -> Port -> NodeId                      
createNodeId host port = NodeId $ encodeEndPointAddress host (show port) 1
                      
-- | Use this for connecting to remote servers on the default port.
defaultServerPort :: Port
defaultServerPort = 6543

-- | Use this for connecting to remote servers with the default database name.
defaultDatabaseName :: DatabaseName
defaultDatabaseName = "base"

-- | Use this for connecting to remote servers with the default head name.
defaultHeadName :: HeadName
defaultHeadName = "master"

-- | Create a connection configuration which connects to the localhost on the default server port and default server database name. The configured notification callback is set to ignore all events.
defaultRemoteConnectionInfo :: ConnectionInfo
defaultRemoteConnectionInfo = RemoteProcessConnectionInfo defaultDatabaseName (createNodeId "127.0.0.1" defaultServerPort) emptyNotificationCallback 

-- | The 'Connection' represents either local or remote access to a database. All operations flow through the connection.
type ClientNodes = StmSet.Set ProcessId

-- internal structure specific to in-process connections
data InProcessConnectionConf = InProcessConnectionConf {
  ipPersistenceStrategy :: PersistenceStrategy, 
  ipClientNodes :: ClientNodes, 
  ipSessions :: Sessions, 
  ipTransactionGraph :: TVar TransactionGraph,
  ipScriptSession :: Maybe ScriptSession,
  ipLocalNode :: LocalNode,
  ipTransport :: Transport, -- we hold onto this so that we can close it gracefully
  ipLocks :: Maybe (LockFile, MVar LockFileHash) -- nothing when NoPersistence
  }

data RemoteProcessConnectionConf = RemoteProcessConnectionConf {
  rLocalNode :: LocalNode, 
  rProcessId :: ProcessId, --remote processId
  rTransport :: Transport --the TCP socket transport
  }
  
data Connection = InProcessConnection InProcessConnectionConf |
                  RemoteProcessConnection RemoteProcessConnectionConf
                  
-- | There are several reasons why a connection can fail.
data ConnectionError = SetupDatabaseDirectoryError PersistenceError |
                       IOExceptionError IOException |
                       NoSuchDatabaseByNameError DatabaseName |
                       LoginError 
                       deriving (Show, Eq, Generic)
                  
remoteDBLookupName :: DatabaseName -> String    
remoteDBLookupName = (++) "db-" 

createLocalNode :: IO (LocalNode, Transport)
createLocalNode = do
#if MIN_VERSION_network_transport_tcp(0,6,0)  
  eLocalTransport <- createTransport "127.0.0.1" "0" (\sn -> ("127.0.0.1", sn)) defaultTCPParameters
#else
  eLocalTransport <- createTransport "127.0.0.1" "0" defaultTCPParameters
#endif
  case eLocalTransport of
    Left err -> error ("failed to create transport: " ++ show err)
    Right localTransport -> do
      localNode <- newLocalNode localTransport initRemoteTable 
      pure (localNode, localTransport)
    
notificationListener :: NotificationCallback -> Process ()    
notificationListener callback = do
  --pid <- getSelfPid
  --liftIO $ putStrLn $ "LISTENER THREAD START " ++ show pid
  _ <- forever $
    receiveWait [
      match (\(NotificationMessage eNots) ->
            --say $ "NOTIFICATION: " ++ show eNots
            -- when notifications are thrown, they are not adjusted for the current schema which could be problematic, but we don't have access to the current session here
            liftIO $ mapM_ (uncurry callback) (M.toList eNots)
            )
      ]
  --say "LISTENER THREAD EXIT"
  pure ()
  
startNotificationListener :: LocalNode -> NotificationCallback -> IO ProcessId
startNotificationListener localNode callback = forkProcess localNode (notificationListener callback)
  
createScriptSession :: [String] -> IO (Maybe ScriptSession)  
createScriptSession ghcPkgPaths = do
  eScriptSession <- initScriptSession ghcPkgPaths
  case eScriptSession of
    Left err -> hPutStrLn stderr ("Failed to load scripting engine- scripting disabled: " ++ show err) >> pure Nothing --not a fatal error, but the scripting feature must be disabled
    Right s -> pure (Just s)

-- | To create a 'Connection' to a remote or local database, create a 'ConnectionInfo' and call 'connectProjectM36'.
connectProjectM36 :: ConnectionInfo -> IO (Either ConnectionError Connection)
--create a new in-memory database/transaction graph
connectProjectM36 (InProcessConnectionInfo strat notificationCallback ghcPkgPaths) = do
  freshId <- nextRandom
  tstamp <- getCurrentTime
  let bootstrapContext = basicDatabaseContext 
      freshGraph = bootstrapTransactionGraph tstamp freshId bootstrapContext
  case strat of
    --create date examples graph for now- probably should be empty context in the future
    NoPersistence -> do
        graphTvar <- newTVarIO freshGraph
        clientNodes <- StmSet.newIO
        sessions <- StmMap.newIO
        (localNode, transport) <- createLocalNode
        notificationPid <- startNotificationListener localNode notificationCallback
        mScriptSession <- createScriptSession ghcPkgPaths
        
        let conn = InProcessConnection InProcessConnectionConf {
                                           ipPersistenceStrategy = strat, 
                                           ipClientNodes = clientNodes, 
                                           ipSessions = sessions, 
                                           ipTransactionGraph = graphTvar, 
                                           ipScriptSession = mScriptSession,
                                           ipLocalNode = localNode,
                                           ipTransport = transport, 
                                           ipLocks = Nothing}
        addClientNode conn notificationPid
        pure (Right conn)
    MinimalPersistence dbdir -> connectPersistentProjectM36 strat NoDiskSync dbdir freshGraph notificationCallback ghcPkgPaths
    CrashSafePersistence dbdir -> connectPersistentProjectM36 strat FsyncDiskSync dbdir freshGraph notificationCallback ghcPkgPaths
        
connectProjectM36 (RemoteProcessConnectionInfo databaseName serverNodeId notificationCallback) = do
  connStatus <- newEmptyMVar
  (localNode, transport) <- createLocalNode
  let dbName = remoteDBLookupName databaseName
  --putStrLn $ "Connecting to " ++ show serverNodeId ++ " " ++ dbName
  notificationListenerPid <- startNotificationListener localNode notificationCallback
  runProcess localNode $ do
    --even a failed connection retains an open socket!
    mServerProcessId <- whereisRemote serverNodeId dbName
    case mServerProcessId of
      Nothing -> liftIO $ putMVar connStatus $ Left (NoSuchDatabaseByNameError databaseName)
      Just serverProcessId -> do
        loginConfirmation <- safeLogin (Login notificationListenerPid) serverProcessId
        if not loginConfirmation then
          liftIO $ putMVar connStatus (Left LoginError)
          else
          liftIO $ putMVar connStatus (Right $ RemoteProcessConnection RemoteProcessConnectionConf {rLocalNode = localNode, rProcessId = serverProcessId, rTransport = transport})
  takeMVar connStatus

connectPersistentProjectM36 :: PersistenceStrategy ->
                               DiskSync ->
                               FilePath -> 
                               TransactionGraph ->
                               NotificationCallback ->
                               [GhcPkgPath] -> 
                               IO (Either ConnectionError Connection)      
connectPersistentProjectM36 strat sync dbdir freshGraph notificationCallback ghcPkgPaths = do
  err <- setupDatabaseDir sync dbdir freshGraph 
  case err of
    Left err' -> return $ Left (SetupDatabaseDirectoryError err')
    Right (lockFileH, digest) -> do
      mScriptSession <- createScriptSession ghcPkgPaths
      graph <- transactionGraphLoad dbdir emptyTransactionGraph mScriptSession
      case graph of
        Left err' -> return $ Left (SetupDatabaseDirectoryError err')
        Right graph' -> do
          tvarGraph <- newTVarIO graph'
          sessions <- StmMap.newIO
          clientNodes <- StmSet.newIO
          (localNode, transport) <- createLocalNode
          lockMVar <- newMVar digest
          let conn = InProcessConnection InProcessConnectionConf {
                                             ipPersistenceStrategy = strat,
                                             ipClientNodes = clientNodes,
                                             ipSessions = sessions,
                                             ipTransactionGraph = tvarGraph,
                                             ipScriptSession = mScriptSession,
                                             ipLocalNode = localNode,
                                             ipTransport = transport,
                                             ipLocks = Just (lockFileH, lockMVar)
                                             }

          notificationPid <- startNotificationListener localNode notificationCallback 
          addClientNode conn notificationPid
          pure (Right conn)
          
-- | Create a new session at the transaction id and return the session's Id.
createSessionAtCommit :: Connection -> TransactionId -> IO (Either RelationalError SessionId)
createSessionAtCommit conn@(InProcessConnection _) commitId = do
   newSessionId <- nextRandom
   atomically $ createSessionAtCommit_ commitId newSessionId conn
createSessionAtCommit conn@(RemoteProcessConnection _) uuid = remoteCall conn (CreateSessionAtCommit uuid)

createSessionAtCommit_ :: TransactionId -> SessionId -> Connection -> STM (Either RelationalError SessionId)
createSessionAtCommit_ commitId newSessionId (InProcessConnection conf) = do
    let sessions = ipSessions conf
        graphTvar = ipTransactionGraph conf
    graph <- readTVar graphTvar
    case transactionForId commitId graph of
        Left err -> pure (Left err)
        Right transaction -> do
            let freshDiscon = DisconnectedTransaction commitId (Trans.schemas transaction) NoOperation
            keyDuplication <- StmMap.lookup newSessionId sessions
            case keyDuplication of
                Just _ -> pure $ Left (SessionIdInUseError newSessionId)
                Nothing -> do
                   StmMap.insert (Session freshDiscon defaultSchemaName) newSessionId sessions
                   pure $ Right newSessionId
createSessionAtCommit_ _ _ (RemoteProcessConnection _) = error "createSessionAtCommit_ called on remote connection"
  
-- | Call 'createSessionAtHead' with a transaction graph's head's name to create a new session pinned to that head. This function returns a 'SessionId' which can be used in other function calls to reference the point in the transaction graph.
createSessionAtHead :: Connection -> HeadName -> IO (Either RelationalError SessionId)
createSessionAtHead conn@(InProcessConnection conf) headn = do
    let graphTvar = ipTransactionGraph conf
    newSessionId <- nextRandom
    atomically $ do
        graph <- readTVar graphTvar
        case transactionForHead headn graph of
            Nothing -> pure $ Left (NoSuchHeadNameError headn)
            Just trans -> createSessionAtCommit_ (transactionId trans) newSessionId conn
createSessionAtHead conn@(RemoteProcessConnection _) headn = remoteCall conn (CreateSessionAtHead headn)

-- | Used internally for server connections to keep track of remote nodes for the purpose of sending notifications later.
addClientNode :: Connection -> ProcessId -> IO ()
addClientNode (RemoteProcessConnection _) _ = error "addClientNode called on remote connection"
addClientNode (InProcessConnection conf) newProcessId = atomically (StmSet.insert newProcessId (ipClientNodes conf))

-- | Discards a session, eliminating any uncommitted changes present in the session.
closeSession :: SessionId -> Connection -> IO ()
closeSession sessionId (InProcessConnection conf) = 
    atomically $ StmMap.delete sessionId (ipSessions conf)
closeSession sessionId conn@(RemoteProcessConnection _) = remoteCall conn (CloseSession sessionId)       

-- | 'close' cleans up the database access connection and closes any relevant sockets.
close :: Connection -> IO ()
close (InProcessConnection conf) = do
  atomically $ do
    let sessions = ipSessions conf
#if MIN_VERSION_stm_containers(1,0,0)        
    StmMap.reset sessions
#else
    StmMap.deleteAll sessions
#endif
    pure ()
  closeLocalNode (ipLocalNode conf)
  closeTransport (ipTransport conf)
  let mLocks = ipLocks conf
  case mLocks of
    Nothing -> pure ()
    Just (lockFileH, _) -> closeLockFile lockFileH

close conn@(RemoteProcessConnection conf) = do
  _ <- remoteCall conn Logout :: IO Bool
  closeLocalNode (rLocalNode conf)
  closeTransport (rTransport conf)

--used only by the server EntryPoints
closeRemote_ :: Connection -> IO ()
closeRemote_ (InProcessConnection _) = error "invalid call of closeRemote_ on InProcessConnection"
closeRemote_ (RemoteProcessConnection conf) = runProcess (rLocalNode conf) (reconnect (rProcessId conf))

  --we need to actually close the localNode's connection to the remote
--within the database server, we must catch and handle all exception lest they take down the database process- this handling might be different for other use-cases
--exceptions should generally *NOT* be thrown from any Project:M36 code paths, but third-party code such as AtomFunction scripts could conceivably throw undefined, etc.

excEither :: IO (Either RelationalError a) -> IO (Either RelationalError a)
excEither = handle handler
  where
    handler exc | Just (_ :: AsyncException) <- fromException exc = throwIO exc
                | otherwise = pure (Left (UnhandledExceptionError (show exc)))
      
runProcessResult :: LocalNode -> Process a -> IO a      
runProcessResult localNode proc = do
  ret <- newEmptyMVar
  runProcess localNode $ do
    val <- proc
    liftIO $ putMVar ret val
  takeMVar ret

safeLogin :: Login -> ProcessId -> Process Bool
safeLogin login procId = do 
  ret <- call procId login
  case ret of
    Left (_ :: ServerError) -> pure False
    Right val -> pure val

remoteCall :: (Binary a, Binary b, Typeable a, Typeable b) => Connection -> a -> IO b
remoteCall (InProcessConnection _ ) _ = error "remoteCall called on local connection"
remoteCall (RemoteProcessConnection conf) arg = runProcessResult localNode $ do
  ret <- safeCall serverProcessId arg
  case ret of
    Left err -> error ("server died: " ++ show err)
    Right ret' -> case ret' of
                       Left RequestTimeoutError -> liftIO (throwIO RequestTimeoutException)
                       Left (ProcessDiedError _) -> liftIO (throwIO RemoteProcessDiedException)
                       Right val -> pure val
  where
    localNode = rLocalNode conf
    serverProcessId = rProcessId conf

sessionForSessionId :: SessionId -> Sessions -> STM (Either RelationalError Session)
sessionForSessionId sessionId sessions = 
  maybe (Left $ NoSuchSessionError sessionId) Right <$> StmMap.lookup sessionId sessions
  
schemaForSessionId :: Session -> STM (Either RelationalError Schema)  
schemaForSessionId session = do
  let sname = schemaName session
  if sname == defaultSchemaName then
    pure (Right (Schema [])) -- the main schema includes no transformations (but neither do empty schemas :/ )
    else
    case M.lookup sname (subschemas session) of
      Nothing -> pure (Left (SubschemaNameNotInUseError sname))
      Just schema -> pure (Right schema)
  
sessionAndSchema :: SessionId -> Sessions -> STM (Either RelationalError (Session, Schema))
sessionAndSchema sessionId sessions = do
  eSession <- sessionForSessionId sessionId sessions
  case eSession of
    Left err -> pure (Left err)
    Right session -> do  
      eSchema <- schemaForSessionId session
      case eSchema of
        Left err -> pure (Left err)
        Right schema -> pure (Right (session, schema))
  
-- | Returns the name of the currently selected isomorphic schema.
currentSchemaName :: SessionId -> Connection -> IO (Either RelationalError SchemaName)
currentSchemaName sessionId (InProcessConnection conf) = atomically $ do
  let sessions = ipSessions conf
  eSession <- sessionForSessionId sessionId sessions
  case eSession of
    Left err -> pure (Left err)
    Right session -> pure (Right (Sess.schemaName session))
currentSchemaName sessionId conn@(RemoteProcessConnection _) = remoteCall conn (RetrieveCurrentSchemaName sessionId)

-- | Switch to the named isomorphic schema.
setCurrentSchemaName :: SessionId -> Connection -> SchemaName -> IO (Either RelationalError ())
setCurrentSchemaName sessionId (InProcessConnection conf) sname = atomically $ do
  let sessions = ipSessions conf
  eSession <- sessionForSessionId sessionId sessions
  case eSession of
    Left err -> pure (Left err)
    Right session -> case Sess.setSchemaName sname session of
      Left err -> pure (Left err)
      Right newSession -> StmMap.insert newSession sessionId sessions >> pure (Right ())
setCurrentSchemaName sessionId conn@(RemoteProcessConnection _) sname = remoteCall conn (ExecuteSetCurrentSchema sessionId sname)

-- | Execute a relational expression in the context of the session and connection. Relational expressions are queries and therefore cannot alter the database.
executeRelationalExpr :: SessionId -> Connection -> RelationalExpr -> IO (Either RelationalError Relation)
executeRelationalExpr sessionId (InProcessConnection conf) expr = excEither $ atomically $ do
  let sessions = ipSessions conf
  eSession <- sessionAndSchema sessionId sessions
  case eSession of
    Left err -> pure $ Left err
    Right (session, schema) -> do
      let expr' = if schemaName session /= defaultSchemaName then
                    Schema.processRelationalExprInSchema schema expr
                  else
                    Right expr
      case expr' of
        Left err -> pure (Left err)
        Right expr'' -> case optimizeRelationalExpr (Sess.concreteDatabaseContext session) expr'' of
          Left err -> pure (Left err)
          Right optExpr -> do
            let evald = runReader (RE.evalRelationalExpr optExpr) (RE.mkRelationalExprState (Sess.concreteDatabaseContext session))
            case evald of
              Left err -> pure (Left err)
              Right rel -> pure (force (Right rel)) -- this is necessary so that any undefined/error exceptions are spit out here 
executeRelationalExpr sessionId conn@(RemoteProcessConnection _) relExpr = remoteCall conn (ExecuteRelationalExpr sessionId relExpr)

-- | Execute a database context expression in the context of the session and connection. Database expressions modify the current session's disconnected transaction but cannot modify the transaction graph.
executeDatabaseContextExpr :: SessionId -> Connection -> DatabaseContextExpr -> IO (Either RelationalError ())
executeDatabaseContextExpr sessionId (InProcessConnection conf) expr = excEither $ atomically $ do
  let sessions = ipSessions conf
  eSession <- sessionAndSchema sessionId sessions
  case eSession of
    Left err -> pure (Left err)
    Right (session, schema) -> do
      let expr' = if schemaName session == defaultSchemaName then
                    Right expr
                  else
                    Schema.processDatabaseContextExprInSchema schema expr
      case expr' of 
        Left err -> pure (Left err)
        Right expr'' -> case runState (do
                                          eOptExpr <- applyStaticDatabaseOptimization expr''
                                          case eOptExpr of
                                            Left err -> pure (Left err)
                                            Right optExpr ->
                                              RE.evalDatabaseContextExpr optExpr) (RE.freshDatabaseState (Sess.concreteDatabaseContext session)) of
          (Left err,_) -> pure (Left err)
          (Right (), (_,_,False)) -> pure (Right ()) --optimization- if nothing was dirtied, nothing to do
          (Right (), (!context',_,True)) -> do
            let newDiscon = DisconnectedTransaction (Sess.parentId session) newSchemas NoOperation
                newSubschemas = Schema.processDatabaseContextExprSchemasUpdate (Sess.subschemas session) expr
                newSchemas = Schemas context' newSubschemas
                newSession = Session newDiscon (Sess.schemaName session)
            StmMap.insert newSession sessionId sessions
            pure (Right ())
executeDatabaseContextExpr sessionId conn@(RemoteProcessConnection _) dbExpr = remoteCall conn (ExecuteDatabaseContextExpr sessionId dbExpr)

-- | Similar to a git rebase, 'autoMergeToHead' atomically creates a temporary branch and merges it to the latest commit of the branch referred to by the 'HeadName' and commits the merge. This is useful to reduce incidents of 'TransactionIsNotAHeadError's but at the risk of merge errors (thus making it similar to rebasing). Alternatively, as an optimization, if a simple commit is possible (meaning that the head has not changed), then a fast-forward commit takes place instead.
autoMergeToHead :: SessionId -> Connection -> MergeStrategy -> HeadName -> IO (Either RelationalError ())
autoMergeToHead sessionId (InProcessConnection conf) strat headName' = do
  let sessions = ipSessions conf
  id1 <- nextRandom
  id2 <- nextRandom
  id3 <- nextRandom
  tstamp <- getCurrentTime
  commitLock_ sessionId conf $ \graph -> do
    eSession <- sessionForSessionId sessionId sessions  
    case eSession of
      Left err -> pure (Left err)
      Right session -> 
        case Graph.transactionForHead headName' graph of
          Nothing -> pure (Left (NoSuchHeadNameError headName'))
          Just headTrans -> do
            --attempt fast-forward commit, if possible
            let graphInfo = if Sess.parentId session == transactionId headTrans then do
                              ret <- Graph.evalGraphOp tstamp id1 (Sess.disconnectedTransaction session) graph Commit
                              pure (ret, [id1])
                            else do
                              ret <- Graph.autoMergeToHead tstamp (id1, id2, id3) (Sess.disconnectedTransaction session) headName' strat graph 
                              pure (ret, [id1,id2,id3])
            case graphInfo of
              Left err -> pure (Left err)
              Right ((discon', graph'), transactionIdsAdded) ->
                pure (Right (discon', graph', transactionIdsAdded))
autoMergeToHead sessionId conn@(RemoteProcessConnection _) strat headName' = remoteCall conn (ExecuteAutoMergeToHead sessionId strat headName')
      
-- | Execute a database context IO-monad-based expression for the given session and connection. `DatabaseContextIOExpr`s modify the DatabaseContext but cannot be purely implemented.
--this is almost completely identical to executeDatabaseContextExpr above
executeDatabaseContextIOExpr :: SessionId -> Connection -> DatabaseContextIOExpr -> IO (Either RelationalError ())
executeDatabaseContextIOExpr sessionId (InProcessConnection conf) expr = excEither $ do
  let sessions = ipSessions conf
      scriptSession = ipScriptSession conf
  eSession <- atomically $ sessionForSessionId sessionId sessions --potentially race condition due to interleaved IO?
  case eSession of
    Left err -> pure (Left err)
    Right session -> do
      res <- RE.evalDatabaseContextIOExpr scriptSession (Sess.concreteDatabaseContext session) expr
      case res of
        Left err -> pure (Left err)
        Right context' -> do
          let newDiscon = DisconnectedTransaction (Sess.parentId session) newSchemas NoOperation
              newSchemas = Schemas context' (Sess.subschemas session)
              newSession = Session newDiscon (Sess.schemaName session)
          atomically $ StmMap.insert newSession sessionId sessions
          pure (Right ())
executeDatabaseContextIOExpr sessionId conn@(RemoteProcessConnection _) dbExpr = remoteCall conn (ExecuteDatabaseContextIOExpr sessionId dbExpr)
         
{-
executeGraphExprSTM_ :: TransactionId -> SessionId -> Session -> Sessions -> TransactionGraphOperator -> TransactionGraph -> TVar TransactionGraph -> STM (Either RelationalError (TransactionGraph, DisconnectedTransaction)
executeGraphExprSTM_ freshId sessionId session sessions graphExpr graph graphTVar= do
  case evalGraphOp freshId (Sess.disconnectedTransaction session) graph graphExpr of
    Left err -> do
      when updateGraphOnError (writeTVar graphTVar graph)
      pure $ Left err
    Right (discon', graph') -> do
      writeTVar graphTVar graph'
      let newSession = Session discon' (Sess.schemaName session)
      STMMap.insert newSession sessionId sessions
      pure $ Right graph'
-}
  
-- process notifications for commits
executeCommitExprSTM_ :: DatabaseContext -> DatabaseContext -> ClientNodes -> STM (EvaluatedNotifications, ClientNodes)
executeCommitExprSTM_ oldContext newContext nodes = do
  let nots = notifications oldContext
      fireNots = notificationChanges nots oldContext newContext 
      evaldNots = M.map mkEvaldNot fireNots
      evalInContext expr ctx = runReader (RE.evalRelationalExpr expr) (RE.mkRelationalExprState ctx)
      mkEvaldNot notif = EvaluatedNotification { notification = notif, 
                                                 reportOldRelation = evalInContext (reportOldExpr notif) oldContext,
                                                 reportNewRelation = evalInContext (reportNewExpr notif) newContext}
  pure (evaldNots, nodes)
  
-- | Execute a transaction graph expression in the context of the session and connection. Transaction graph operators modify the transaction graph state.

-- OPTIMIZATION OPPORTUNITY: no locks are required to write new transaction data, only to update the transaction graph id file
-- if writing data is re-entrant, we may be able to use unsafeIOtoSTM
-- perhaps keep hash of data file instead of checking if our head was updated on every write
executeGraphExpr :: SessionId -> Connection -> TransactionGraphOperator -> IO (Either RelationalError ())
executeGraphExpr sessionId (InProcessConnection conf) graphExpr = excEither $ do
  let sessions = ipSessions conf
  freshId <- nextRandom
  tstamp <- getCurrentTime
  commitLock_ sessionId conf $ \updatedGraph -> do
    eSession <- sessionForSessionId sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right session -> do
        let discon = Sess.disconnectedTransaction session
        case evalGraphOp tstamp freshId discon updatedGraph graphExpr of
          Left err -> pure (Left err)
          Right (discon', graph') -> do
            --if freshId appears in the graph, then we need to pass it on
            let transIds = [freshId | isRight (transactionForId freshId graph')]
            pure (Right (discon', graph', transIds))
{-
executeGraphExpr sessionId (InProcessConnection conf) graphExpr = excEither $ do
  let strat = ipPersistenceStrategy conf
      clientNodes = ipClientNodes conf
      sessions = ipSessions conf
      graphTvar = ipTransactionGraph conf
      mLockFileH = ipLocks conf
      lockHandler body = case graphExpr of
        Commit -> case mLockFileH of
          Nothing -> body False
          Just (lockFileH, lockMVar) -> let acquireLocks = do
                                              lastWrittenDigest <- takeMVar lockMVar 
                                              lockFile lockFileH WriteLock
                                              latestDigest <- readGraphTransactionIdDigest strat
                                              pure (latestDigest /= lastWrittenDigest)
                                              
                                            releaseLocks _ = do
                                              --still holding the lock- get the latest digest
                                              gDigest <- readGraphTransactionIdDigest strat
                                              unlockFile lockFileH 
                                              putMVar lockMVar gDigest
                                        in bracket acquireLocks releaseLocks body
        _ -> body False
  freshId <- nextRandom
  lockHandler $ \dbWrittenByOtherProcess -> do
    --if the database file has been updated since we wrote it last, load it before trying to sync our version done- this can result in TransactionNotAHeadErrors
    --read transaction data and compare to existing graph
      --in the future, we can detect if updated transaction graph can be safely merged (such as with a transaction on a separate head) (rebase-able commits should force the user to rebase from the client to confirm that the action makes sense)
      manip <- atomically $ do
        eSession <- sessionForSessionId sessionId sessions
        --handle graph update by other process
        oldGraph <- readTVar graphTvar
        case eSession of
         Left err -> pure (Left err)
         Right session -> do
            let mScriptSession = ipScriptSession conf              
                dbdir = case strat of
                  MinimalPersistence x -> x
                  CrashSafePersistence x -> x
                  _ -> error "accessing dbdir on non-persisted connection"
            eRefreshedGraph <- if dbWrittenByOtherProcess then
                               unsafeIOToSTM (transactionGraphLoad dbdir oldGraph mScriptSession)
                             else
                               pure (Right oldGraph)
            case eRefreshedGraph of
              Left err -> pure (Left (DatabaseLoadError err))
              Right refreshedGraph -> do
                   --snip it
                   eGraph <- executeGraphExprSTM_ dbWrittenByOtherProcess freshId sessionId session sessions graphExpr refreshedGraph graphTvar
                   --snip it
                   case eGraph of
                     Left err -> pure (Left err)
                     Right newGraph -> do
                       --handle commit
                       if isCommit graphExpr then do
                             case transactionForId (Sess.parentId session) oldGraph of
                               Left err -> pure $ Left err
                               Right previousTrans -> do
                                 (evaldNots, nodes) <- executeCommitExprSTM_ (Trans.concreteDatabaseContext previousTrans) (Sess.concreteDatabaseContext session) clientNodes
                                 nodesToNotify <- toList (StmSet.stream nodes)
                                 pure $ Right (evaldNots, nodesToNotify, newGraph)
                             else
                              pure $ Right (M.empty, [], newGraph)
      case manip of 
       Left err -> return $ Just err
       Right (notsToFire, nodesToNotify, newGraph) -> do
        --update filesystem database, if necessary
        processTransactionGraphPersistence strat newGraph
        sendNotifications nodesToNotify (ipLocalNode conf) notsToFire
        pure Nothing
-}
executeGraphExpr sessionId conn@(RemoteProcessConnection _) graphExpr = remoteCall conn (ExecuteGraphExpr sessionId graphExpr)

-- | A trans-graph expression is a relational query executed against the entirety of a transaction graph.
executeTransGraphRelationalExpr :: SessionId -> Connection -> TransGraphRelationalExpr -> IO (Either RelationalError Relation)
executeTransGraphRelationalExpr _ (InProcessConnection conf) tgraphExpr = excEither . atomically $ do
  let graphTvar = ipTransactionGraph conf
  graph <- readTVar graphTvar
  case evalTransGraphRelationalExpr tgraphExpr graph of
    Left err -> pure (Left err)
    Right relExpr -> case runReader (RE.evalRelationalExpr relExpr) (RE.mkRelationalExprState DBC.empty) of
      Left err -> pure (Left err)
      Right rel -> pure (force (Right rel))
executeTransGraphRelationalExpr sessionId conn@(RemoteProcessConnection _) tgraphExpr = remoteCall conn (ExecuteTransGraphRelationalExpr sessionId tgraphExpr)  

-- | Schema expressions manipulate the isomorphic schemas for the current 'DatabaseContext'.
executeSchemaExpr :: SessionId -> Connection -> Schema.SchemaExpr -> IO (Either RelationalError ())
executeSchemaExpr sessionId (InProcessConnection conf) schemaExpr = atomically $ do
  let sessions = ipSessions conf
  eSession <- sessionAndSchema sessionId sessions  
  case eSession of
    Left err -> pure (Left err)
    Right (session, _) -> do
      let subschemas' = subschemas session
      case Schema.evalSchemaExpr schemaExpr (Sess.concreteDatabaseContext session) subschemas' of
        Left err -> pure (Left err)
        Right (newSubschemas, newContext) -> do
          --hm- maybe we should start using lenses
          let discon = Sess.disconnectedTransaction session 
              newSchemas = Schemas newContext newSubschemas
              newSession = Session (DisconnectedTransaction (Discon.parentId discon) newSchemas NoOperation) (Sess.schemaName session)
          StmMap.insert newSession sessionId sessions
          pure (Right ())
executeSchemaExpr sessionId conn@(RemoteProcessConnection _) schemaExpr = remoteCall conn (ExecuteSchemaExpr sessionId schemaExpr)          

-- | After modifying a 'DatabaseContext', 'commit' the transaction to the transaction graph at the head which the session is referencing. This will also trigger checks for any notifications which need to be propagated.
commit :: SessionId -> Connection -> IO (Either RelationalError ())
commit sessionId conn@(InProcessConnection _) = executeGraphExpr sessionId conn Commit 
commit sessionId conn@(RemoteProcessConnection _) = remoteCall conn (ExecuteGraphExpr sessionId Commit)
  
sendNotifications :: [ProcessId] -> LocalNode -> EvaluatedNotifications -> IO ()
sendNotifications pids localNode nots = mapM_ sendNots pids
  where
    sendNots remoteClientPid =
      unless (M.null nots) $ runProcess localNode $ send remoteClientPid (NotificationMessage nots)
          
-- | Discard any changes made in the current 'Session' and 'DatabaseContext'. This resets the disconnected transaction to reference the original database context of the parent transaction and is a very cheap operation.
rollback :: SessionId -> Connection -> IO (Either RelationalError ())
rollback sessionId conn@(InProcessConnection _) = executeGraphExpr sessionId conn Rollback      
rollback sessionId conn@(RemoteProcessConnection _) = remoteCall conn (ExecuteGraphExpr sessionId Rollback)

-- | Write the transaction graph to disk. This function can be used to incrementally write new transactions to disk.
processTransactionGraphPersistence :: PersistenceStrategy -> [TransactionId] -> TransactionGraph -> IO ()
processTransactionGraphPersistence NoPersistence _ _ = pure ()
processTransactionGraphPersistence (MinimalPersistence dbdir) transIds graph = transactionGraphPersist NoDiskSync dbdir transIds graph >> pure ()
processTransactionGraphPersistence (CrashSafePersistence dbdir) transIds graph = transactionGraphPersist FsyncDiskSync dbdir transIds graph >> pure ()

readGraphTransactionIdDigest :: PersistenceStrategy -> IO LockFileHash
readGraphTransactionIdDigest NoPersistence = error "attempt to read digest from transaction log without persistence enabled"
readGraphTransactionIdDigest (MinimalPersistence dbdir) = readGraphTransactionIdFileDigest dbdir 
readGraphTransactionIdDigest (CrashSafePersistence dbdir) = readGraphTransactionIdFileDigest dbdir 

-- | Return a relation whose type would match that of the relational expression if it were executed. This is useful for checking types and validating a relational expression's types.
typeForRelationalExpr :: SessionId -> Connection -> RelationalExpr -> IO (Either RelationalError Relation)
typeForRelationalExpr sessionId conn@(InProcessConnection _) relExpr = atomically $ typeForRelationalExprSTM sessionId conn relExpr
typeForRelationalExpr sessionId conn@(RemoteProcessConnection _) relExpr = remoteCall conn (ExecuteTypeForRelationalExpr sessionId relExpr)
    
typeForRelationalExprSTM :: SessionId -> Connection -> RelationalExpr -> STM (Either RelationalError Relation)    
typeForRelationalExprSTM sessionId (InProcessConnection conf) relExpr = do
  let sessions = ipSessions conf
  eSession <- sessionAndSchema sessionId sessions
  case eSession of
    Left err -> pure $ Left err
    Right (session, schema) -> do
      let processed = if schemaName session == defaultSchemaName then
                       Right relExpr
                     else
                       Schema.processRelationalExprInSchema schema relExpr
      case processed of
        Left err -> pure (Left err)
        Right relExpr' -> pure $ runReader (RE.typeForRelationalExpr relExpr') (RE.mkRelationalExprState (Sess.concreteDatabaseContext session))
    
typeForRelationalExprSTM _ _ _ = error "typeForRelationalExprSTM called on non-local connection"

-- | Return a 'Map' of the database's constraints at the context of the session and connection.
inclusionDependencies :: SessionId -> Connection -> IO (Either RelationalError InclusionDependencies)
inclusionDependencies sessionId (InProcessConnection conf) = do
  let sessions = ipSessions conf
  atomically $ do
    eSession <- sessionAndSchema sessionId sessions
    case eSession of
      Left err -> pure $ Left err 
      Right (session, schema) -> do
            let context = Sess.concreteDatabaseContext session
            if schemaName session == defaultSchemaName then
              pure $ Right (B.inclusionDependencies context)
              else
              pure (Schema.inclusionDependenciesInSchema schema (B.inclusionDependencies context))

inclusionDependencies sessionId conn@(RemoteProcessConnection _) = remoteCall conn (RetrieveInclusionDependencies sessionId)

typeConstructorMapping :: SessionId -> Connection -> IO (Either RelationalError TypeConstructorMapping)
typeConstructorMapping sessionId (InProcessConnection conf) = do
  let sessions = ipSessions conf
  atomically $ do
    eSession <- sessionAndSchema sessionId sessions
    case eSession of
      Left err -> pure $ Left err 
      Right (session, _) -> --warning, no schema support for typeconstructors
        pure (Right (B.typeConstructorMapping (Sess.concreteDatabaseContext session)))
typeConstructorMapping sessionId conn@(RemoteProcessConnection _) = remoteCall conn (RetrieveTypeConstructorMapping sessionId)
  
-- | Return an optimized database expression which is logically equivalent to the input database expression. This function can be used to determine which expression will actually be evaluated.
planForDatabaseContextExpr :: SessionId -> Connection -> DatabaseContextExpr -> IO (Either RelationalError DatabaseContextExpr)  
planForDatabaseContextExpr sessionId (InProcessConnection conf) dbExpr = do
  let sessions = ipSessions conf
  atomically $ do
    eSession <- sessionAndSchema sessionId sessions
    case eSession of
      Left err -> pure $ Left err 
      Right (session, _) -> if schemaName session == defaultSchemaName then
                                   pure $ evalState (applyStaticDatabaseOptimization dbExpr) (RE.freshDatabaseState (Sess.concreteDatabaseContext session))
                                 else -- don't show any optimization because the current optimization infrastructure relies on access to the base context- this probably underscores the need for each schema to have its own DatabaseContext, even if it is generated on-the-fly
                                   pure (Right dbExpr)

planForDatabaseContextExpr sessionId conn@(RemoteProcessConnection _) dbExpr = remoteCall conn (RetrievePlanForDatabaseContextExpr sessionId dbExpr)
             
-- | Return a relation which represents the current state of the global transaction graph. The attributes are 
--    * current- boolean attribute representing whether or not the current session references this transaction
--    * head- text attribute which is a non-empty 'HeadName' iff the transaction references a head.
--    * id- id attribute of the transaction
--    * parents- a relation-valued attribute which contains a relation of transaction ids which are parent transaction to the transaction
transactionGraphAsRelation :: SessionId -> Connection -> IO (Either RelationalError Relation)
transactionGraphAsRelation sessionId (InProcessConnection conf) = do
  let sessions = ipSessions conf
      tvar = ipTransactionGraph conf
  atomically $ do
    eSession <- sessionForSessionId sessionId sessions
    case eSession of
      Left err -> pure $ Left err
      Right session ->
        graphAsRelation (Sess.disconnectedTransaction session) <$> readTVar tvar
    
transactionGraphAsRelation sessionId conn@(RemoteProcessConnection _) = remoteCall conn (RetrieveTransactionGraph sessionId) 

-- | Returns the names and types of the relation variables in the current 'Session'.
relationVariablesAsRelation :: SessionId -> Connection -> IO (Either RelationalError Relation)
relationVariablesAsRelation sessionId (InProcessConnection conf) = do
  let sessions = ipSessions conf
  atomically $ do
    eSession <- sessionAndSchema sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right (session, schema) -> do
        let context = Sess.concreteDatabaseContext session
        if Sess.schemaName session == defaultSchemaName then
          pure $ R.relationVariablesAsRelation (relationVariables context)
          else
          case Schema.relationVariablesInSchema schema context of
            Left err -> pure (Left err)
            Right relvars -> pure $ R.relationVariablesAsRelation relvars
      
relationVariablesAsRelation sessionId conn@(RemoteProcessConnection _) = remoteCall conn (RetrieveRelationVariableSummary sessionId)

-- | Returns the names and types of the atom functions in the current 'Session'.
atomFunctionsAsRelation :: SessionId -> Connection -> IO (Either RelationalError Relation)
atomFunctionsAsRelation sessionId (InProcessConnection conf) = do
  let sessions = ipSessions conf
  atomically $ do
    eSession <- sessionAndSchema sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right (session, _) -> 
        pure (AF.atomFunctionsAsRelation (atomFunctions (concreteDatabaseContext session)))
        
atomFunctionsAsRelation sessionId conn@(RemoteProcessConnection _) = remoteCall conn (RetrieveAtomFunctionSummary sessionId)        

databaseContextFunctionsAsRelation :: SessionId -> Connection -> IO (Either RelationalError Relation)
databaseContextFunctionsAsRelation sessionId (InProcessConnection conf) = do
  let sessions = ipSessions conf
  atomically $ do
    eSession <- sessionAndSchema sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right (session, _) ->
        pure (DCF.databaseContextFunctionsAsRelation (dbcFunctions (concreteDatabaseContext session)))

databaseContextFunctionsAsRelation sessionId conn@(RemoteProcessConnection _) = remoteCall conn (RetrieveDatabaseContextFunctionSummary sessionId)        

-- | Returns the transaction id for the connection's disconnected transaction committed parent transaction.  
headTransactionId :: SessionId -> Connection -> IO (Either RelationalError TransactionId)
headTransactionId sessionId (InProcessConnection conf) = do
  let sessions = ipSessions conf  
  atomically $ do
    eSession <- sessionForSessionId sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right session -> pure $ Right (Sess.parentId session)
headTransactionId sessionId conn@(RemoteProcessConnection _) = remoteCall conn (RetrieveHeadTransactionId sessionId)
    
headNameSTM_ :: SessionId -> Sessions -> TVar TransactionGraph -> STM (Either RelationalError HeadName)  
headNameSTM_ sessionId sessions graphTvar = do
    graph <- readTVar graphTvar
    eSession <- sessionForSessionId sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right session -> case transactionForId (Sess.parentId session) graph of
        Left err -> pure (Left err)
        Right parentTrans -> case headNameForTransaction parentTrans graph of
          Nothing -> pure (Left UnknownHeadError)
          Just headName' -> pure (Right headName')
  
-- | Returns Just the name of the head of the current disconnected transaction or Nothing.    
headName :: SessionId -> Connection -> IO (Either RelationalError HeadName)
headName sessionId (InProcessConnection conf) = do
  let sessions = ipSessions conf
      graphTvar = ipTransactionGraph conf
  atomically (headNameSTM_ sessionId sessions graphTvar)
headName sessionId conn@(RemoteProcessConnection _) = remoteCall conn (ExecuteHeadName sessionId)

-- | Returns a listing of all available atom types.
atomTypesAsRelation :: SessionId -> Connection -> IO (Either RelationalError Relation)
atomTypesAsRelation sessionId (InProcessConnection conf) = do
  let sessions = ipSessions conf
  atomically $ do
    eSession <- sessionForSessionId sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right session ->
        case typesAsRelation (B.typeConstructorMapping (Sess.concreteDatabaseContext session)) of
          Left err -> pure (Left err)
          Right rel -> pure (Right rel)
atomTypesAsRelation sessionId conn@(RemoteProcessConnection _) = remoteCall conn (RetrieveAtomTypesAsRelation sessionId)

disconnectedTransactionIsDirty :: SessionId -> Connection -> IO (Either RelationalError Bool)
disconnectedTransactionIsDirty sessionId (InProcessConnection conf) = do
  let sessions = ipSessions conf
  atomically $ do
    eSession <- sessionForSessionId sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right session ->
        pure (Right (isDirty session))
disconnectedTransactionIsDirty sessionId conn@(RemoteProcessConnection _) = remoteCall conn (RetrieveSessionIsDirty sessionId)
        
--used only for testing- we expect this to throw an exception
callTestTimeout_ :: SessionId -> Connection -> IO Bool
callTestTimeout_ _ (InProcessConnection _) = error "bad testing call"
callTestTimeout_ sessionId conn@(RemoteProcessConnection _) = remoteCall conn (TestTimeout sessionId)

--used in tests only
transactionGraph_ :: Connection -> IO TransactionGraph
transactionGraph_ (InProcessConnection conf) = readTVarIO (ipTransactionGraph conf)
transactionGraph_ _ = error "remote connection used"

--used in tests only
disconnectedTransaction_ :: SessionId -> Connection -> IO DisconnectedTransaction
disconnectedTransaction_ sessionId (InProcessConnection conf) = do
  let sessions = ipSessions conf
  mSession <- atomically $ StmMap.lookup sessionId sessions
  case mSession of
    Nothing -> error "No such session"
    Just (Sess.Session discon _) -> pure discon
disconnectedTransaction_ _ _= error "remote connection used"

-- wrap a graph evaluation in file locking
commitLock_ :: SessionId -> 
               InProcessConnectionConf -> 
               (TransactionGraph -> 
                STM (Either RelationalError (DisconnectedTransaction, TransactionGraph, [TransactionId]))) -> 
               IO (Either RelationalError ())
commitLock_ sessionId conf stmBlock = do
  let sessions = ipSessions conf
      strat = ipPersistenceStrategy conf      
      mScriptSession = ipScriptSession conf              
      graphTvar = ipTransactionGraph conf
      clientNodes = ipClientNodes conf      
      mLockFileH = ipLocks conf
      lockHandler body = case mLockFileH of
        Nothing -> body False
        Just (lockFileH, lockMVar) ->
          let acquireLocks = do
                lastWrittenDigest <- takeMVar lockMVar 
                lockFile lockFileH WriteLock
                latestDigest <- readGraphTransactionIdDigest strat
                pure (latestDigest /= lastWrittenDigest)
              releaseLocks _ = do
                --still holding the lock- get the latest digest
                gDigest <- readGraphTransactionIdDigest strat
                unlockFile lockFileH 
                putMVar lockMVar gDigest
          in bracket acquireLocks releaseLocks body
  manip <- lockHandler $ \dbWrittenByOtherProcess -> atomically $ do
     eSession <- sessionForSessionId sessionId sessions
     --handle graph update by other process
     oldGraph <- readTVar graphTvar
     case eSession of
      Left err -> pure (Left err)
      Right session -> do
        let dbdir = case strat of
              MinimalPersistence x -> x
              CrashSafePersistence x -> x
              _ -> error "accessing dbdir on non-persisted connection"
        --this should also happen for non-commit expressions
        eRefreshedGraph <- if dbWrittenByOtherProcess then
                             unsafeIOToSTM (transactionGraphLoad dbdir oldGraph mScriptSession)
                           else
                             pure (Right oldGraph)
        case eRefreshedGraph of
          Left err -> pure (Left (DatabaseLoadError err))
          Right refreshedGraph -> do
            eGraph <- stmBlock refreshedGraph
            case eGraph of
              Left err -> pure (Left err)
              Right (discon', graph', transactionIdsToPersist) -> do
                writeTVar graphTvar graph'
                let newSession = Session discon' (Sess.schemaName session)
                StmMap.insert newSession sessionId sessions
                case transactionForId (Sess.parentId session) oldGraph of
                  Left err -> pure $ Left err
                  Right previousTrans ->
                    if not (Prelude.null transactionIdsToPersist) then do
                      (evaldNots, nodes) <- executeCommitExprSTM_ (Trans.concreteDatabaseContext previousTrans) (Sess.concreteDatabaseContext session) clientNodes
                      nodesToNotify <- stmSetToList nodes
                      pure $ Right (evaldNots, nodesToNotify, graph', transactionIdsToPersist)
                    else pure (Right (M.empty, [], graph', []))

      --handle notification firing                
  case manip of 
    Left err -> pure (Left err)
    Right (notsToFire, nodesToNotify, newGraph, transactionIdsToPersist) -> do
      --update filesystem database, if necessary
      processTransactionGraphPersistence strat transactionIdsToPersist newGraph
      sendNotifications nodesToNotify (ipLocalNode conf) notsToFire
      pure (Right ())
{-
writeDisconAndGraph_ :: TVar TransactionGraph -> SessionId -> Session -> Sessions -> DisconnectedTransaction -> TransactionGraph  -> STM ()
writeDisconAndGraph_ graphTvar sessionId session sessions discon graph = do
  writeTVar graphTvar graph
  let newSession = Session discon (Sess.schemaName session)
  StmMap.insert newSession sessionId sessions
-}

-- | Runs an IO monad, commits the result when the monad returns no errors, otherwise, rolls back the changes and the error.
withTransaction :: SessionId -> Connection -> IO (Either RelationalError a) -> IO (Either RelationalError ()) -> IO (Either RelationalError a)
withTransaction sessionId conn io successFunc = bracketOnError (pure ()) (const do_rollback) block
  where
    do_rollback = rollback sessionId conn
    block _ = do
      eErr <- io
      case eErr of 
        Left err -> do
          _ <- do_rollback
          pure (Left err)
        Right val -> do
            eIsDirty <- disconnectedTransactionIsDirty sessionId conn
            case eIsDirty of
              Left err -> pure (Left err)
              Right dirty -> 
                if dirty then do
                  res <- successFunc
                  case res of
                    Left err -> pure (Left err)
                    Right _ -> pure (Right val)
                  else -- no updates executed, so don't create a commit
                  pure (Right val)

executeDataFrameExpr :: SessionId -> Connection -> DF.DataFrameExpr -> IO (Either RelationalError DF.DataFrame)
executeDataFrameExpr sessionId conn@(InProcessConnection _) dfExpr = do
  eRel <- executeRelationalExpr sessionId conn (DF.convertExpr dfExpr)
  case eRel of
    Left err -> pure (Left err)
    Right rel -> do
      let relAttrs = R.attributes rel
          attrName (DF.AttributeOrderExpr name _) = name
          order (DF.AttributeOrderExpr _ ord) = ord
          orders = map order (DF.orderExprs dfExpr)
          attributeForName' = flip attributeForName relAttrs 
          attrNames = map attrName (DF.orderExprs dfExpr)
          verified = forM attrNames attributeForName'
      case verified of
        Left err -> pure (Left err)
        Right attrs -> do
          let attrOrders = map (\(attr',order') -> DF.AttributeOrder (attributeName attr') order') (zip attrs orders)
          case DF.sortDataFrameBy attrOrders . DF.toDataFrame $ rel of
            Left err -> pure (Left err)
            Right dFrame -> do
              let dFrame' = maybe dFrame (`DF.drop'` dFrame) (DF.offset dfExpr)
                  dFrame'' = maybe dFrame' (`DF.take'` dFrame') (DF.limit dfExpr)
              pure (Right dFrame'')
executeDataFrameExpr sessionId conn@(RemoteProcessConnection _) dfExpr = remoteCall conn (ExecuteDataFrameExpr sessionId dfExpr)
        

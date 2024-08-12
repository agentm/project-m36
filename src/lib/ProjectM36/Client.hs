{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, MonoLocalBinds, DerivingVia #-}
{-|
Module: ProjectM36.Client

Client interface to local and remote Project:M36 databases. To get started, connect with 'connectProjectM36', then run some database changes with 'executeDatabaseContextExpr', and issue queries using 'executeRelationalExpr'.
-}
module ProjectM36.Client
       (ConnectionInfo(..),
       Connection(..),
       Port,
       Hostname,
       ServiceName,
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
       registeredQueriesAsRelation,
       ddlAsRelation,
       ProjectM36.Client.atomFunctionsAsRelation,
       disconnectedTransactionIsDirty,
       headName,
       remoteDBLookupName,
       defaultServerPort,
       headTransactionId,
       defaultDatabaseName,
       defaultRemoteConnectionInfo,
       defaultHeadName,
       addClientNode,
       getDDLHash,
       convertSQLQuery,
       convertSQLDBUpdates,
       PersistenceStrategy(..),
       RelationalExpr,
       RelationalExprBase(..),
       DatabaseContextExprBase(..),
       DatabaseContextExpr,
       DatabaseContextIOExprBase(..),
       DatabaseContextIOExpr,
       Attribute(..),
       MergeStrategy(..),
       attributesFromList,
       createSessionAtCommit,
       createSessionAtHead,
       closeSession,
       callTestTimeout_,
       RelationCardinality(..),
       TransactionGraphOperator(..),
       ProjectM36.Client.autoMergeToHead,
       transactionGraph_,
       disconnectedTransaction_,
       TransGraphRelationalExpr,
       TransactionIdLookup(..),
       TransactionIdHeadBacktrack(..),
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
       ProjectM36.Client.validateMerkleHashes,
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
       TupleExprsBase(..),
       AtomExprBase(..),
       RestrictionPredicateExprBase(..),
       withTransaction,
       basicDatabaseContext
       ) where
import ProjectM36.Base hiding (inclusionDependencies) --defined in this module as well
import qualified ProjectM36.Base as B
import ProjectM36.Serialise.Error ()
import ProjectM36.Error
import ProjectM36.DatabaseContext
import ProjectM36.Atomable
import ProjectM36.AtomFunction as AF
import ProjectM36.StaticOptimizer
import ProjectM36.Key
import qualified ProjectM36.DataFrame as DF
import ProjectM36.DatabaseContextFunction as DCF
import qualified ProjectM36.IsomorphicSchema as Schema
#if MIN_VERSION_base(4,16,0)
import Control.Monad (forever, forM, forM_, unless, void)
#endif
--import Control.Monad.State
import qualified ProjectM36.RelationalExpression as RE
import qualified ProjectM36.TransactionGraph as Graph
import ProjectM36.TransactionGraph as TG
import qualified ProjectM36.Transaction as Trans
import ProjectM36.TransactionGraph.Persist
import ProjectM36.Attribute
import ProjectM36.TransGraphRelationalExpression as TGRE (TransGraphRelationalExpr)
import ProjectM36.Persist (DiskSync(..))
import ProjectM36.FileLock
import ProjectM36.DDLType
import ProjectM36.NormalizeExpr
import ProjectM36.Notifications
import ProjectM36.Server.RemoteCallTypes
import qualified ProjectM36.DisconnectedTransaction as Discon
import ProjectM36.Relation (typesAsRelation)
import ProjectM36.ScriptSession (initScriptSession, ScriptSession)
import qualified ProjectM36.Relation as R
import Control.Exception.Base
import Control.Concurrent.STM
import Control.Concurrent.Async

import Data.Either (isRight)
import Data.UUID.V4 (nextRandom)
import Data.Word
import Data.Hashable
import Control.Concurrent.MVar
import Codec.Winery hiding (Schema, schema)
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
import ProjectM36.HashSecurely (SecureHash)
import ProjectM36.RegisteredQuery
import GHC.Generics (Generic)
import Control.DeepSeq (force)
import System.IO
import Data.Time.Clock
import qualified Network.RPC.Curryer.Client as RPC
import qualified Network.RPC.Curryer.Server as RPC
import Network.Socket (Socket, AddrInfo(..), getAddrInfo, defaultHints, AddrInfoFlag(..), SocketType(..), ServiceName, hostAddressToTuple, SockAddr(..))
import GHC.Conc (unsafeIOToSTM)
import ProjectM36.SQL.Select as SQL
import ProjectM36.SQL.DBUpdate as SQL
import ProjectM36.SQL.Convert 

type Hostname = String

type Port = Word16

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

-- | Construct a 'ConnectionInfo' to describe how to make the 'Connection'. The database can be run within the current process or running remotely via RPC.
data ConnectionInfo = InProcessConnectionInfo PersistenceStrategy NotificationCallback [GhcPkgPath] DatabaseContext |
                      RemoteConnectionInfo DatabaseName Hostname ServiceName NotificationCallback
                      
type EvaluatedNotifications = M.Map NotificationName EvaluatedNotification

-- | Used for callbacks from the server when monitored changes have been made.
newtype NotificationMessage = NotificationMessage EvaluatedNotifications
                           deriving (Eq, Show, Generic)
                           deriving Serialise via WineryVariant NotificationMessage

-- | When a notification is fired, the 'reportOldExpr' is evaluated in the commit's pre-change context while the 'reportNewExpr' is evaluated in the post-change context and they are returned along with the original notification.
data EvaluatedNotification = EvaluatedNotification {
  notification :: Notification,
  reportOldRelation :: Either RelationalError Relation,
  reportNewRelation :: Either RelationalError Relation
  }
  deriving (Eq, Show, Generic)
  deriving Serialise via WineryRecord EvaluatedNotification
                      

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
defaultRemoteConnectionInfo =
  RemoteConnectionInfo defaultDatabaseName defaultServerHostname (show defaultServerPort) emptyNotificationCallback

defaultServerHostname :: Hostname
defaultServerHostname = "localhost"

newtype RemoteConnectionConf = RemoteConnectionConf RPC.Connection
  
data Connection = InProcessConnection InProcessConnectionConf |
                  RemoteConnection RemoteConnectionConf
                  
-- | There are several reasons why a connection can fail.
data ConnectionError = SetupDatabaseDirectoryError PersistenceError |
                       IOExceptionError IOException |
                       NoSuchDatabaseByNameError DatabaseName |
                       DatabaseValidationError [MerkleValidationError] |
                       LoginError 
                       deriving (Show, Eq, Generic)
                  
remoteDBLookupName :: DatabaseName -> String    
remoteDBLookupName = (++) "db-" 

createScriptSession :: [String] -> IO (Maybe ScriptSession)  
createScriptSession ghcPkgPaths = do
  eScriptSession <- initScriptSession ghcPkgPaths
  case eScriptSession of
    Left err -> hPutStrLn stderr ("Warning: Haskell scripting disabled: " ++ show err) >> pure Nothing --not a fatal error, but the scripting feature must be disabled
    Right s -> pure (Just s)


-- | To create a 'Connection' to a remote or local database, create a 'ConnectionInfo' and call 'connectProjectM36'.
connectProjectM36 :: ConnectionInfo -> IO (Either ConnectionError Connection)
--create a new in-memory database/transaction graph
connectProjectM36 (InProcessConnectionInfo strat notificationCallback ghcPkgPaths bootstrapDatabaseContext) = do
  freshId <- nextRandom
  tstamp <- getCurrentTime
  let freshGraph = bootstrapTransactionGraph tstamp freshId bootstrapDatabaseContext
  case strat of
    --create date examples graph for now- probably should be empty context in the future
    NoPersistence -> do
        graphTvar <- newTVarIO freshGraph
        clientNodes <- StmSet.newIO
        sessions <- StmMap.newIO
        mScriptSession <- createScriptSession ghcPkgPaths
        notifAsync <- startNotificationListener clientNodes notificationCallback
        let conn = InProcessConnection InProcessConnectionConf {
                                           ipPersistenceStrategy = strat, 
                                           ipClientNodes = clientNodes, 
                                           ipSessions = sessions, 
                                           ipTransactionGraph = graphTvar, 
                                           ipScriptSession = mScriptSession,
                                           ipLocks = Nothing,
                                           ipCallbackAsync = notifAsync
                                           }
        pure (Right conn)
    MinimalPersistence dbdir -> connectPersistentProjectM36 strat NoDiskSync dbdir freshGraph notificationCallback ghcPkgPaths
    CrashSafePersistence dbdir -> connectPersistentProjectM36 strat FsyncDiskSync dbdir freshGraph notificationCallback ghcPkgPaths
        
connectProjectM36 (RemoteConnectionInfo dbName hostName servicePort notificationCallback) = do
  --TODO- add notification callback thread
  let resolutionHints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV],
                                       addrSocketType = Stream
                                       }
  resolved <- getAddrInfo (Just resolutionHints) (Just hostName) (Just servicePort)
  case resolved of
    [] -> error ("DNS resolution failed for" <> hostName <> ":" <> servicePort)
    addrInfo:_ -> do
      --supports IPv4 only for now
      let (port, addr) = case addrAddress addrInfo of
                           SockAddrInet p a -> (p, a)
                           _ -> error "no IPv4 address available (IPv6 not implemented)"
          notificationHandlers =
            [RPC.ClientAsyncRequestHandler $
             \(NotificationMessage notifications') ->
               forM_ (M.toList notifications') (uncurry notificationCallback)
            ]
      let connectExcHandler (e :: IOException) = pure $ Left (IOExceptionError e)
      eConn <- (Right <$> RPC.connect notificationHandlers (hostAddressToTuple addr) port) `catch` connectExcHandler
      case eConn of
        Left err -> pure (Left err)
        Right conn -> do
          eRet <- RPC.call conn (Login dbName)
          case eRet of
            Left err -> error (show err)
            Right False -> error "wtf"
            Right True ->
      --TODO handle connection errors!
              pure (Right (RemoteConnection (RemoteConnectionConf conn)))

--convert RPC errors into exceptions
convertRPCErrors :: RPC.ConnectionError -> IO a
convertRPCErrors err =
  case err of
    RPC.TimeoutError -> throw RequestTimeoutException
    RPC.CodecError msg -> error $ "decoding message failed on server: " <> msg
    RPC.ExceptionError msg -> error $ "server threw exception: " <> msg

addClientNode :: Connection -> RPC.Locking Socket -> IO ()
addClientNode (RemoteConnection _) _ = error "addClientNode called on remote connection"
addClientNode (InProcessConnection conf) lockSock = atomically (StmSet.insert clientInfo (ipClientNodes conf))
  where
    clientInfo = RemoteClientInfo lockSock

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
          case TG.validateMerkleHashes graph' of
            Left merkleErrs -> pure (Left (DatabaseValidationError merkleErrs))
            Right _ -> do
              tvarGraph <- newTVarIO graph'
              sessions <- StmMap.newIO
              clientNodes <- StmSet.newIO
              lockMVar <- newMVar digest
              notifAsync <- startNotificationListener clientNodes notificationCallback
              let conn = InProcessConnection InProcessConnectionConf {
                                             ipPersistenceStrategy = strat,
                                             ipClientNodes = clientNodes,
                                             ipSessions = sessions,
                                             ipTransactionGraph = tvarGraph,
                                             ipScriptSession = mScriptSession,
                                             ipLocks = Just (lockFileH, lockMVar),
                                             ipCallbackAsync = notifAsync
                                             }
              pure (Right conn)

--startup local async process to handle notification callbacks
startNotificationListener :: ClientNodes -> NotificationCallback -> IO (Async ())
startNotificationListener cNodes notificationCallback = do
  inProcessClientInfo@(InProcessClientInfo notifMVar) <- InProcessClientInfo <$> newEmptyMVar          
  atomically $ StmSet.insert inProcessClientInfo cNodes 
  async $ forever $ do
    notifs <- takeMVar notifMVar
    forM_ (M.toList notifs) $ uncurry notificationCallback

-- | Create a new session at the transaction id and return the session's Id.
createSessionAtCommit :: Connection -> TransactionId -> IO (Either RelationalError SessionId)
createSessionAtCommit conn@(InProcessConnection _) commitId = do
   newSessionId <- nextRandom
   atomically $ createSessionAtCommit_ commitId newSessionId conn
createSessionAtCommit conn@(RemoteConnection _) uuid = remoteCall conn (CreateSessionAtCommit uuid)

createSessionAtCommit_ :: TransactionId -> SessionId -> Connection -> STM (Either RelationalError SessionId)
createSessionAtCommit_ commitId newSessionId (InProcessConnection conf) = do
    let sessions = ipSessions conf
        graphTvar = ipTransactionGraph conf
    graph <- readTVar graphTvar
    case RE.transactionForId commitId graph of
        Left err -> pure (Left err)
        Right transaction -> do
            let freshDiscon = DisconnectedTransaction commitId (Discon.loadGraphRefRelVarsOnly commitId (Trans.schemas transaction)) False
            keyDuplication <- StmMap.lookup newSessionId sessions
            case keyDuplication of
                Just _ -> pure $ Left (SessionIdInUseError newSessionId)
                Nothing -> do
                   StmMap.insert (Session freshDiscon defaultSchemaName) newSessionId sessions
                   pure $ Right newSessionId
createSessionAtCommit_ _ _ (RemoteConnection _) = error "createSessionAtCommit_ called on remote connection"
  
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
createSessionAtHead conn@(RemoteConnection _) headn = remoteCall conn (CreateSessionAtHead headn)

-- | Discards a session, eliminating any uncommitted changes present in the session.
closeSession :: SessionId -> Connection -> IO ()
closeSession sessionId (InProcessConnection conf) = 
    atomically $ StmMap.delete sessionId (ipSessions conf)
closeSession sessionId conn@(RemoteConnection _) = remoteCall conn (CloseSession sessionId)       

-- | 'close' cleans up the database access connection and closes any relevant sockets.
close :: Connection -> IO ()
close (InProcessConnection conf) = do
  cancel (ipCallbackAsync conf)
  atomically $ do
    let sessions = ipSessions conf
#if MIN_VERSION_stm_containers(1,0,0)        
    StmMap.reset sessions
#else
    StmMap.deleteAll sessions
#endif
    pure ()
  let mLocks = ipLocks conf
  case mLocks of
    Nothing -> pure ()
    Just (lockFileH, _) -> closeLockFile lockFileH

close (RemoteConnection (RemoteConnectionConf conn)) =
  RPC.close conn

--used only by the server EntryPoints
closeRemote_ :: Connection -> IO ()
closeRemote_ (InProcessConnection _) = error "invalid call of closeRemote_ on InProcessConnection"
closeRemote_ (RemoteConnection (RemoteConnectionConf conn)) = RPC.close conn

  --we need to actually close the localNode's connection to the remote
--within the database server, we must catch and handle all exception lest they take down the database process- this handling might be different for other use-cases
--exceptions should generally *NOT* be thrown from any Project:M36 code paths, but third-party code such as AtomFunction scripts could conceivably throw undefined, etc.

excEither :: IO (Either RelationalError a) -> IO (Either RelationalError a)
excEither = handle handler
  where
    handler exc | Just (_ :: AsyncException) <- fromException exc = throwIO exc
                | otherwise = pure (Left (UnhandledExceptionError (show exc)))


remoteCall :: (Serialise a, Serialise b) => Connection -> a -> IO b
remoteCall (InProcessConnection _ ) _ = error "remoteCall called on local connection"
remoteCall (RemoteConnection (RemoteConnectionConf rpcConn)) arg = do
  eRet <- RPC.call rpcConn arg
  case eRet of
    Left err -> convertRPCErrors err
    Right val -> pure val

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
currentSchemaName sessionId conn@(RemoteConnection _) = remoteCall conn (RetrieveCurrentSchemaName sessionId)

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
setCurrentSchemaName sessionId conn@(RemoteConnection _) sname = remoteCall conn (ExecuteSetCurrentSchema sessionId sname)

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
        Right expr'' -> do
          let graphTvar = ipTransactionGraph conf
          graph <- readTVar graphTvar
          let reEnv = RE.mkRelationalExprEnv (Sess.concreteDatabaseContext session) graph
          case optimizeAndEvalRelationalExpr reEnv expr'' of
            Right rel -> pure (force (Right rel)) -- this is necessary so that any undefined/error exceptions are spit out here 
            Left err -> pure (Left err)

executeRelationalExpr sessionId conn@(RemoteConnection _) relExpr = remoteCall conn (ExecuteRelationalExpr sessionId relExpr)

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
        Right expr'' -> do
          graph <- readTVar (ipTransactionGraph conf)
          let ctx = Sess.concreteDatabaseContext session
              env = RE.mkDatabaseContextEvalEnv transId graph
              transId = Sess.parentId session
          case RE.runDatabaseContextEvalMonad ctx env (optimizeAndEvalDatabaseContextExpr True expr'') of
            Left err -> pure (Left err)
            Right newState ->
              if not (RE.dbc_dirty newState) then --nothing dirtied, nothing to do
                pure (Right ())
              else do
                let newDiscon = DisconnectedTransaction (Sess.parentId session) newSchemas True
                    context' = RE.dbc_context newState
                    newSubschemas = Schema.processDatabaseContextExprSchemasUpdate (Sess.subschemas session) expr
                    newSchemas = Schemas context' newSubschemas
                    newSession = Session newDiscon (Sess.schemaName session)
                StmMap.insert newSession sessionId sessions
                pure (Right ())
executeDatabaseContextExpr sessionId conn@(RemoteConnection _) dbExpr = remoteCall conn (ExecuteDatabaseContextExpr sessionId dbExpr)

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
autoMergeToHead sessionId conn@(RemoteConnection _) strat headName' = remoteCall conn (ExecuteAutoMergeToHead sessionId strat headName')
      
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
      graph <- readTVarIO (ipTransactionGraph conf)
      let env = RE.DatabaseContextIOEvalEnv transId graph scriptSession objFilesPath
          objFilesPath = objectFilesPath <$> persistenceDirectory (ipPersistenceStrategy conf)
          transId = Sess.parentId session
          context = Sess.concreteDatabaseContext session
      res <- RE.runDatabaseContextIOEvalMonad env context (optimizeAndEvalDatabaseContextIOExpr expr)
      case res of
        Left err -> pure (Left err)
        Right newState -> do
          let newDiscon = DisconnectedTransaction (Sess.parentId session) newSchemas False
              newSchemas = Schemas context' (Sess.subschemas session)
              newSession = Session newDiscon (Sess.schemaName session)
              context' = RE.dbc_context newState
          atomically $ StmMap.insert newSession sessionId sessions
          pure (Right ())
executeDatabaseContextIOExpr sessionId conn@(RemoteConnection _) dbExpr = remoteCall conn (ExecuteDatabaseContextIOExpr sessionId dbExpr)
         
-- process notifications for commits
executeCommitExprSTM_
  :: TransactionGraph
  -> DatabaseContext
  -> DatabaseContext
  -> ClientNodes
  -> STM (EvaluatedNotifications, ClientNodes)
executeCommitExprSTM_ graph oldContext newContext nodes = do
  let nots = notifications oldContext
      fireNots = notificationChanges nots graph oldContext newContext 
      evaldNots = M.map mkEvaldNot fireNots
      evalInContext expr ctx = optimizeAndEvalRelationalExpr (RE.mkRelationalExprEnv ctx graph) expr
 
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
            let transIds = [freshId | isRight (RE.transactionForId freshId graph')]
            pure (Right (discon', graph', transIds))

executeGraphExpr sessionId conn@(RemoteConnection _) graphExpr = remoteCall conn (ExecuteGraphExpr sessionId graphExpr)

-- | A trans-graph expression is a relational query executed against the entirety of a transaction graph.
executeTransGraphRelationalExpr :: SessionId -> Connection -> TransGraphRelationalExpr -> IO (Either RelationalError Relation)
executeTransGraphRelationalExpr _ (InProcessConnection conf) tgraphExpr = excEither . atomically $ do
  let graphTvar = ipTransactionGraph conf
  graph <- readTVar graphTvar
  pure $ force $ optimizeAndEvalTransGraphRelationalExpr graph tgraphExpr
executeTransGraphRelationalExpr sessionId conn@(RemoteConnection _) tgraphExpr = remoteCall conn (ExecuteTransGraphRelationalExpr sessionId tgraphExpr)  

-- | Schema expressions manipulate the isomorphic schemas for the current 'DatabaseContext'.
executeSchemaExpr :: SessionId -> Connection -> Schema.SchemaExpr -> IO (Either RelationalError ())
executeSchemaExpr sessionId (InProcessConnection conf) schemaExpr = atomically $ do
  let sessions = ipSessions conf
  eSession <- sessionAndSchema sessionId sessions  
  case eSession of
    Left err -> pure (Left err)
    Right (session, _) -> do
      let subschemas' = subschemas session
          transId = Sess.parentId session
          context = Sess.concreteDatabaseContext session
      graph <- readTVar (ipTransactionGraph conf)
      case Schema.evalSchemaExpr schemaExpr context transId graph subschemas' of
        Left err -> pure (Left err)
        Right (newSubschemas, newContext) -> do
          --hm- maybe we should start using lenses
          let discon = Sess.disconnectedTransaction session 
              newSchemas = Schemas newContext newSubschemas
              newSession = Session (DisconnectedTransaction (Discon.parentId discon) newSchemas False) (Sess.schemaName session)
          StmMap.insert newSession sessionId sessions
          pure (Right ())
executeSchemaExpr sessionId conn@(RemoteConnection _) schemaExpr = remoteCall conn (ExecuteSchemaExpr sessionId schemaExpr)          

-- | After modifying a 'DatabaseContext', 'commit' the transaction to the transaction graph at the head which the session is referencing. This will also trigger checks for any notifications which need to be propagated.
commit :: SessionId -> Connection -> IO (Either RelationalError ())
commit sessionId conn@(InProcessConnection _) = executeGraphExpr sessionId conn Commit 
commit sessionId conn@(RemoteConnection _) = remoteCall conn (ExecuteGraphExpr sessionId Commit)

sendNotifications :: [ClientInfo] -> EvaluatedNotifications -> IO ()
sendNotifications clients notifs =
  unless (M.null notifs) $ forM_ clients sender
 where
  sender (RemoteClientInfo sock) = RPC.sendMessage sock (NotificationMessage notifs)
  sender (InProcessClientInfo tvar) = putMVar tvar notifs

-- | Discard any changes made in the current 'Session' and 'DatabaseContext'. This resets the disconnected transaction to reference the original database context of the parent transaction and is a very cheap operation.
rollback :: SessionId -> Connection -> IO (Either RelationalError ())
rollback sessionId conn@(InProcessConnection _) = executeGraphExpr sessionId conn Rollback      
rollback sessionId conn@(RemoteConnection _) = remoteCall conn (ExecuteGraphExpr sessionId Rollback)

-- | Write the transaction graph to disk. This function can be used to incrementally write new transactions to disk.
processTransactionGraphPersistence :: PersistenceStrategy -> [TransactionId] -> TransactionGraph -> IO ()
processTransactionGraphPersistence NoPersistence _ _ = pure ()
processTransactionGraphPersistence (MinimalPersistence dbdir) transIds graph = void $ transactionGraphPersist NoDiskSync dbdir transIds graph
processTransactionGraphPersistence (CrashSafePersistence dbdir) transIds graph = void $ transactionGraphPersist FsyncDiskSync dbdir transIds graph

readGraphTransactionIdDigest :: PersistenceStrategy -> IO LockFileHash
readGraphTransactionIdDigest NoPersistence = error "attempt to read digest from transaction log without persistence enabled"
readGraphTransactionIdDigest (MinimalPersistence dbdir) = readGraphTransactionIdFileDigest dbdir 
readGraphTransactionIdDigest (CrashSafePersistence dbdir) = readGraphTransactionIdFileDigest dbdir 

-- | Return a relation whose type would match that of the relational expression if it were executed. This is useful for checking types and validating a relational expression's types.
typeForRelationalExpr :: SessionId -> Connection -> RelationalExpr -> IO (Either RelationalError Relation)
typeForRelationalExpr sessionId conn@(InProcessConnection _) relExpr = atomically $ typeForRelationalExprSTM sessionId conn relExpr
typeForRelationalExpr sessionId conn@(RemoteConnection _) relExpr = remoteCall conn (ExecuteTypeForRelationalExpr sessionId relExpr)
    
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
        Right relExpr' -> do
          graph <- readTVar (ipTransactionGraph conf)          
          let reEnv = RE.mkRelationalExprEnv (Sess.concreteDatabaseContext session) graph
          pure $ RE.runRelationalExprM reEnv (RE.typeForRelationalExpr relExpr') 
    
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

inclusionDependencies sessionId conn@(RemoteConnection _) = remoteCall conn (RetrieveInclusionDependencies sessionId)

typeConstructorMapping :: SessionId -> Connection -> IO (Either RelationalError TypeConstructorMapping)
typeConstructorMapping sessionId (InProcessConnection conf) = do
  let sessions = ipSessions conf
  atomically $ do
    eSession <- sessionAndSchema sessionId sessions
    case eSession of
      Left err -> pure $ Left err 
      Right (session, _) -> --warning, no schema support for typeconstructors
        pure (Right (B.typeConstructorMapping (Sess.concreteDatabaseContext session)))
typeConstructorMapping sessionId conn@(RemoteConnection _) = remoteCall conn (RetrieveTypeConstructorMapping sessionId)
  
-- | Return an optimized database expression which is logically equivalent to the input database expression. This function can be used to determine which expression will actually be evaluated.
planForDatabaseContextExpr :: SessionId -> Connection -> DatabaseContextExpr -> IO (Either RelationalError GraphRefDatabaseContextExpr)  
planForDatabaseContextExpr sessionId (InProcessConnection conf) dbExpr = do
  let sessions = ipSessions conf
  atomically $ do
    graph <- readTVar (ipTransactionGraph conf)    
    eSession <- sessionAndSchema sessionId sessions
    case eSession of
      Left err -> pure $ Left err 
      Right (session, _) ->
        if schemaName session == defaultSchemaName then do
          let ctx = Sess.concreteDatabaseContext session
              transId = Sess.parentId session
              gfExpr = runProcessExprM UncommittedContextMarker (processDatabaseContextExpr dbExpr)
          pure $ runGraphRefSOptDatabaseContextExprM transId ctx graph (optimizeGraphRefDatabaseContextExpr gfExpr)
        else -- don't show any optimization because the current optimization infrastructure relies on access to the base context- this probably underscores the need for each schema to have its own DatabaseContext, even if it is generated on-the-fly-}
          pure (Left NonConcreteSchemaPlanError)

planForDatabaseContextExpr sessionId conn@(RemoteConnection _) dbExpr = remoteCall conn (RetrievePlanForDatabaseContextExpr sessionId dbExpr)
             
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
    
transactionGraphAsRelation sessionId conn@(RemoteConnection _) = remoteCall conn (RetrieveTransactionGraph sessionId) 

-- | Returns the names and types of the relation variables in the current 'Session'.
relationVariablesAsRelation :: SessionId -> Connection -> IO (Either RelationalError Relation)
relationVariablesAsRelation sessionId (InProcessConnection conf) = do
  let sessions = ipSessions conf
  atomically $ do
    graph <- readTVar (ipTransactionGraph conf)
    eSession <- sessionAndSchema sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right (session, schema) -> do
        let context = Sess.concreteDatabaseContext session
        pure $ Schema.relationVariablesAsRelationInSchema context schema graph
      
relationVariablesAsRelation sessionId conn@(RemoteConnection _) = remoteCall conn (RetrieveRelationVariableSummary sessionId)

-- | Returns a relation representing the complete DDL of the current `DatabaseContext`.
ddlAsRelation :: SessionId -> Connection -> IO (Either RelationalError Relation)
ddlAsRelation sessionId (InProcessConnection conf) = do
  let sessions = ipSessions conf
  atomically $ do
    graph <- readTVar (ipTransactionGraph conf)
    eSession <- sessionAndSchema sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right (session, schema) -> do
        let context = Sess.concreteDatabaseContext session
        pure (ddlType schema context graph)
ddlAsRelation sessionId conn@RemoteConnection{} = remoteCall conn (RetrieveDDLAsRelation sessionId)

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
        
atomFunctionsAsRelation sessionId conn@(RemoteConnection _) = remoteCall conn (RetrieveAtomFunctionSummary sessionId)        

databaseContextFunctionsAsRelation :: SessionId -> Connection -> IO (Either RelationalError Relation)
databaseContextFunctionsAsRelation sessionId (InProcessConnection conf) = do
  let sessions = ipSessions conf
  atomically $ do
    eSession <- sessionAndSchema sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right (session, _) ->
        pure (DCF.databaseContextFunctionsAsRelation (dbcFunctions (concreteDatabaseContext session)))

databaseContextFunctionsAsRelation sessionId conn@(RemoteConnection _) = remoteCall conn (RetrieveDatabaseContextFunctionSummary sessionId)        

-- | Returns the transaction id for the connection's disconnected transaction committed parent transaction.  
headTransactionId :: SessionId -> Connection -> IO (Either RelationalError TransactionId)
headTransactionId sessionId (InProcessConnection conf) = do
  let sessions = ipSessions conf  
  atomically $ do
    eSession <- sessionForSessionId sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right session -> pure $ Right (Sess.parentId session)
headTransactionId sessionId conn@(RemoteConnection _) = remoteCall conn (RetrieveHeadTransactionId sessionId)
    
headNameSTM_ :: SessionId -> Sessions -> TVar TransactionGraph -> STM (Either RelationalError HeadName)  
headNameSTM_ sessionId sessions graphTvar = do
    graph <- readTVar graphTvar
    eSession <- sessionForSessionId sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right session -> case RE.transactionForId (Sess.parentId session) graph of
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
headName sessionId conn@(RemoteConnection _) = remoteCall conn (ExecuteHeadName sessionId)

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
atomTypesAsRelation sessionId conn@(RemoteConnection _) = remoteCall conn (RetrieveAtomTypesAsRelation sessionId)

disconnectedTransactionIsDirty :: SessionId -> Connection -> IO (Either RelationalError Bool)
disconnectedTransactionIsDirty sessionId (InProcessConnection conf) = do
  let sessions = ipSessions conf
  atomically $ do
    eSession <- sessionForSessionId sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right session ->
        pure (Right (isDirty session))
disconnectedTransactionIsDirty sessionId conn@(RemoteConnection _) = remoteCall conn (RetrieveSessionIsDirty sessionId)
        
--used only for testing- we expect this to throw an exception
callTestTimeout_ :: SessionId -> Connection -> IO Bool
callTestTimeout_ _ (InProcessConnection _) = error "bad testing call"
callTestTimeout_ sessionId conn@(RemoteConnection _) = remoteCall conn (TestTimeout sessionId)

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
                case RE.transactionForId (Sess.parentId session) oldGraph of
                  Left err -> pure $ Left err
                  Right previousTrans ->
                    if not (Prelude.null transactionIdsToPersist) then do
                      (evaldNots, nodes) <- executeCommitExprSTM_ graph' (Trans.concreteDatabaseContext previousTrans) (Sess.concreteDatabaseContext session) clientNodes
                      nodesToNotify <- stmSetToList nodes
                      pure $ Right (evaldNots, nodesToNotify, graph', transactionIdsToPersist)
                    else pure (Right (M.empty, [], graph', []))

      --handle notification firing                
  case manip of 
    Left err -> pure (Left err)
    Right (notsToFire, nodesToNotify, newGraph, transactionIdsToPersist) -> do
      --update filesystem database, if necessary
      processTransactionGraphPersistence strat transactionIdsToPersist newGraph
      sendNotifications nodesToNotify notsToFire
      pure (Right ())

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
          let attrOrders = zipWith
                            (DF.AttributeOrder . attributeName)
                           attrs
                           orders
          case DF.sortDataFrameBy attrOrders . DF.toDataFrame $ rel of
            Left err -> pure (Left err)
            Right dFrame -> do
              let dFrame' = maybe dFrame (`DF.drop'` dFrame) (DF.offset dfExpr)
                  dFrame'' = maybe dFrame' (`DF.take'` dFrame') (DF.limit dfExpr)
              pure (Right dFrame'')
executeDataFrameExpr sessionId conn@(RemoteConnection _) dfExpr = remoteCall conn (ExecuteDataFrameExpr sessionId dfExpr)
        
validateMerkleHashes :: SessionId -> Connection -> IO (Either RelationalError ())
validateMerkleHashes sessionId (InProcessConnection conf) = do
  let sessions = ipSessions conf
  atomically $ do
    eSession <- sessionForSessionId sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right _ -> do
        graph <- readTVar (ipTransactionGraph conf)
        case Graph.validateMerkleHashes graph of
          Left merkleErrs -> pure $ Left $ someErrors (map (\(MerkleValidationError tid expected actual) -> MerkleHashValidationError tid expected actual) merkleErrs)
          Right () -> pure (Right ())
validateMerkleHashes sessionId conn@RemoteConnection{} = remoteCall conn (ExecuteValidateMerkleHashes sessionId)

-- | Calculate a hash on the DDL of the current database context (not the graph). This is useful for validating on the client that the database schema meets the client's expectation. Any DDL change will change this hash. This hash does not change based on the current isomorphic schema being examined. This function is not affected by the current schema (since they are all isomorphic anyway, they should return the same hash).
getDDLHash :: SessionId -> Connection -> IO (Either RelationalError SecureHash)
getDDLHash sessionId (InProcessConnection conf) = do
  let sessions = ipSessions conf
  atomically $ do
    eSession <- sessionForSessionId sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right session -> do
        let ctx = Sess.concreteDatabaseContext session            
        graph <- readTVar (ipTransactionGraph conf)
        pure (ddlHash ctx graph)
getDDLHash sessionId conn@RemoteConnection{} = remoteCall conn (GetDDLHash sessionId)

-- | Convert a SQL Query expression into a DataFrameExpr. Because the conversion process requires substantial database metadata access (such as retrieving types for various subexpressions), we cannot process SQL client-side. However, the underlying DBMS is completely unaware that the resultant DataFrameExpr has come from SQL.
convertSQLQuery :: SessionId -> Connection -> Query -> IO (Either RelationalError DF.DataFrameExpr)
convertSQLQuery sessionId (InProcessConnection conf) query = do
  let sessions = ipSessions conf
      graphTvar = ipTransactionGraph conf  
  atomically $ do
    transGraph <- readTVar graphTvar    
    eSession <- sessionAndSchema sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right (session, _schema) -> do -- TODO: enable SQL to leverage isomorphic schemas
        let ctx = Sess.concreteDatabaseContext session
            reEnv = RE.mkRelationalExprEnv ctx transGraph
            typeF expr =
              RE.runRelationalExprM reEnv (RE.typeForRelationalExpr expr) 
        -- convert SQL data into DataFrameExpr
        case evalConvertM mempty (convertQuery typeF query) of
          Left err -> pure (Left (SQLConversionError err))
          Right dfExpr -> pure (Right dfExpr)
convertSQLQuery sessionId conn@RemoteConnection{} q = remoteCall conn (ConvertSQLQuery sessionId q)

convertSQLDBUpdates :: SessionId -> Connection -> [SQL.DBUpdate] -> IO (Either RelationalError DatabaseContextExpr)
convertSQLDBUpdates sessionId (InProcessConnection conf) updates = do
  let sessions = ipSessions conf
      graphTvar = ipTransactionGraph conf  
  atomically $ do
    transGraph <- readTVar graphTvar    
    eSession <- sessionAndSchema sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right (session, _schema) -> do -- TODO: enable SQL to leverage isomorphic schemas
        let ctx = Sess.concreteDatabaseContext session
            reEnv = RE.mkRelationalExprEnv ctx transGraph
            typeF = optimizeAndEvalRelationalExpr reEnv -- TODO: replace with typeForRelationalExpr
        -- convert SQL data into DataFrameExpr
        case evalConvertM mempty (convertDBUpdates typeF updates) of
          Left err -> pure (Left (SQLConversionError err))
          Right updateExpr -> pure (Right updateExpr)
convertSQLDBUpdates sessionId conn@RemoteConnection{} ups = remoteCall conn (ConvertSQLUpdates sessionId ups)   

registeredQueriesAsRelation :: SessionId -> Connection -> IO (Either RelationalError Relation)
registeredQueriesAsRelation sessionId (InProcessConnection conf) = do
  let sessions = ipSessions conf
  atomically $ do
    eSession <- sessionAndSchema sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right (session, schema) -> do
        let ctx = Sess.concreteDatabaseContext session        
        pure $ registeredQueriesAsRelationInSchema schema (registeredQueries ctx)
registeredQueriesAsRelation sessionId conn@RemoteConnection{} = remoteCall conn (RetrieveRegisteredQueries sessionId)        

type ClientNodes = StmSet.Set ClientInfo

-- internal structure specific to in-process connections
data InProcessConnectionConf = InProcessConnectionConf {
  ipPersistenceStrategy :: PersistenceStrategy, 
  ipClientNodes :: ClientNodes, 
  ipSessions :: Sessions, 
  ipTransactionGraph :: TVar TransactionGraph,
  ipScriptSession :: Maybe ScriptSession,
  ipLocks :: Maybe (LockFile, MVar LockFileHash), -- nothing when NoPersistence
  ipCallbackAsync :: Async ()
  }

-- clients may connect associate one socket/mvar with the server to register for change callbacks
data ClientInfo = RemoteClientInfo (RPC.Locking Socket) |
                  InProcessClientInfo (MVar EvaluatedNotifications)

instance Eq ClientInfo where
  (RemoteClientInfo a) == (RemoteClientInfo b) = RPC.lockless a == RPC.lockless b
  (InProcessClientInfo a) == (InProcessClientInfo b) = a == b
  _ == _ = False

instance Hashable ClientInfo where
  hashWithSalt salt (RemoteClientInfo sock) = hashWithSalt salt (show (RPC.lockless sock))
  hashWithSalt salt (InProcessClientInfo _) = hashWithSalt salt (1::Int)

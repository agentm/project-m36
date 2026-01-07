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
       TlsConfig(..),
       ConnectionError(..),
       connectProjectM36,
       close,
       closeRemote_,
       executeRelationalExpr,
       executeDatabaseContextExpr,
       executeDatabaseContextIOExpr,
       executeDataFrameExpr,
       executeTransactionGraphExpr,
       executeAlterTransactionGraphExpr,
       executeSchemaExpr,
       executeTransGraphRelationalExpr,
       commit,
       rollback,
       typeForRelationalExpr,
       inclusionDependencies,
       ProjectM36.Client.typeConstructorMapping,
       ProjectM36.Client.databaseContextFunctionsAsRelation,      
       planForDatabaseContextExpr,
       planForRelationalExpr,
       currentSchemaName,
       SchemaName,
       HeadName,
       setCurrentSchemaName,
       transactionGraphAsRelation,
       relationVariablesAsRelation,
       registeredQueriesAsRelation,
       notificationsAsRelation,
       ddlAsRelation,
       ProjectM36.Client.atomFunctionsAsRelation,
       disconnectedTransactionIsDirty,
       currentHead,
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
       ExtendTupleExprBase(..),
       DatabaseContextIOExprBase(..),
       DatabaseContextIOExpr,
       Attribute(..),
       MergeStrategy(..),
       attributesFromList,
       createSessionAtTransactionId,
       createSessionAtHead,
       closeSession,
       callTestTimeout_,
       RelationCardinality(..),
       TransactionGraphExpr(..),
       AlterTransactionGraphExpr(..),
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
       basicDatabaseContext,
       RemoteServerAddress(..),
       resolveRemoteServerAddress,
       defaultRemoteServerAddress,
       defaultServerHostname,
       Discon.CurrentHead(..),
       LoginRoles.adminRoleName,
       SRPC.ClientAuth(..),
       ClientInfo(..),
       InProcessConnectionConf(..),
       executeAlterLoginRolesExpr,
       setRoleName,
       LoginRoles.AlterLoginRolesExpr(..),
       LoginRoles.SuccessResult(..),
       SomePermission(..),
       FunctionPermission(..),
       DBCFunctionPermission(..),       
       AlterDBCACLExprBase(..),
       RelVarPermission(..),
       RoleName
       ) where
import ProjectM36.Base
import ProjectM36.Serialise.Error ()
import ProjectM36.Error
import qualified ProjectM36.DatabaseContext.Types as DBC
import ProjectM36.DatabaseContext.Basic
import qualified ProjectM36.DatabaseContext as DBC
import ProjectM36.DatabaseContext.Types (DatabaseContext, notifications)
import ProjectM36.Atomable
import ProjectM36.AtomFunction as AF
import ProjectM36.StaticOptimizer
import ProjectM36.Key
import qualified ProjectM36.DataFrame as DF
import ProjectM36.DatabaseContextFunction as DCF
import qualified ProjectM36.IsomorphicSchema as Schema
import Control.Monad (forever, forM, forM_, unless, void, when)
import qualified ProjectM36.RelationalExpression as RE
import qualified ProjectM36.TransactionGraph as Graph
import ProjectM36.TransactionGraph as TG
import qualified ProjectM36.Transaction.Types as Trans
import ProjectM36.IsomorphicSchema.Types hiding (subschemas, concreteDatabaseContext)
import ProjectM36.TransactionGraph.Persist
import ProjectM36.Attribute
import ProjectM36.TransGraphRelationalExpression as TGRE (TransGraphRelationalExpr, process, TransGraphEvalEnv(..))
import ProjectM36.Persist (DiskSync(..))
import ProjectM36.FileLock
import ProjectM36.DDLType
import ProjectM36.NormalizeExpr
import ProjectM36.Notifications
import ProjectM36.Server.RemoteCallTypes
import ProjectM36.GraphRefRelationalExpr
import ProjectM36.Streaming.RelationalExpression (planGraphRefRelationalExpr, renderPretty)
import qualified ProjectM36.DisconnectedTransaction as Discon
import ProjectM36.Relation (typesAsRelation)
import ProjectM36.ScriptSession (initScriptSession, ScriptSession)
import qualified ProjectM36.Relation as R
import Control.Exception.Base
import Control.Concurrent.STM
import Control.Concurrent.Async

import Data.UUID.V4 (nextRandom)
import Data.UUID (nil)
import Data.Word
import Data.Hashable
import Control.Concurrent.MVar
import Codec.Winery hiding (Schema, schema)
import qualified Data.Map as M
import qualified StmContainers.Map as StmMap
import qualified ProjectM36.Session as Sess
import ProjectM36.Session
import ProjectM36.ValueMarker
import ProjectM36.AccessControl
import ProjectM36.Sessions
import ProjectM36.AccessControlList
import ProjectM36.HashSecurely (SecureHash)
import ProjectM36.RegisteredQuery
import qualified ProjectM36.Cache.RelationalExprCache as RelExprCache
import ProjectM36.Cache.RelationalExprCache (RelExprCache)
import GHC.Generics (Generic)
import Control.DeepSeq (force)
import System.IO
import qualified Data.Text as T
import Data.Time.Clock
import qualified Network.RPC.Curryer.Client as CRPC
import qualified Network.RPC.Curryer.Server as SRPC
import Network.Socket (AddrInfo(..), getAddrInfo, defaultHints, SocketType(..), ServiceName, SockAddr, Family(..), SockAddr(..))
import GHC.Conc (unsafeIOToSTM)
import ProjectM36.SQL.Select as SQL
import ProjectM36.SQL.DBUpdate as SQL
import ProjectM36.SQL.Convert
import ProjectM36.TransactionGraph.Types
import ProjectM36.DisconnectedTransaction (DisconnectedTransaction(..))
import ProjectM36.DatabaseContextExpr
import qualified Data.Set as S
import qualified ProjectM36.LoginRoles as LoginRoles
import Streamly.Internal.Network.Socket (SockSpec(..))
import System.FilePath ((</>))
import Data.Maybe (catMaybes)
import qualified Network.Socket as Socket
import qualified Data.HashSet as HS
import System.Random (StdGen)

type Hostname = String
type Port = Word16

data RemoteServerAddress = RemoteServerHostAddress Hostname Port |
                           RemoteServerUnixDomainSocketAddress FilePath
                           deriving (Show)

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
data ConnectionInfo = InProcessConnectionInfo PersistenceStrategy NotificationCallback [GhcPkgPath] DBC.ResolvedDatabaseContext StdGen RoleName |
                      RemoteConnectionInfo DatabaseName RemoteServerAddress CRPC.ClientConnectionConfig NotificationCallback RoleName
                      
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

-- | Use this for connecting to the default remote server.
defaultRemoteServerAddress :: RemoteServerAddress
defaultRemoteServerAddress = RemoteServerHostAddress "127.0.0.1" defaultServerPort

-- | Create a connection configuration which connects to the localhost on the default server port and default server database name. The configured notification callback is set to ignore all events.
defaultRemoteConnectionInfo :: RoleName -> ConnectionInfo
defaultRemoteConnectionInfo =
  RemoteConnectionInfo defaultDatabaseName defaultRemoteServerAddress CRPC.defaultClientConnectionConfig emptyNotificationCallback

defaultServerHostname :: Hostname
defaultServerHostname = "localhost"

newtype RemoteConnectionConf = RemoteConnectionConf CRPC.Connection
  
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

-- | Resolve a server address using DNS, if necessary. The caller is expected to set any necessary socket options afterwards.
resolveRemoteServerAddress :: RemoteServerAddress -> IO (SockSpec, SockAddr)
resolveRemoteServerAddress (RemoteServerHostAddress hostname port) = do
  let addrHints = defaultHints { addrSocketType = Stream }
  hostAddrs <- getAddrInfo (Just addrHints) (Just hostname) (Just (show port))
  case hostAddrs of
    [] -> error "getAddrInfo returned zero matches"
    (AddrInfo _flags family socketType proto sockAddr _canonicalName:_) -> do
      let sockSpec = SockSpec { sockFamily = family,
                                sockType = socketType,
                                sockProto = proto,
                                sockOpts = [] }
      pure (sockSpec, sockAddr)
resolveRemoteServerAddress (RemoteServerUnixDomainSocketAddress sockPath) = do
  let sockSpec = SockSpec { sockFamily = AF_UNIX,
                            sockType = Stream,
                            sockProto = 0,
                            sockOpts = [] }
      sockAddr = SockAddrUnix sockPath
  pure (sockSpec, sockAddr)

-- | To create a 'Connection' to a remote or local database, create a 'ConnectionInfo' and call 'connectProjectM36'. Requires "login" permission to connect successfully.
connectProjectM36 :: ConnectionInfo -> IO (Either ConnectionError Connection)
--create a new in-memory database/transaction graph
connectProjectM36 (InProcessConnectionInfo strat notificationCallback ghcPkgPaths bootstrapDatabaseContext rando roleName) = do
  freshId <- nextRandom
  tstamp <- getCurrentTime
  let freshGraph = bootstrapTransactionGraph tstamp freshId (DBC.toDatabaseContext bootstrapDatabaseContext)
  case strat of
    NoPersistence -> do
        graphTvar <- newTVarIO freshGraph
        clientNodes <- StmMap.newIO
        sessions <- StmMap.newIO
        mScriptSession <- createScriptSession ghcPkgPaths
        notifAsync <- startNotificationListener clientNodes notificationCallback
        maxCacheSize <- RelExprCache.defaultUpperBound
        cache <- RelExprCache.empty maxCacheSize
        loginRoles <- LoginRoles.openNoPersistence
        LoginRoles.setupDatabaseIfNecessary loginRoles
        let conn = InProcessConnection InProcessConnectionConf {
                    ipPersistenceStrategy = strat, 
                    ipClientNodes = clientNodes, 
                    ipSessions = sessions, 
                    ipTransactionGraph = graphTvar, 
                    ipScriptSession = mScriptSession,
                    ipLocks = Nothing,
                    ipCallbackAsync = notifAsync,
                    ipRelExprCache = cache,
                    ipLoginRoles = loginRoles,
                    ipRoleName = roleName,
                    ipRandomGen = rando
                  }
        pure (Right conn)
    MinimalPersistence dbdir -> connectPersistentProjectM36 strat NoDiskSync dbdir freshGraph notificationCallback ghcPkgPaths rando roleName
    CrashSafePersistence dbdir -> connectPersistentProjectM36 strat FsyncDiskSync dbdir freshGraph notificationCallback ghcPkgPaths rando roleName
        
connectProjectM36 (RemoteConnectionInfo dbName remoteAddress connConfig notificationCallback roleName) = do
  (sockSpec, sockAddr) <- resolveRemoteServerAddress remoteAddress
  let notificationHandlers =
        [CRPC.ClientAsyncRequestHandler $
          \(NotificationMessage notifications') ->
            forM_ (M.toList notifications') (uncurry notificationCallback)
        ]
      connectExcHandler (e :: IOException) = pure $ Left (IOExceptionError e)
  eConn <- (Right <$> CRPC.connect notificationHandlers connConfig sockSpec sockAddr) `catch` connectExcHandler
  case eConn of
    Left err -> pure (Left err)
    Right conn -> do
      eRet <- CRPC.call conn (Login dbName roleName)
      case eRet of
        Left err -> error (show err)
        Right False -> error "wtf"
        Right True ->
      --TODO handle connection errors!
          pure (Right (RemoteConnection (RemoteConnectionConf conn)))

--convert RPC errors into exceptions
convertRPCErrors :: SRPC.ConnectionError -> IO a
convertRPCErrors err =
  case err of
    SRPC.TimeoutError -> throw RequestTimeoutException
    SRPC.CodecError msg -> error $ "decoding message failed on server: " <> msg
    SRPC.ExceptionError msg -> error $ "server threw exception: " <> msg

addClientNode :: Connection -> SRPC.ClientConnectionId -> SRPC.SocketContext -> RoleName -> IO ()
addClientNode (RemoteConnection _) _ _ _ = error "addClientNode called on remote connection"
addClientNode (InProcessConnection conf) clientId sockCtx roleName = do
  let lrdb = ipLoginRoles conf
      dropConn = SRPC.withLock (SRPC.lockingSocket sockCtx) $ \sock -> Socket.close sock
  LoginRoles.withTransaction lrdb $ do
    --check the role has login privilege
    eMayLogin <- LoginRoles.roleNameMayLogin roleName lrdb
    case eMayLogin of
      Left{} -> dropConn
      Right False -> dropConn
      Right True -> do
        -- collect roleids for rolename to pass to ACL validation functions
        eRoleIds <- LoginRoles.roleIdsForRoleName roleName lrdb
        case eRoleIds of
          Left{} -> dropConn
          Right _roleIds -> do
            let clientInfo = RemoteClientInfo sockCtx roleName
            atomically (StmMap.insert clientInfo clientId (ipClientNodes conf))

connectPersistentProjectM36 :: PersistenceStrategy ->
                               DiskSync ->
                               FilePath -> 
                               TransactionGraph ->
                               NotificationCallback ->
                               [GhcPkgPath] ->
                               StdGen ->
                               RoleName ->
                               IO (Either ConnectionError Connection)      
connectPersistentProjectM36 strat sync dbdir freshGraph notificationCallback ghcPkgPaths rando roleName = do
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
              clientNodes <- StmMap.newIO
              lockMVar <- newMVar digest
              notifAsync <- startNotificationListener clientNodes notificationCallback
              maxCacheSize <- RelExprCache.defaultUpperBound
              cache <- RelExprCache.empty maxCacheSize
              loginRoles <- LoginRoles.open (dbdir </> "loginroles.sqlite3")
              let conn = InProcessConnection InProcessConnectionConf {
                          ipPersistenceStrategy = strat,
                          ipClientNodes = clientNodes,
                          ipSessions = sessions,
                          ipTransactionGraph = tvarGraph,
                          ipScriptSession = mScriptSession,
                          ipLocks = Just (lockFileH, lockMVar),
                          ipCallbackAsync = notifAsync,
                          ipRelExprCache = cache,
                          ipLoginRoles = loginRoles,
                          ipRoleName = roleName,
                          ipRandomGen = rando
                        }
              pure (Right conn)

--startup local async process to handle notification callbacks
startNotificationListener :: ClientNodes -> NotificationCallback -> IO (Async ())
startNotificationListener cNodes notificationCallback = do
  inProcessClientInfo@(InProcessClientInfo notifMVar) <- InProcessClientInfo <$> newEmptyMVar          
  atomically $ StmMap.insert inProcessClientInfo nil cNodes 
  async $ forever $ do
    notifs <- takeMVar notifMVar
    forM_ (M.toList notifs) $ uncurry notificationCallback

-- | Create a new session at the transaction id and return the session's Id. 
createSessionAtTransactionId :: Connection -> TransactionId -> IO (Either RelationalError SessionId)
createSessionAtTransactionId conn@(InProcessConnection _) commitId = do
   newSessionId <- nextRandom
   atomically $ createSessionAtTransactionId_ Nothing commitId newSessionId conn
createSessionAtTransactionId conn@(RemoteConnection _) uuid = remoteCall conn (CreateSessionAtTransactionId uuid)

createSessionAtTransactionId_ :: Maybe HeadName -> TransactionId -> SessionId -> Connection -> STM (Either RelationalError SessionId)
createSessionAtTransactionId_ mHeadName commitId newSessionId (InProcessConnection conf) = do
    let sessions = ipSessions conf
        graphTvar = ipTransactionGraph conf
    graph <- readTVar graphTvar
    case RE.transactionForId commitId graph of
        Left err -> pure (Left err)
        Right transaction -> do
            let freshDiscon = Discon.freshTransaction' currentHead' commitId (Trans.schemas transaction)
                currentHead' = case mHeadName of
                                Just hname -> Discon.CurrentHeadBranch hname
                                Nothing -> Discon.CurrentHeadTransactionId commitId
            keyDuplication <- StmMap.lookup newSessionId sessions
            case keyDuplication of
                Just _ -> pure $ Left (SessionIdInUseError newSessionId)
                Nothing -> do
                   StmMap.insert (Session freshDiscon defaultSchemaName) newSessionId sessions
                   pure $ Right newSessionId
createSessionAtTransactionId_ _ _ _ (RemoteConnection _) = error "createSessionAtTransactionId_ called on remote connection"
  
-- | Call 'createSessionAtHead' with a transaction graph's head's name to create a new session pinned to that head. This function returns a 'SessionId' which can be used in other function calls to reference the point in the transaction graph.
createSessionAtHead :: Connection -> HeadName -> IO (Either RelationalError SessionId)
createSessionAtHead conn@(InProcessConnection conf) headn = do
    let graphTvar = ipTransactionGraph conf
    newSessionId <- nextRandom
    atomically $ do
        graph <- readTVar graphTvar
        case transactionForHead headn graph of
            Nothing -> pure $ Left (NoSuchHeadNameError headn)
            Just trans -> createSessionAtTransactionId_ (Just headn) (Trans.transactionId trans) newSessionId conn
createSessionAtHead conn@(RemoteConnection _) headn = remoteCall conn (CreateSessionAtHead headn)

-- | Discards a session, eliminating any uncommitted changes present in the session.
-- TODO: should sessions only be closed by the role which opened it? Yes. Otherwise we risk DoS attacks.
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
  CRPC.close conn

--used only by the server EntryPoints
closeRemote_ :: Connection -> IO ()
closeRemote_ (InProcessConnection _) = error "invalid call of closeRemote_ on InProcessConnection"
closeRemote_ (RemoteConnection (RemoteConnectionConf conn)) = CRPC.close conn

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
  eRet <- CRPC.call rpcConn arg
  case eRet of
    Left err -> convertRPCErrors err
    Right val -> pure val

sessionForSessionId :: SessionId -> Sessions -> STM (Either RelationalError Session)
sessionForSessionId sessionId sessions = 
  maybe (Left $ NoSuchSessionError sessionId) Right <$> StmMap.lookup sessionId sessions
  
schemaForSessionId :: Session -> InProcessConnectionConf -> STM (Either RelationalError Schema)
schemaForSessionId session conf = do
  let sname = schemaName session
  if sname == defaultSchemaName then
    pure (Right (Schema [])) -- the main schema includes no transformations (but neither do empty schemas :/ )
    else do
    graph <- readTVar (ipTransactionGraph conf)      
    case resolveSubschemas session graph of
      Left err -> pure (Left err)
      Right sschemas ->
        case M.lookup sname sschemas of
          Nothing -> pure (Left (SubschemaNameNotInUseError sname))
          Just schema -> pure (Right schema)
  
sessionAndSchema :: SessionId -> InProcessConnectionConf -> STM (Either RelationalError (Session, Schema))
sessionAndSchema sessionId conf = do
  let sessions = ipSessions conf  
  eSession <- sessionForSessionId sessionId sessions
  case eSession of
    Left err -> pure (Left err)
    Right session -> do
      eSchema <- schemaForSessionId session conf
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
    Right session -> do
      graph <- readTVar (ipTransactionGraph conf)  
      case Sess.setSchemaName sname session graph of
        Left err -> pure (Left err)
        Right newSession -> StmMap.insert newSession sessionId sessions >> pure (Right ())
setCurrentSchemaName sessionId conn@(RemoteConnection _) sname = remoteCall conn (ExecuteSetCurrentSchema sessionId sname)

-- | Execute a relational expression in the context of the session and connection. Relational expressions are queries and therefore cannot alter the database. Requires `AccessRelVars` permission if the relational expression includes a reference to a relation variable.
executeRelationalExpr :: SessionId -> Connection -> RelationalExpr -> IO (Either RelationalError Relation)
executeRelationalExpr sessionId (InProcessConnection conf) expr = do
  res <- excEither $ atomically $ do
   eSession <- sessionAndSchema sessionId conf
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
          let reEnv = RE.mkRelationalExprEnv dbctx graph
              dbctx = Sess.concreteDatabaseContext session
          pure (Right (graph, reEnv, expr''))
  case res of
    Left err -> pure (Left err)
    Right (graph, reEnv, rexpr) -> do
      case RE.resolveDBC' graph (RE.re_context reEnv) DBC.acl of
        Left err -> pure (Left err)
        Right acl' -> do
          roleIds <- roleIdsForRoleName conf
          case applyACLRelationalExpr roleIds (relvarsACL acl') rexpr of
            Left err -> pure (Left err)
            Right () -> do
              qres <- optimizeAndEvalRelationalExpr' reEnv rexpr (ipRelExprCache conf)
              case qres of
                Right rel -> pure (force (Right rel)) -- this is necessary so that any undefined/error exceptions are spit out here 
                Left err -> pure (Left err)

executeRelationalExpr sessionId conn@(RemoteConnection _) relExpr = remoteCall conn (ExecuteRelationalExpr sessionId relExpr)

-- | Execute a database context expression in the context of the session and connection. Database expressions modify the current session's disconnected transaction but cannot modify the transaction graph. Requires AccessRelVarsPermission. Requires ExecuteFunctionPermission if a function is executed.
executeDatabaseContextExpr :: SessionId -> Connection -> DatabaseContextExpr -> IO (Either RelationalError ())
executeDatabaseContextExpr sessionId (InProcessConnection conf) expr = do
 roleIds <- roleIdsForRoleName conf
 roles <- LoginRoles.allRoles (ipLoginRoles conf)
 excEither $ atomically $ do
  let sessions = ipSessions conf
  eSession <- sessionAndSchema sessionId conf
  case eSession of
    Left err -> pure (Left err)
    Right (session, schema) -> do
      let expr' = if schemaName session == defaultSchemaName then do
                    resolveRoleIds roleNameResolver expr
                  else 
                    resolveRoleIds roleNameResolver expr >>= Schema.processDatabaseContextExprInSchema schema
          roleNameResolver nam = fst <$> lookup nam roles
      case expr' of 
        Left err -> pure (Left err)
        Right expr'' -> do
          graph <- readTVar (ipTransactionGraph conf)
          let ctx = Sess.concreteDatabaseContext session
              dbcfuncutils = DBC.DatabaseContextFunctionUtils {
                DBC.executeDatabaseContextExpr = \ctx' dbexpr' -> 
                    case resolveRoleIds roleNameResolver dbexpr' of
                      Left err -> Left err
                      Right dbexpr'' ->
                        case RE.runDatabaseContextEvalMonad ctx' env (optimizeAndEvalDatabaseContextExpr True dbexpr'') of
                          Left err -> Left err
                          Right reState -> pure (RE.dbc_context reState)
                      ,
                DBC.executeRelationalExpr = \ctx' relExpr ->
                          let reEnv = RE.mkRelationalExprEnv ctx' graph in 
                          optimizeAndEvalRelationalExpr reEnv relExpr
                      }
              env = RE.mkDatabaseContextEvalEnv transId graph dbcfuncutils
              transId = Sess.parentId session
              runExpr = do
                applyACLDatabaseContextExpr roleIds expr''
                optimizeAndEvalDatabaseContextExpr True expr''
          case RE.runDatabaseContextEvalMonad ctx env runExpr of
            Left err -> pure (Left err)
            Right newState ->
              if not (DBC.isUpdated (RE.dbc_context newState)) then do --nothing dirtied, nothing to do
                pure (Right ())
              else do
                case resolveSubschemas session graph of
                  Left err -> pure (Left err)
                  Right sschemas -> do
                    let newDiscon = DisconnectedTransaction {
                          disconTransactionId = Sess.parentId session,
                          disconSchemas = newSchemas,
                          disconCurrentHead = Discon.disconCurrentHead (Sess.disconnectedTransaction session)
                          }
                        ctcontext = RE.dbc_context newState
                        newSubschemas = Schema.processDatabaseContextExprSchemasUpdate sschemas expr
                        newSubschemas' = if newSubschemas == sschemas then
                                           NotChangedSinceMarker transId
                                         else
                                           ValueMarker newSubschemas
                        newSchemas = Schemas ctcontext newSubschemas' -- the schemas in the disconnectedtransaction need to pass the ctdbc around until commit time
                        newSession = Session newDiscon (Sess.schemaName session)
                    StmMap.insert newSession sessionId sessions
                    pure (Right ())
executeDatabaseContextExpr sessionId conn@(RemoteConnection _) dbExpr = remoteCall conn (ExecuteDatabaseContextExpr sessionId dbExpr)

-- | Similar to a git rebase, 'autoMergeToHead' atomically creates a temporary branch and merges it to the latest commit of the branch referred to by the 'HeadName' and commits the merge. This is useful to reduce incidents of 'TransactionIsNotAHeadError's but at the risk of merge errors (thus making it similar to rebasing). Alternatively, as an optimization, if a simple commit is possible (meaning that the head has not changed), then a fast-forward commit takes place instead. Requires CommitTransactionPermission.
autoMergeToHead :: SessionId -> Connection -> MergeStrategy -> HeadName -> IO (Either RelationalError ())
autoMergeToHead sessionId (InProcessConnection conf) strat headName' = do
  let sessions = ipSessions conf
  id1 <- nextRandom
  id2 <- nextRandom
  id3 <- nextRandom
  tstamp <- getCurrentTime
  roleIds <- roleIdsForRoleName conf
  commitLock_ sessionId conf $ \graph -> do
    eSession <- sessionForSessionId sessionId sessions  
    case eSession of
      Left err -> pure (Left err)
      Right session -> 
        case Graph.transactionForHead headName' graph of
          Nothing -> pure (Left (NoSuchHeadNameError headName'))
          Just headTrans -> do
            --attempt fast-forward commit, if possible
            let disconIn = Sess.disconnectedTransaction session
                dbctx = Sess.concreteDatabaseContext session
            --check permission
            case RE.resolveDBC' graph dbctx DBC.acl of
              Left err -> pure (Left err)
              Right acl' -> do
                let alterTGACL = transGraphACL acl'
                case applyACLAlterTransGraphExpr roleIds alterTGACL Commit of
                  Left err -> pure (Left err)
                  Right () -> do
                    if Sess.parentId session == Trans.transactionId headTrans then do
                      let ret = Graph.evalAlterTransactionGraphExpr tstamp id1 disconIn graph Commit
                      case ret of
                        Left err -> pure (Left err)
                        Right (discon', mtrans', tGraph) ->
                          pure (Right (discon', TransactionGraphIncrementalWriteInfo {
                                          uncommittedTransactions = S.fromList (catMaybes [mtrans']),
                                          newGraph = tGraph
                                          }))
                      else do
                      pure $ Graph.autoMergeToHead tstamp (id1, id2, id3) disconIn headName' strat graph
autoMergeToHead sessionId conn@(RemoteConnection _) strat headName' = remoteCall conn (ExecuteAutoMergeToHead sessionId strat headName')
      
-- | Execute a database context IO-monad-based expression for the given session and connection. `DatabaseContextIOExpr`s modify the DatabaseContext but cannot be purely implemented. Requires ExecuteFunctionPermission.
--this is almost completely identical to executeDatabaseContextExpr above
executeDatabaseContextIOExpr :: SessionId -> Connection -> DatabaseContextIOExpr -> IO (Either RelationalError ())
executeDatabaseContextIOExpr sessionId (InProcessConnection conf) expr = do
 roleIds <- roleIdsForRoleName conf
 roles <- LoginRoles.allRoles (ipLoginRoles conf) 
 myRoleId <- primaryRoleIdForRoleName conf
 excEither $ do
  let sessions = ipSessions conf
      scriptSession = ipScriptSession conf
  eSession <- atomically $ sessionForSessionId sessionId sessions --potentially race condition due to interleaved IO?
  case eSession of
    Left err -> pure (Left err)
    Right session -> do
      graph <- readTVarIO (ipTransactionGraph conf)
      let env = RE.DatabaseContextIOEvalEnv transId graph scriptSession myRoleId objFilesPath dbcFuncUtils
          dbcEnv = RE.mkDatabaseContextEvalEnv transId graph dbcFuncUtils
          roleNameResolver nam = fst <$> lookup nam roles      
          dbcFuncUtils = DBC.DatabaseContextFunctionUtils {
            DBC.executeDatabaseContextExpr = \ctx' expr' ->
                    case resolveRoleIds roleNameResolver expr' of
                      Left err -> Left err
                      Right expr'' ->
                        case RE.runDatabaseContextEvalMonad ctx' dbcEnv (optimizeAndEvalDatabaseContextExpr True expr'') of
                          Left err -> Left err
                          Right reState -> pure (RE.dbc_context reState)
                      ,
            DBC.executeRelationalExpr = \ctx' relExpr ->
                          let reEnv = RE.mkRelationalExprEnv ctx' graph in 
                          optimizeAndEvalRelationalExpr reEnv relExpr
            }
          objFilesPath = objectFilesPath <$> persistenceDirectory (ipPersistenceStrategy conf)
          transId = Sess.parentId session
          context = Sess.concreteDatabaseContext session
          runExpr = do
            --check perms
            applyACLDatabaseContextIOExpr roleIds expr
            optimizeAndEvalDatabaseContextIOExpr expr
      res <- RE.runDatabaseContextIOEvalMonad env context runExpr
      case res of
        Left err -> pure (Left err)
        Right newState -> do
          let newDiscon = DisconnectedTransaction {
                disconTransactionId = Sess.parentId session,
                disconSchemas = newSchemas,
                disconCurrentHead = Discon.disconCurrentHead (Sess.disconnectedTransaction session)
                }
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
  nots <- case RE.resolveDBC' graph oldContext notifications of
            Left err -> throwSTM err
            Right nots' -> pure nots'
  let fireNots = notificationChanges nots graph oldContext newContext
      evaldNots = M.map mkEvaldNot fireNots
      evalInContext expr ctx = optimizeAndEvalRelationalExpr (RE.mkRelationalExprEnv ctx graph) expr
 
      mkEvaldNot notif = EvaluatedNotification { notification = notif, 
                                                 reportOldRelation = evalInContext (reportOldExpr notif) oldContext,
                                                 reportNewRelation = evalInContext (reportNewExpr notif) newContext}
  pure (evaldNots, nodes)
  
-- | Execute a transaction graph expression in the context of the session and connection. Transaction graph operators cannot modify the transaction graph state. Does not require any permission.
executeTransactionGraphExpr :: SessionId -> Connection -> TransactionGraphExpr -> IO (Either RelationalError ())
executeTransactionGraphExpr sessionId (InProcessConnection conf) graphExpr = excEither $ do
  let sessions = ipSessions conf
  commitLock_ sessionId conf $ \updatedGraph -> do
    eSession <- sessionForSessionId sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right session -> do
        let discon = Sess.disconnectedTransaction session
        case evalTransactionGraphExpr discon updatedGraph graphExpr of
          Left err -> pure (Left err)
          Right discon' -> do
            pure (Right (discon', TransactionGraphIncrementalWriteInfo {
                            uncommittedTransactions = mempty,
                            newGraph = updatedGraph
                            }))

executeTransactionGraphExpr sessionId conn@(RemoteConnection _) graphExpr = remoteCall conn (ExecuteGraphExpr sessionId graphExpr)

-- | Execute an operator which alters the transaction graph such as commit, branching, or merge. Requires CommitTransactionPermission.
executeAlterTransactionGraphExpr :: SessionId -> Connection -> AlterTransactionGraphExpr -> IO (Either RelationalError ())
executeAlterTransactionGraphExpr sessionId (InProcessConnection conf) alterGraphExpr = do
  let sessions = ipSessions conf
  freshId <- nextRandom
  tstamp <- getCurrentTime
  roleIds <- roleIdsForRoleName conf
  commitLock_ sessionId conf $ \updatedGraph -> do
    eSession <- sessionForSessionId sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right session -> do
        let discon = Sess.disconnectedTransaction session
            dbctx = Sess.concreteDatabaseContext session        
        case RE.resolveDBC' updatedGraph dbctx DBC.acl of
          Left err -> pure (Left err)
          Right acl' -> do
            let alterTGACL = transGraphACL acl'
            case applyACLAlterTransGraphExpr roleIds alterTGACL alterGraphExpr of
              Left err -> pure (Left err)
              Right () -> do

                case evalAlterTransactionGraphExpr tstamp freshId discon updatedGraph alterGraphExpr of
                  Left err -> pure (Left err)
                  Right (discon', mtrans', graph') -> do
                    pure (Right (discon', TransactionGraphIncrementalWriteInfo {
                                    uncommittedTransactions = S.fromList (catMaybes [mtrans']),
                                    newGraph = graph'
                                    }))
executeAlterTransactionGraphExpr sessionId conn@RemoteConnection{} alterGraphExpr =
  remoteCall conn (ExecuteAlterTransactionGraphExpr sessionId alterGraphExpr)

-- | A trans-graph expression is a relational query executed against the entirety of a transaction graph. Requires AccessRelVarsPermission if the expression mentions a relation variable.
executeTransGraphRelationalExpr :: SessionId -> Connection -> TransGraphRelationalExpr -> IO (Either RelationalError Relation)
executeTransGraphRelationalExpr sessionId (InProcessConnection conf) tgraphExpr = do
  roleIds <- roleIdsForRoleName conf
  eGraph <- atomically $ do
    eSession <- sessionAndSchema sessionId conf    
    case eSession of
      Left err -> pure $ Left err
      Right (session, _schema) -> do
        let dbctx = Sess.concreteDatabaseContext session
            graphTvar = ipTransactionGraph conf
        graph <- readTVar graphTvar
        case RE.resolveDBC' graph dbctx DBC.acl of
          Left err -> pure (Left err)
          Right acl' -> do
            let rvACL = relvarsACL acl'
            case TGRE.process (TransGraphEvalEnv graph) tgraphExpr of
              Left err -> pure (Left err)
              Right gfExpr -> do
                -- security check- get rvs mentioned at any transaction ids
                let rvMentionedMarkers = decomposeGraphRefTransactionMarkers gfExpr
                    -- collect acls from those transactions
                    lookupACL UncommittedContextMarker = do
                      applyACLRelationalExpr roleIds rvACL (RelationVariable "true" ())
                    lookupACL (TransactionMarker tid) = do
                      trans <- TG.lookupTransaction graph (TransactionIdLookup tid)
                      let dbctx' = Trans.concreteDatabaseContext trans
                      rvacl <- relvarsACL <$> RE.resolveDBC' graph dbctx' DBC.acl
                      applyACLRelationalExpr roleIds rvacl (RelationVariable "true" ())
                case forM_ rvMentionedMarkers (\(marker, _rvname) -> lookupACL marker) of
                  Left err -> pure (Left err)
                  Right () -> pure (Right graph)
  case eGraph of
    Left err -> pure (Left err)
    Right graph -> 
      optimizeAndEvalTransGraphRelationalExprWithCache (ipRandomGen conf) graph tgraphExpr (ipRelExprCache conf)
  
executeTransGraphRelationalExpr sessionId conn@(RemoteConnection _) tgraphExpr = remoteCall conn (ExecuteTransGraphRelationalExpr sessionId tgraphExpr)  

-- | Schema expressions manipulate the isomorphic schemas for the current 'DatabaseContext'. Requires AlterSchemaPermission.
executeSchemaExpr :: SessionId -> Connection -> Schema.SchemaExpr -> IO (Either RelationalError ())
executeSchemaExpr sessionId (InProcessConnection conf) schemaExpr = do
 roleIds <- roleIdsForRoleName conf
 roles <- LoginRoles.allRoles (ipLoginRoles conf)  
 atomically $ do
  let sessions = ipSessions conf
  eSession <- sessionAndSchema sessionId conf
  graph <- readTVar (ipTransactionGraph conf)
  case eSession of
    Left err -> pure (Left err)
    Right (session, _) -> do
      case resolveSubschemas session graph of
        Left err -> pure (Left err)
        Right subschemas' -> do
          let transId = Sess.parentId session
              context = Sess.concreteDatabaseContext session
              roleNameResolver nam = fst <$> lookup nam roles
          case RE.resolveDBC' graph context DBC.acl of
            Left err -> pure (Left err)
            Right acl' ->
              case applyACLSchemaExpr roleIds (schemaACL acl') schemaExpr of
                Left err -> pure (Left err)
                Right () -> do
                  let dbcEnv = RE.mkDatabaseContextEvalEnv transId graph dbcFunctionUtils
                      dbcFunctionUtils = DBC.DatabaseContextFunctionUtils {
                        DBC.executeDatabaseContextExpr =
                            \ctx' dbexpr ->
                              case resolveRoleIds roleNameResolver dbexpr of
                                Left err -> Left err
                                Right expr'' ->
                                  case RE.runDatabaseContextEvalMonad ctx' dbcEnv (optimizeAndEvalDatabaseContextExpr True expr'') of
                                    Left err -> Left err
                                    Right reState -> pure (RE.dbc_context reState)
                              ,
                        DBC.executeRelationalExpr =
                        \ctx' relExpr ->
                          let reEnv = RE.mkRelationalExprEnv ctx' graph in 
                          optimizeAndEvalRelationalExpr reEnv relExpr
                        }
                  case Schema.evalSchemaExpr schemaExpr context transId graph dbcFunctionUtils subschemas' of
                    Left err -> pure (Left err)
                    Right (newSubschemas, newContext) -> do
                      --hm- maybe we should start using lenses
                      let discon = Sess.disconnectedTransaction session 
                          newSchemas = Schemas newContext (ValueMarker newSubschemas)
                          newSession = Session (DisconnectedTransaction {
                                                   disconTransactionId = Discon.parentId discon,
                                                   disconSchemas = newSchemas,
                                                   disconCurrentHead = Discon.disconCurrentHead (Sess.disconnectedTransaction session) }) (Sess.schemaName session)
                      StmMap.insert newSession sessionId sessions
                      pure (Right ())
executeSchemaExpr sessionId conn@(RemoteConnection _) schemaExpr = remoteCall conn (ExecuteSchemaExpr sessionId schemaExpr)          

-- | After modifying a 'DatabaseContext', 'commit' the transaction to the transaction graph at the head which the session is referencing. This will also trigger checks for any notifications which need to be propagated.
commit :: SessionId -> Connection -> IO (Either RelationalError ())
commit sessionId conn@(InProcessConnection _) = executeAlterTransactionGraphExpr sessionId conn Commit 
commit sessionId conn@(RemoteConnection _) = remoteCall conn (ExecuteAlterTransactionGraphExpr sessionId Commit)

-- | Sends notifications to client who have registered notifications. Only sends notifications to roles which have the AccessRelVarsPermission.
sendNotifications :: InProcessConnectionConf -> RelVarAccessControlList -> [ClientInfo] -> EvaluatedNotifications -> IO ()
sendNotifications conf acl' clients notifs =
  unless (M.null notifs) $ forM_ clients sender
 where
   --check if role has access to relvars based on the context- make sure that roles which have had permission in the past but have had it revoked no longer get notifications- find a solution to send notifications for changes in DBC function results.
  sender (RemoteClientInfo sock roleName) = do
    eRoles <- LoginRoles.roleIdsForRoleName roleName (ipLoginRoles conf)
    case eRoles of
      Left _err -> pure ()
      Right roleIds ->
        when (hasAccess roleIds AccessRelVarsPermission acl') $
          SRPC.sendMessage sock (NotificationMessage notifs)
  sender (InProcessClientInfo tvar) = putMVar tvar notifs

-- | Discard any changes made in the current 'Session' and 'DatabaseContext'. This resets the disconnected transaction to reference the original database context of the parent transaction and is a very cheap operation. Requires CommitTransactionPermission.
rollback :: SessionId -> Connection -> IO (Either RelationalError ())
rollback sessionId conn@(InProcessConnection _) = executeAlterTransactionGraphExpr sessionId conn Rollback >> pure (Right ())
rollback sessionId conn@(RemoteConnection _) = remoteCall conn (ExecuteAlterTransactionGraphExpr sessionId Rollback)

-- | Write the transaction graph to disk. This function can be used to incrementally write new transactions to disk.
processTransactionGraphPersistence :: PersistenceStrategy -> TransactionGraphIncrementalWriteInfo -> IO ()
processTransactionGraphPersistence NoPersistence _ = pure ()
processTransactionGraphPersistence (MinimalPersistence dbdir) tWriteInfo = void $ transactionGraphPersistIncremental NoDiskSync dbdir tWriteInfo
processTransactionGraphPersistence (CrashSafePersistence dbdir) tWriteInfo = void $ transactionGraphPersistIncremental FsyncDiskSync dbdir tWriteInfo

readGraphTransactionIdDigest :: PersistenceStrategy -> IO LockFileHash
readGraphTransactionIdDigest NoPersistence = error "attempt to read digest from transaction log without persistence enabled"
readGraphTransactionIdDigest (MinimalPersistence dbdir) = readGraphTransactionIdFileDigest dbdir 
readGraphTransactionIdDigest (CrashSafePersistence dbdir) = readGraphTransactionIdFileDigest dbdir 

-- | Return a relation whose type would match that of the relational expression if it were executed. This is useful for checking types and validating a relational expression's types. If relvars are mentioned, requires AccessRelVarsPermission. If functions are mentioned, requires ViewFunctionPermission.
typeForRelationalExpr :: SessionId -> Connection -> RelationalExpr -> IO (Either RelationalError Relation)
typeForRelationalExpr sessionId (InProcessConnection conf) relExpr = do
  roleIds <- roleIdsForRoleName conf
  atomically $ do
    eSession <- sessionAndSchema sessionId conf
    case eSession of
      Left err -> pure $ Left err
      Right (session, _schema) -> do
        graph <- readTVar (ipTransactionGraph conf)
        let dbctx = Sess.concreteDatabaseContext session
            reEnv = RE.mkRelationalExprEnv dbctx graph
        case RE.resolveDBC' graph (RE.re_context reEnv) DBC.acl of
          Left err -> pure (Left err)
          Right acl' -> do
            case applyACLRelationalExpr roleIds (relvarsACL acl') relExpr of
              Left err -> pure (Left err)
              Right () ->
                typeForRelationalExprSTM sessionId conf relExpr
typeForRelationalExpr sessionId conn@(RemoteConnection _) relExpr = remoteCall conn (ExecuteTypeForRelationalExpr sessionId relExpr)
    
typeForRelationalExprSTM :: SessionId -> InProcessConnectionConf -> RelationalExpr -> STM (Either RelationalError Relation)    
typeForRelationalExprSTM sessionId conf relExpr = do
  eSession <- sessionAndSchema sessionId conf
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
          
-- | Return a 'Map' of the database's constraints at the context of the session and connection.
inclusionDependencies :: SessionId -> Connection -> IO (Either RelationalError InclusionDependencies)
inclusionDependencies sessionId (InProcessConnection conf) = do
  atomically $ do
    eSession <- sessionAndSchema sessionId conf
    case eSession of
      Left err -> pure $ Left err 
      Right (session, schema) -> do
            graph <- readTVar (ipTransactionGraph conf)        
            let context = Sess.concreteDatabaseContext session
            case RE.resolveDBC' graph context DBC.inclusionDependencies of
              Left err -> pure (Left err)
              Right incDeps -> do
                if schemaName session == defaultSchemaName then
                  pure (Right incDeps)
                  else
                    pure $ Schema.inclusionDependenciesInSchema schema incDeps

inclusionDependencies sessionId conn@(RemoteConnection _) = remoteCall conn (RetrieveInclusionDependencies sessionId)

typeConstructorMapping :: SessionId -> Connection -> IO (Either RelationalError TypeConstructorMapping)
typeConstructorMapping sessionId (InProcessConnection conf) = do
  atomically $ do
    eSession <- sessionAndSchema sessionId conf
    case eSession of
      Left err -> pure $ Left err 
      Right (session, _) -> do --warning, no schema support for typeconstructors
        let context = Sess.concreteDatabaseContext session
        graph <- readTVar (ipTransactionGraph conf)            
        case RE.resolveDBC' graph context DBC.typeConstructorMapping of
          Left err -> pure (Left err)
          Right tConsMap ->
            pure (Right tConsMap)
typeConstructorMapping sessionId conn@(RemoteConnection _) = remoteCall conn (RetrieveTypeConstructorMapping sessionId)
  
-- | Return an optimized database expression which is logically equivalent to the input database expression. This function can be used to determine which expression will actually be evaluated.
planForDatabaseContextExpr :: SessionId -> Connection -> DatabaseContextExpr -> IO (Either RelationalError GraphRefDatabaseContextExpr)  
planForDatabaseContextExpr sessionId (InProcessConnection conf) dbExpr = do
  roles <- LoginRoles.allRoles (ipLoginRoles conf)
  atomically $ do
    graph <- readTVar (ipTransactionGraph conf)    
    eSession <- sessionAndSchema sessionId conf
    case eSession of
      Left err -> pure $ Left err 
      Right (session, _) ->
        if schemaName session == defaultSchemaName then do
          let ctx = Sess.concreteDatabaseContext session
              transId = Sess.parentId session
              gfExpr = runProcessExprM UncommittedContextMarker (processDatabaseContextExpr dbExpr)
              roleNameResolver nam = fst <$> lookup nam roles
              roleIdResolver roleId = lookup roleId (map (\(a,(b,_c)) -> (b,a)) roles)
          case resolveRoleIds roleNameResolver gfExpr of
            Left err -> pure (Left err)
            Right gfExpr' -> do
              let dbcFuncUtils = DBC.DatabaseContextFunctionUtils {
                    DBC.executeDatabaseContextExpr = undefined,
                    DBC.executeRelationalExpr = undefined
                    }
              case runGraphRefSOptDatabaseContextExprM transId ctx graph dbcFuncUtils (optimizeGraphRefDatabaseContextExpr gfExpr') of
                Left err -> pure (Left err)
                Right optExpr -> 
              -- convert roleIds back roleNames to avoid leaking role ids to the client
                  case resolveRoleNames roleIdResolver optExpr of
                    Left err -> pure (Left err)
                    Right optExpr' -> pure (Right optExpr')
        else -- don't show any optimization because the current optimization infrastructure relies on access to the base context- this probably underscores the need for each schema to have its own DatabaseContext, even if it is generated on-the-fly-}
          pure (Left NonConcreteSchemaPlanError)

planForDatabaseContextExpr sessionId conn@(RemoteConnection _) dbExpr = remoteCall conn (RetrievePlanForDatabaseContextExpr sessionId dbExpr)

planForRelationalExpr :: SessionId -> Connection -> RelationalExpr -> IO (Either RelationalError T.Text)
planForRelationalExpr sessionId (InProcessConnection conf) rexpr = do
  atomically $ do
    graph <- readTVar (ipTransactionGraph conf)    
    eSession <- sessionAndSchema sessionId conf
    case eSession of
      Left err -> pure $ Left err 
      Right (session, schema) ->
        case  Schema.processRelationalExprInSchema schema rexpr of
          Left err -> pure (Left err)
          Right rexpr' -> do
            let ctx = Sess.concreteDatabaseContext session
                gfExpr = runProcessExprM UncommittedContextMarker (processRelationalExpr rexpr')
                gfEnv = RE.freshGraphRefRelationalExprEnv (Just ctx) graph
            pure (renderPretty <$> planGraphRefRelationalExpr gfExpr gfEnv)

planForRelationalExpr sessionId conn@RemoteConnection{} expr = remoteCall conn (RetrievePlanForRelationalExpr sessionId expr)

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

-- | Alter roles (users) which can login to the database.
executeAlterLoginRolesExpr :: SessionId -> Connection -> LoginRoles.AlterLoginRolesExpr -> IO (Either LoginRoles.LoginRoleError LoginRoles.SuccessResult)
executeAlterLoginRolesExpr _sessionId (InProcessConnection conf) expr = do
  LoginRoles.executeAlterLoginRolesExpr (ipRoleName conf) (ipLoginRoles conf) expr
executeAlterLoginRolesExpr sessionId conn@RemoteConnection{} expr = do
  remoteCall conn (ExecuteAlterLoginRolesExpr sessionId expr)

-- | Returns the names and types of the relation variables in the current 'Session'.
relationVariablesAsRelation :: SessionId -> Connection -> IO (Either RelationalError Relation)
relationVariablesAsRelation sessionId (InProcessConnection conf) = do
  atomically $ do
    graph <- readTVar (ipTransactionGraph conf)
    eSession <- sessionAndSchema sessionId conf
    case eSession of
      Left err -> pure (Left err)
      Right (session, schema) -> do
        let context = Sess.concreteDatabaseContext session
        pure $ Schema.relationVariablesAsRelationInSchema context schema graph
      
relationVariablesAsRelation sessionId conn@(RemoteConnection _) = remoteCall conn (RetrieveRelationVariableSummary sessionId)

-- | Returns a relation representing the complete DDL of the current `DatabaseContext`.
ddlAsRelation :: SessionId -> Connection -> IO (Either RelationalError Relation)
ddlAsRelation sessionId (InProcessConnection conf) = do
  atomically $ do
    graph <- readTVar (ipTransactionGraph conf)
    eSession <- sessionAndSchema sessionId conf
    case eSession of
      Left err -> pure (Left err)
      Right (session, schema) -> do
        let context = Sess.concreteDatabaseContext session
        pure (ddlType schema context graph)
ddlAsRelation sessionId conn@RemoteConnection{} = remoteCall conn (RetrieveDDLAsRelation sessionId)

-- | Returns the names and types of the atom functions in the current 'Session'.
atomFunctionsAsRelation :: SessionId -> Connection -> IO (Either RelationalError Relation)
atomFunctionsAsRelation sessionId (InProcessConnection conf) = do
  atomically $ do
    eSession <- sessionAndSchema sessionId conf
    case eSession of
      Left err -> pure (Left err)
      Right (session, _) -> do
        graph <- readTVar (ipTransactionGraph conf)
        let context = concreteDatabaseContext session
        case RE.resolveDBC' graph context DBC.atomFunctions of
          Left err -> pure (Left err)
          Right afuncs ->
            pure (AF.atomFunctionsAsRelation afuncs)
        
atomFunctionsAsRelation sessionId conn@(RemoteConnection _) = remoteCall conn (RetrieveAtomFunctionSummary sessionId)        

-- | Return a relation representing all database context functions. Requires ViewFunctionPermission and ViewDBCFunctionPermission per function.
databaseContextFunctionsAsRelation :: SessionId -> Connection -> IO (Either RelationalError Relation)
databaseContextFunctionsAsRelation sessionId (InProcessConnection conf) = do
  roleIds <- roleIdsForRoleName conf
  atomically $ do
    eSession <- sessionAndSchema sessionId conf
    case eSession of
      Left err -> pure (Left err)
      Right (session, _) -> do
        graph <- readTVar (ipTransactionGraph conf)
        let context = concreteDatabaseContext session
            reEnv = RE.mkRelationalExprEnv context graph
            requiredPerm = ViewFunctionPermission
        case RE.resolveDBC' graph (RE.re_context reEnv) DBC.acl of
          Left err -> pure (Left err)
          Right acl' -> do
            if hasAccess roleIds requiredPerm (dbcFunctionsACL acl') then
                case RE.resolveDBC' graph context DBC.dbcFunctions of
                  Left err -> pure (Left err)
                  Right dbcFuncs -> do
                    -- filter out dbcFuncs to which this user does not have ViewDBCFunctionPermission
                    let dbcFuncsFiltered = HS.filter dbcFuncFilter dbcFuncs
                        dbcFuncFilter f = hasAccess roleIds ViewDBCFunctionPermission (funcACL f)
                    pure (DCF.databaseContextFunctionsAsRelation dbcFuncsFiltered)
            else
               pure (Left (AccessDeniedError (SomeFunctionPermission requiredPerm)))

databaseContextFunctionsAsRelation sessionId conn@(RemoteConnection _) = remoteCall conn (RetrieveDatabaseContextFunctionSummary sessionId)

-- | Show notifications in a relation format. Requires RelVarsAccessPermission.
notificationsAsRelation :: SessionId -> Connection -> IO (Either RelationalError Relation)
notificationsAsRelation sessionId (InProcessConnection conf) = do
  roleIds <- roleIdsForRoleName conf    
  atomically $ do
    eSession <- sessionAndSchema sessionId conf
    case eSession of
      Left err -> pure (Left err)
      Right (session, schema) -> do
        graph <- readTVar (ipTransactionGraph conf)
        let context = concreteDatabaseContext session
        case RE.resolveDBC' graph context DBC.notifications of
          Left err -> pure (Left err)
          Right notifs ->
            case relvarsACL <$> RE.resolveDBC' graph context DBC.acl of
              Left err -> pure (Left err)
              Right rvacl' ->
                if hasAccess roleIds AccessRelVarsPermission rvacl' then
                  pure (Schema.notificationsAsRelationInSchema notifs schema)
                else
                  pure (Left (AccessDeniedError (SomeRelVarPermission AccessRelVarsPermission)))                  

notificationsAsRelation sessionId conn@RemoteConnection{} = remoteCall conn (RetrieveNotificationsAsRelation sessionId)

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
    
currentHeadSTM_ :: SessionId -> Sessions -> STM (Either RelationalError Discon.CurrentHead)  
currentHeadSTM_ sessionId sessions = do
    eSession <- sessionForSessionId sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right session ->
        pure (Right (Discon.disconCurrentHead (Sess.disconnectedTransaction session)))
  
-- | Returns Just the current head of the current disconnected transaction which may be a branch name or a transaction id.
currentHead :: SessionId -> Connection -> IO (Either RelationalError Discon.CurrentHead)
currentHead sessionId (InProcessConnection conf) = do
  let sessions = ipSessions conf
  atomically (currentHeadSTM_ sessionId sessions)
currentHead sessionId conn@(RemoteConnection _) = remoteCall conn (ExecuteHeadName sessionId)

-- | Returns a listing of all available atom types. Requires no specific permission.
atomTypesAsRelation :: SessionId -> Connection -> IO (Either RelationalError Relation)
atomTypesAsRelation sessionId (InProcessConnection conf) = do
  let sessions = ipSessions conf
  atomically $ do
    eSession <- sessionForSessionId sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right session -> do
        graph <- readTVar (ipTransactionGraph conf)
        let context = concreteDatabaseContext session
        case RE.resolveDBC' graph context DBC.typeConstructorMapping of
          Left err -> pure (Left err)
          Right tConsMap ->
            case typesAsRelation tConsMap of
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
        pure (Right (Sess.isUpdated session))
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
-- the wrapped function returns multiple disconnected transaction and transaction ids because we anticipate coalescing multiple in-flight commits into one big fsync
commitLock_ :: SessionId -> 
               InProcessConnectionConf -> 
               (TransactionGraph -> 
                STM (Either RelationalError (DisconnectedTransaction, TransactionGraphIncrementalWriteInfo))) -> 
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
            eTransWriteInfo <- stmBlock refreshedGraph
            case eTransWriteInfo of
              Left err -> pure (Left err)
              Right (discon, tWriteInfo) -> do
                writeTVar graphTvar (newGraph tWriteInfo)
                let newSession = Session discon (Sess.schemaName session)
                    dbctx = Discon.concreteDatabaseContext discon
                case RE.resolveDBC' (newGraph tWriteInfo) dbctx DBC.acl of
                  Left err -> pure (Left err)
                  Right acl' -> do
                    let rvacl = relvarsACL acl'
                    StmMap.insert newSession sessionId sessions
                    case RE.transactionForId (Sess.parentId session) oldGraph of
                      Left err -> pure $ Left err
                      Right previousTrans ->
                        if not (Prelude.null (uncommittedTransactions tWriteInfo)) then do
                          (evaldNots, nodes) <- executeCommitExprSTM_ (newGraph tWriteInfo) (Trans.concreteDatabaseContext previousTrans) (Sess.concreteDatabaseContext session) clientNodes
                          clientNodeMap <- stmMapToList nodes
                          let nodesToNotify = map snd clientNodeMap
                          pure $ Right (evaldNots, nodesToNotify, tWriteInfo, rvacl)
                        else pure (Right (M.empty, [], tWriteInfo, rvacl))

      --handle notification firing                
  case manip of 
    Left err -> pure (Left err)
    Right (notsToFire, nodesToNotify, tWriteInfo, acl') -> do
      --update filesystem database, if necessary
      processTransactionGraphPersistence strat tWriteInfo
      sendNotifications conf acl' nodesToNotify notsToFire
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

-- | Execute an expression which returns a dataframe. Requires permissions to run the underlying relational expression.
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

-- | Run a check on the transaction graph to validate all merkle hashes. Requires no specific permission.
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

-- | Calculate a hash on the DDL of the current database context (not the graph). This is useful for validating on the client that the database schema meets the client's expectation. Any DDL change will change this hash. This hash does not change based on the current isomorphic schema being examined. This function is not affected by the current schema (since they are all isomorphic anyway, they should return the same hash). Requires AccessRelVarsPermission.
getDDLHash :: SessionId -> Connection -> IO (Either RelationalError SecureHash)
getDDLHash sessionId (InProcessConnection conf) = do
  roleIds <- roleIdsForRoleName conf      
  let sessions = ipSessions conf
  atomically $ do
    eSession <- sessionForSessionId sessionId sessions
    case eSession of
      Left err -> pure (Left err)
      Right session -> do
        let ctx = Sess.concreteDatabaseContext session
        graph <- readTVar (ipTransactionGraph conf)
        case RE.resolveDBC' graph ctx DBC.acl of
          Left err -> pure (Left err)
          Right acl' -> 
            if hasAccess roleIds AccessRelVarsPermission (relvarsACL acl') then
              pure (ddlHash ctx graph)
            else
               pure (Left (AccessDeniedError (SomeRelVarPermission AccessRelVarsPermission)))              
getDDLHash sessionId conn@RemoteConnection{} = remoteCall conn (GetDDLHash sessionId)

-- | Convert a SQL Query expression into a DataFrameExpr. Because the conversion process requires substantial database metadata access (such as retrieving types for various subexpressions), we cannot process SQL client-side. However, the underlying DBMS is completely unaware that the resultant DataFrameExpr has come from SQL.
convertSQLQuery :: SessionId -> Connection -> Query -> IO (Either RelationalError DF.DataFrameExpr)
convertSQLQuery sessionId (InProcessConnection conf) query = do
  let graphTvar = ipTransactionGraph conf  
  atomically $ do
    transGraph <- readTVar graphTvar    
    eSession <- sessionAndSchema sessionId conf
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
  let graphTvar = ipTransactionGraph conf  
  atomically $ do
    transGraph <- readTVar graphTvar    
    eSession <- sessionAndSchema sessionId conf
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

-- | Registered queries are queries must always typecheck. Return a relation of them. Requires AccessRelVarsPermission.
registeredQueriesAsRelation :: SessionId -> Connection -> IO (Either RelationalError Relation)
registeredQueriesAsRelation sessionId (InProcessConnection conf) = do
  roleIds <- roleIdsForRoleName conf  
  atomically $ do
    eSession <- sessionAndSchema sessionId conf
    case eSession of
      Left err -> pure (Left err)
      Right (session, schema) -> do
        graph <- readTVar (ipTransactionGraph conf)        
        let context = Sess.concreteDatabaseContext session
        case RE.resolveDBC' graph context DBC.acl of
          Left err -> pure (Left err)
          Right acl' -> 
            if hasAccess roleIds AccessRelVarsPermission (relvarsACL acl') then
              case RE.resolveDBC' graph context DBC.registeredQueries of
                Left err -> pure (Left err)
                Right regQs ->
                  pure $ registeredQueriesAsRelationInSchema schema regQs
            else
               pure (Left (AccessDeniedError (SomeRelVarPermission AccessRelVarsPermission)))                            
registeredQueriesAsRelation sessionId conn@RemoteConnection{} = remoteCall conn (RetrieveRegisteredQueries sessionId)        

type ClientNodes = StmMap.Map SRPC.ClientConnectionId ClientInfo

-- internal structure specific to in-process connections
data InProcessConnectionConf = InProcessConnectionConf {
  ipPersistenceStrategy :: PersistenceStrategy, 
  ipClientNodes :: ClientNodes, 
  ipSessions :: Sessions,
  ipTransactionGraph :: TVar TransactionGraph,
  ipScriptSession :: Maybe ScriptSession,
  ipLocks :: Maybe (LockFile, MVar LockFileHash), -- nothing when NoPersistence
  ipCallbackAsync :: Async (),
  ipRelExprCache :: RelExprCache, -- can the remote client also include such a pinned expr cache? should that cache be controlled by the server or client?
  ipLoginRoles :: LoginRoles.LoginRolesDB, -- ^ roles allowed to connect to the database, access control is otherwise handled by ACLs in the database context
  ipRoleName :: RoleName, -- ^ role name for the user accessing the database
  ipRandomGen :: StdGen -- ^ random number generator
  }

-- clients may connect associate one socket/mvar with the server to register for change callbacks
data ClientInfo = RemoteClientInfo SRPC.SocketContext RoleName |
                  InProcessClientInfo (MVar EvaluatedNotifications)

instance Eq ClientInfo where
  (RemoteClientInfo a roleNameA) == (RemoteClientInfo b roleNameB) = a == b && roleNameA == roleNameB
  (InProcessClientInfo a) == (InProcessClientInfo b) = a == b
  _ == _ = False

instance Hashable ClientInfo where
  hashWithSalt salt (RemoteClientInfo sockCtx roleName) = salt `hashWithSalt` show (SRPC.lockless (SRPC.lockingSocket sockCtx)) `hashWithSalt` roleName
  hashWithSalt salt (InProcessClientInfo _) = hashWithSalt salt (1::Int)

data TlsConfig = TlsConfig
  {
    serverTlsHostName :: String,
    serverTlsServiceName :: String,
    serverX509PublicPrivateKeyPaths :: Maybe (FilePath, FilePath),
    serverX509CertificatePath :: Maybe FilePath -- ^ Using Nothing indicates referencing the system certificate store.
  }
  deriving Show

roleIdsForRoleName :: InProcessConnectionConf -> IO [RoleId]
roleIdsForRoleName conf = do
  eRoles <- LoginRoles.roleIdsForRoleName (ipRoleName conf) (ipLoginRoles conf)
  --ignore errors and expect ACL functions to handle empty role id list
  case eRoles of
    Left _err -> pure []
    Right roleIds -> pure roleIds

primaryRoleIdForRoleName :: InProcessConnectionConf -> IO RoleId
primaryRoleIdForRoleName conf = do
  eRoleId <- LoginRoles.roleIdForRoleName (ipRoleName conf) (ipLoginRoles conf)
  case eRoleId of
    Left _err -> error ("role id for role name \"" <> T.unpack (ipRoleName conf) <> "\" no longer exists")
    Right roleId -> pure roleId

-- | Useful for in-process connections to change the role related to access control. Remote connections authenticate via TLS, so require new connections to change roles.
setRoleName :: RoleName -> Connection -> Connection
setRoleName newRoleName (InProcessConnection conf) =
  InProcessConnection (conf { ipRoleName = newRoleName })
setRoleName _ RemoteConnection{} = error "setRoleName can only be used with in-process connections"
  

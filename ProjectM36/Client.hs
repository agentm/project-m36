{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.Client
       (ConnectionInfo(..),
       Connection(..),
       Port,
       Hostname,
       DatabaseName,
       ConnectionError,
       connectProjectM36,
       close,
       executeRelationalExpr,
       executeDatabaseContextExpr,
       executeGraphExpr,
       commit,
       rollback,
       typeForRelationalExpr,
       inclusionDependencies,
       planForDatabaseContextExpr,
       processPersistence,
       transactionGraphAsRelation,
       headName,
       remoteDBLookupName,
       defaultServerPort,
       headTransactionUUID,
       defaultDatabaseName,
       defaultRemoteConnectionInfo,
       defaultHeadName,
       PersistenceStrategy(..),
       RelationalExpr(..),
       DatabaseExpr(..),
       Attribute(..),
       attributesFromList,
       createNodeId,
       createSessionAtCommit,
       createSessionAtHead,
       closeSession,
       TransactionGraphOperator(..),
       Atomable,
       NodeId(..),
       Atom(..),
       Session,
       SessionId,
       AtomType(..)) where
import ProjectM36.Base hiding (inclusionDependencies) --defined in this module as well
import qualified ProjectM36.Base as B
import ProjectM36.Error
import ProjectM36.StaticOptimizer
import Control.Monad.State
import qualified ProjectM36.RelationalExpression as RE
import ProjectM36.TransactionGraph
import ProjectM36.TransactionGraph.Persist
import ProjectM36.Attribute
import ProjectM36.Persist (DiskSync(..))
import ProjectM36.Server.RemoteCallTypes 
import Network.Transport.TCP (createTransport, defaultTCPParameters, encodeEndPointAddress)
import Control.Distributed.Process.Node (newLocalNode, initRemoteTable, runProcess, LocalNode)
import Control.Distributed.Process.Extras.Internal.Types (whereisRemote)
import Control.Distributed.Process.ManagedProcess.Client (call, safeCall)
import Control.Distributed.Process (NodeId(..))

import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Control.Concurrent.STM
import Data.Word
import Control.Distributed.Process (ProcessId, Process)
import Control.Exception (IOException)
import Control.Concurrent.MVar
import qualified Data.Map as M
import Control.Distributed.Process.Serializable (Serializable)
--import Control.Distributed.Process.Debug
import qualified STMContainers.Map as STMMap
import ProjectM36.Session
import ProjectM36.Sessions
import ListT

type Hostname = String

type Port = Word16

type DatabaseName = String

data ConnectionInfo = InProcessConnectionInfo PersistenceStrategy |
                      RemoteProcessConnectionInfo DatabaseName NodeId
                      
createNodeId :: Hostname -> Port -> NodeId                      
createNodeId host port = NodeId $ encodeEndPointAddress host (show port) 0
                      
defaultServerPort :: Port
defaultServerPort = 6543

defaultDatabaseName :: DatabaseName
defaultDatabaseName = "base"

defaultHeadName :: HeadName
defaultHeadName = "master"

defaultRemoteConnectionInfo :: ConnectionInfo
defaultRemoteConnectionInfo = RemoteProcessConnectionInfo defaultDatabaseName (createNodeId "127.0.0.1" defaultServerPort)

-- | The 'Connection' represents either local or remote access to a database. All operations flow through the connection.
data Connection = InProcessConnection PersistenceStrategy Sessions (TVar TransactionGraph) |
                  RemoteProcessConnection LocalNode ProcessId
                  
-- | There are several reasons why a connection can fail.
data ConnectionError = SetupDatabaseDirectoryError PersistenceError |
                       IOExceptionError IOException |
                       NoSuchDatabaseByNameError DatabaseName |
                       LoginError 
                       deriving (Show, Eq)
                  
remoteDBLookupName :: DatabaseName -> String    
remoteDBLookupName = (++) "db-" 


commonLocalNode :: IO (Either ConnectionError LocalNode)
commonLocalNode = do
  eLocalTransport <- createTransport "127.0.0.1" "0" defaultTCPParameters
  case eLocalTransport of
    Left err -> pure (Left $ IOExceptionError err)
    Right localTransport -> newLocalNode localTransport initRemoteTable >>= pure . Right
  
-- | To create a 'Connection' to a remote or local database, create a connectionInfo and call 'connectProjectM36'.
connectProjectM36 :: ConnectionInfo -> IO (Either ConnectionError Connection)
--create a new in-memory database/transaction graph
connectProjectM36 (InProcessConnectionInfo strat) = do
  freshUUID <- nextRandom
  let bootstrapContext = RE.basicDatabaseContext 
      freshGraph = bootstrapTransactionGraph freshUUID bootstrapContext
  case strat of
    --create date examples graph for now- probably should be empty context in the future
    NoPersistence -> do
        graphTvar <- newTVarIO freshGraph
        sessions <- STMMap.newIO
        return $ Right $ InProcessConnection strat sessions graphTvar
    MinimalPersistence dbdir -> connectPersistentProjectM36 strat NoDiskSync dbdir freshGraph
    CrashSafePersistence dbdir -> connectPersistentProjectM36 strat FsyncDiskSync dbdir freshGraph
        
connectProjectM36 (RemoteProcessConnectionInfo databaseName serverNodeId) = do
  connStatus <- newEmptyMVar
  eLocalNode <- commonLocalNode
  let dbName = remoteDBLookupName databaseName
  putStrLn $ show serverNodeId ++ " " ++ dbName
  case eLocalNode of
    Left err -> pure (Left err)
    Right localNode -> do
      runProcess localNode $ do
        mServerProcessId <- whereisRemote serverNodeId dbName
        case mServerProcessId of
          Nothing -> liftIO $ putMVar connStatus $ Left (NoSuchDatabaseByNameError databaseName)
          Just serverProcessId -> do
            loginConfirmation <- call serverProcessId Login
            if not loginConfirmation then
              liftIO $ putMVar connStatus (Left LoginError)
              else do
                liftIO $ putMVar connStatus (Right $ RemoteProcessConnection localNode serverProcessId)
      status <- takeMVar connStatus
      pure status

connectPersistentProjectM36 :: PersistenceStrategy ->
                               DiskSync ->
                               FilePath -> 
                               TransactionGraph -> 
                               IO (Either ConnectionError Connection)      
connectPersistentProjectM36 strat sync dbdir freshGraph = do
  err <- setupDatabaseDir sync dbdir freshGraph 
  case err of
    Just err' -> return $ Left (SetupDatabaseDirectoryError err')
    Nothing -> do 
      graph <- transactionGraphLoad dbdir emptyTransactionGraph
      case graph of
        Left err' -> return $ Left (SetupDatabaseDirectoryError err')
        Right graph' -> do
          tvarGraph <- newTVarIO graph'
          sessions <- STMMap.newIO
          return $ Right $ InProcessConnection strat sessions tvarGraph

-- | Create a new session at the transaction UUID and return the session's Id.
createSessionAtCommit :: UUID -> Connection -> IO (Either RelationalError SessionId)
createSessionAtCommit commitUUID conn@(InProcessConnection _ _ _) = do
   newSessionId <- nextRandom
   atomically $ do
      createSessionAtCommit_ commitUUID newSessionId conn
createSessionAtCommit uuid conn@(RemoteProcessConnection _ _) = remoteCall conn (CreateSessionAtCommit uuid)

createSessionAtCommit_ :: UUID -> SessionId -> Connection -> STM (Either RelationalError SessionId)
createSessionAtCommit_ commitUUID newSessionId (InProcessConnection _ sessions graphTvar) = do
    graph <- readTVar graphTvar
    case transactionForUUID commitUUID graph of
        Left err -> pure (Left err)
        Right transaction -> do
            let freshDiscon = DisconnectedTransaction commitUUID (transactionContext transaction)
            keyDuplication <- STMMap.lookup newSessionId sessions
            case keyDuplication of
                Just _ -> pure $ Left (SessionIdInUse newSessionId)
                Nothing -> do
                   STMMap.insert (Session freshDiscon) newSessionId sessions
                   pure $ Right newSessionId
createSessionAtCommit_ _ _ (RemoteProcessConnection _ _) = error "createSessionAtCommit_ called on remote connection"
  
-- | Call 'createSessionAtHead' with a transaction graph's head's name to create a new session pinned to that head. This function returns a 'SessionId' which can be used in other function calls to reference the point in the transaction graph.
createSessionAtHead :: HeadName -> Connection -> IO (Either RelationalError SessionId)
createSessionAtHead headn conn@(InProcessConnection _ _ tvar) = do
    newSessionId <- nextRandom
    atomically $ do
        graph <- readTVar tvar
        case transactionForHead headn graph of
            Nothing -> pure $ Left (NoSuchHeadNameError headn)
            Just trans -> createSessionAtCommit_ (transactionUUID trans) newSessionId conn
createSessionAtHead headn conn@(RemoteProcessConnection _ _) = remoteCall conn (CreateSessionAtHead headn)            

-- | Discards a session, eliminating any uncommitted changes present in the session.
closeSession :: SessionId -> Connection -> IO ()
closeSession sessionId (InProcessConnection _ sessions _) = do
    atomically $ do
       STMMap.delete sessionId sessions
closeSession sessionId conn@(RemoteProcessConnection _ _) = remoteCall conn (CloseSession sessionId)       
-- | 'close' cleans up the database access connection. Note that sessions persist even after the connection is closed.              
close :: Connection -> IO ()
close (InProcessConnection _ sessions _) = atomically $ do
    traverse_ (\(k,_) -> STMMap.delete k sessions) (STMMap.stream sessions)
    pure ()
close (RemoteProcessConnection localNode serverProcessId) = do
  runProcessResult localNode $ do
    call serverProcessId Logout
      
runProcessResult :: LocalNode -> Process a -> IO a      
runProcessResult localNode proc = do
  ret <- newEmptyMVar
  runProcess localNode $ do
    val <- proc
    liftIO $ putMVar ret val
  takeMVar ret

remoteCall :: (Serializable a, Serializable b) => Connection -> a -> IO b
remoteCall (InProcessConnection _ _ _) _ = error "remoteCall called on local connection"
remoteCall (RemoteProcessConnection localNode serverProcessId) arg = runProcessResult localNode $ do
  ret <- safeCall serverProcessId arg
  case ret of
    Left err -> error (show err)
    Right ret' -> pure ret'

sessionForSessionId :: SessionId -> Sessions -> STM (Either RelationalError Session)
sessionForSessionId sessionId sessions = do
  maybeSession <- STMMap.lookup sessionId sessions
  pure $ maybe (Left $ NoSuchSession sessionId) Right maybeSession

-- | Execute a relational expression in the context of the session and connection. Relational expressions are queries and therefore cannot alter the database.
executeRelationalExpr :: SessionId -> Connection -> RelationalExpr -> IO (Either RelationalError Relation)
executeRelationalExpr sessionId (InProcessConnection _ sessions _) expr = atomically $ do
  eSession <- sessionForSessionId sessionId sessions
  case eSession of
    Left err -> pure $ Left err
    Right (Session (DisconnectedTransaction _ context)) -> pure $ evalState (RE.evalRelationalExpr expr) context
executeRelationalExpr sessionId conn@(RemoteProcessConnection _ _) relExpr = remoteCall conn (ExecuteRelationalExpr sessionId relExpr)
  
-- | Execute a database context expression in the context of the session and connection. Database expressions modify the current session's disconnected transaction but cannot modify the transaction graph.
executeDatabaseContextExpr :: SessionId -> Connection -> DatabaseExpr -> IO (Maybe RelationalError)
executeDatabaseContextExpr sessionId (InProcessConnection _ sessions _) expr = atomically $ do
  eSession <- sessionForSessionId sessionId sessions
  case eSession of
    Left err -> pure $ Just err
    Right session -> case runState (RE.evalContextExpr expr) (sessionContext session) of
      (Just err,_) -> return $ Just err
      (Nothing, context') -> do
         let newDiscon = DisconnectedTransaction (sessionParentUUID session) context'
             newSession = Session newDiscon
         STMMap.insert newSession sessionId sessions
         return Nothing
executeDatabaseContextExpr sessionId conn@(RemoteProcessConnection _ _) dbExpr = remoteCall conn (ExecuteDatabaseContextExpr sessionId dbExpr)
         
-- | Execute a transaction graph expression in the context of the session and connection. Transaction graph operators modify the transaction graph state.
executeGraphExpr :: SessionId -> Connection -> TransactionGraphOperator -> IO (Maybe RelationalError)
executeGraphExpr sessionId (InProcessConnection strat sessions graphTvar) graphExpr = do
  freshUUID <- nextRandom
  manip <- atomically $ do
    graph <- readTVar graphTvar
    eSession <- sessionForSessionId sessionId sessions
    case eSession of
      Left err -> pure $ Left err
      Right (Session discon) -> case evalGraphOp freshUUID discon graph graphExpr of
        Left err -> pure $ Left err
        Right (discon', graph') -> do
          writeTVar graphTvar graph'
          let newSession = Session discon'
          STMMap.insert newSession sessionId sessions
          pure $ Right graph'
  case manip of 
    Left err -> return $ Just err
    Right newGraph -> do
      --update filesystem database, if necessary
      --this should really grab a lock at the beginning of the method to be threadsafe
      processPersistence strat newGraph
      return Nothing
executeGraphExpr sessionId conn@(RemoteProcessConnection _ _) graphExpr = remoteCall conn (ExecuteGraphExpr sessionId graphExpr)
      
-- | After modifying a session, 'commit' the transaction to the transaction graph at the head which the session is referencing.
commit :: SessionId -> Connection -> IO (Maybe RelationalError)
commit sessionId conn@(InProcessConnection _ _ _) = executeGraphExpr sessionId conn Commit
commit sessionId conn@(RemoteProcessConnection _ _) = remoteCall conn (ExecuteGraphExpr sessionId Commit)
          
-- | Discard any changes made in the current session. This resets the disconnected transaction to reference the original database context of the parent transaction and is a very cheap operation.
rollback :: SessionId -> Connection -> IO (Maybe RelationalError)
rollback sessionId conn@(InProcessConnection _ _ _) = executeGraphExpr sessionId conn Rollback      
rollback sessionId conn@(RemoteProcessConnection _ _) = remoteCall conn (ExecuteGraphExpr sessionId Rollback)

processPersistence :: PersistenceStrategy -> TransactionGraph -> IO ()
processPersistence NoPersistence _ = return ()
processPersistence (MinimalPersistence dbdir) graph = transactionGraphPersist NoDiskSync dbdir graph
processPersistence (CrashSafePersistence dbdir) graph = transactionGraphPersist FsyncDiskSync dbdir graph

-- | Return a relation whose type would match that of the relational expression if it were executed. This is useful for checking types and validating a relational expression's types.
typeForRelationalExpr :: SessionId -> Connection -> RelationalExpr -> IO (Either RelationalError Relation)
typeForRelationalExpr sessionId conn@(InProcessConnection _ _ _) relExpr = atomically $ typeForRelationalExprSTM sessionId conn relExpr
typeForRelationalExpr sessionId conn@(RemoteProcessConnection _ _) relExpr = remoteCall conn (ExecuteTypeForRelationalExpr sessionId relExpr)
    
typeForRelationalExprSTM :: SessionId -> Connection -> RelationalExpr -> STM (Either RelationalError Relation)    
typeForRelationalExprSTM sessionId (InProcessConnection _ sessions _) relExpr = do
  eSession <- sessionForSessionId sessionId sessions
  case eSession of
    Left err -> pure $ Left err
    Right session -> pure $ evalState (RE.typeForRelationalExpr relExpr) (sessionContext session)
    
typeForRelationalExprSTM _ _ _ = error "typeForRelationalExprSTM called on non-local connection"

-- | Return a 'Map' of the database's constraints at the context of the session and connection.
inclusionDependencies :: SessionId -> Connection -> IO (Either RelationalError (M.Map IncDepName InclusionDependency))
inclusionDependencies sessionId (InProcessConnection _ sessions _) = do
  atomically $ do
    eSession <- sessionForSessionId sessionId sessions
    case eSession of
      Left err -> pure $ Left err
      Right session -> pure $ Right (B.inclusionDependencies (sessionContext session))

inclusionDependencies sessionId conn@(RemoteProcessConnection _ _) = remoteCall conn (RetrieveInclusionDependencies sessionId)

-- | Return an optimized database expression which is logically equivalent to the input database expression. This function can be used to determine which expression will actually be evaluated.
planForDatabaseContextExpr :: SessionId -> Connection -> DatabaseExpr -> IO (Either RelationalError DatabaseExpr)  
planForDatabaseContextExpr sessionId (InProcessConnection _ sessions _) dbExpr = do
  atomically $ do
    eSession <- sessionForSessionId sessionId sessions
    case eSession of
      Left err -> pure $ Left err
      Right session -> pure $ evalState (applyStaticDatabaseOptimization dbExpr) (sessionContext session)
planForDatabaseContextExpr sessionId conn@(RemoteProcessConnection _ _) dbExpr = remoteCall conn (RetrievePlanForDatabaseContextExpr sessionId dbExpr)
             
-- | Return a relation which represents the current state of the global transaction graph. The attributes are 
-- * current- boolean attribute representing whether or not the current session references this transaction
-- * head- text attribute which is a non-empty 'HeadName' iff the transaction references a head.
-- * id- UUID attribute of the transaction
-- * parents- a relation-valued attribute which contains a relation of UUIDs which are parent transaction to the transaction
transactionGraphAsRelation :: SessionId -> Connection -> IO (Either RelationalError Relation)
transactionGraphAsRelation sessionId (InProcessConnection _ sessions tvar) = do
  atomically $ do
    eSession <- sessionForSessionId sessionId sessions
    case eSession of
      Left err -> pure $ Left err
      Right (Session discon) -> do
        graph <- readTVar tvar
        pure $ graphAsRelation discon graph
    
transactionGraphAsRelation sessionId conn@(RemoteProcessConnection _ _) = remoteCall conn (RetrieveTransactionGraph sessionId) 

-- | Returns the UUID for the connection's disconnected transaction committed parent transaction.  
headTransactionUUID :: SessionId -> Connection -> IO (Maybe UUID)
headTransactionUUID sessionId (InProcessConnection _ sessions _) = do
  atomically $ do
    eSession <- sessionForSessionId sessionId sessions
    case eSession of
      Left _ -> pure Nothing
      Right session -> pure $ Just (sessionParentUUID session)
headTransactionUUID sessionId conn@(RemoteProcessConnection _ _) = remoteCall conn (RetrieveHeadTransactionUUID sessionId)
    
-- | Returns Just the name of the head of the current disconnected transaction or Nothing.    
headName :: SessionId -> Connection -> IO (Maybe HeadName)
headName sessionId (InProcessConnection _ sessions graphTvar) = do
  atomically $ do
    graph <- readTVar graphTvar
    eSession <- sessionForSessionId sessionId sessions
    case eSession of
      Left _ -> pure $ Nothing
      Right session -> pure $ case transactionForUUID (sessionParentUUID session) graph of
        Left _ -> Nothing
        Right parentTrans -> headNameForTransaction parentTrans graph
headName sessionId conn@(RemoteProcessConnection _ _) = remoteCall conn (ExecuteHeadName sessionId)

    
                                                        
  
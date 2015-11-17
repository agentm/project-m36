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
       PersistenceStrategy(..),
       RelationalExpr(..),
       DatabaseExpr(..),
       Attribute(..),
       attributesFromList,
       createNodeId,
       TransactionGraphOperator(..),
       Atomable,
       NodeId(..),
       Atom(..),
       AtomType(..)) where
import ProjectM36.Base hiding (inclusionDependencies) --defined in this module as well
import qualified ProjectM36.Base as B
import ProjectM36.Error
import ProjectM36.StaticOptimizer
import Control.Monad.State
import ProjectM36.Transaction
import qualified ProjectM36.RelationalExpression as RE
import ProjectM36.TransactionGraph
import ProjectM36.TransactionGraph.Persist
import ProjectM36.Attribute
import ProjectM36.Persist (DiskSync(..))
import ProjectM36.Daemon.RemoteCallTypes 
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

defaultRemoteConnectionInfo :: ConnectionInfo
defaultRemoteConnectionInfo = RemoteProcessConnectionInfo defaultDatabaseName (createNodeId "127.0.0.1" defaultServerPort)
                      
data Connection = InProcessConnection PersistenceStrategy (TVar (DisconnectedTransaction, TransactionGraph)) |
                  RemoteProcessConnection LocalNode ProcessId -- perhaps should reference the local node as well
                  
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
  
-- connect to an in-process database
connectProjectM36 :: ConnectionInfo -> IO (Either ConnectionError Connection)
--create a new in-memory database/transaction graph
connectProjectM36 (InProcessConnectionInfo strat) = do
  freshUUID <- nextRandom
  let bootstrapContext = RE.basicDatabaseContext
      freshDiscon = newDisconnectedTransaction freshUUID bootstrapContext
      freshGraph = bootstrapTransactionGraph freshUUID bootstrapContext
  case strat of
    --create date examples graph for now- probably should be empty context in the future
    NoPersistence -> do
        tvar <- newTVarIO (freshDiscon, freshGraph)
        return $ Right $ InProcessConnection strat tvar
    MinimalPersistence dbdir -> connectPersistentProjectM36 strat NoDiskSync dbdir freshDiscon freshGraph
    CrashSafePersistence dbdir -> connectPersistentProjectM36 strat FsyncDiskSync dbdir freshDiscon freshGraph
        
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
                               DisconnectedTransaction -> 
                               TransactionGraph -> 
                               IO (Either ConnectionError Connection)      
connectPersistentProjectM36 strat sync dbdir freshDiscon freshGraph = do
  err <- setupDatabaseDir sync dbdir freshGraph 
  case err of
    Just err' -> return $ Left (SetupDatabaseDirectoryError err')
    Nothing -> do 
      graph <- transactionGraphLoad dbdir emptyTransactionGraph
      case graph of
        Left err' -> return $ Left (SetupDatabaseDirectoryError err')
        Right graph' -> do
          tvar <- newTVarIO (freshDiscon, graph')
          return $ Right $ InProcessConnection strat tvar              
              
close :: Connection -> IO ()
close (InProcessConnection _ _) = pure ()
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
remoteCall (InProcessConnection _ _) _ = error "remoteCall called on local connection"
remoteCall (RemoteProcessConnection localNode serverProcessId) arg = runProcessResult localNode $ do
  ret <- safeCall serverProcessId arg
  case ret of
    Left err -> error (show err)
    Right ret' -> pure ret'

executeRelationalExpr :: Connection -> RelationalExpr -> IO (Either RelationalError Relation)
executeRelationalExpr (InProcessConnection _ tvar) expr = atomically $ do
  ((DisconnectedTransaction _ context), _) <- readTVar tvar
  return $ evalState (RE.evalRelationalExpr expr) context
executeRelationalExpr conn@(RemoteProcessConnection _ _) relExpr = remoteCall conn (ExecuteRelationalExpr relExpr)
  
executeDatabaseContextExpr :: Connection -> DatabaseExpr -> IO (Maybe RelationalError)
executeDatabaseContextExpr (InProcessConnection _ tvar) expr = atomically $ do
  ((DisconnectedTransaction parentUUID context), graph) <- readTVar tvar
  case runState (RE.evalContextExpr expr) context of
       (Just err,_) -> return $ Just err
       (Nothing, context') -> do
         let newDiscon = DisconnectedTransaction parentUUID context'
         writeTVar tvar (newDiscon, graph)
         return Nothing
executeDatabaseContextExpr conn@(RemoteProcessConnection _ _) dbExpr = remoteCall conn (ExecuteDatabaseContextExpr dbExpr)
         
executeGraphExpr :: Connection -> TransactionGraphOperator -> IO (Maybe RelationalError)
executeGraphExpr (InProcessConnection strat tvar) graphExpr = do
  freshUUID <- nextRandom
  manip <- atomically $ do
    (discon, graph) <- readTVar tvar
    case evalGraphOp freshUUID discon graph graphExpr of
      Left err -> return $ Left err
      Right (discon', graph') -> do
        writeTVar tvar (discon', graph')
        return $ Right (discon', graph')
  case manip of 
    Left err -> return $ Just err
    Right (_, newGraph) -> do
      --update filesystem database, if necessary
      --this should really grab a lock at the beginning of the method to be threadsafe
      processPersistence strat newGraph
      return Nothing
executeGraphExpr conn@(RemoteProcessConnection _ _) graphExpr = remoteCall conn (ExecuteGraphExpr graphExpr)
      
commit :: Connection -> IO (Maybe RelationalError)
commit conn@(InProcessConnection _ _) = executeGraphExpr conn Commit
commit conn@(RemoteProcessConnection _ _) = remoteCall conn (ExecuteGraphExpr Commit)
          
rollback :: Connection -> IO (Maybe RelationalError)
rollback conn@(InProcessConnection _ _) = executeGraphExpr conn Rollback      
rollback conn@(RemoteProcessConnection _ _) = remoteCall conn (ExecuteGraphExpr Rollback)

processPersistence :: PersistenceStrategy -> TransactionGraph -> IO ()
processPersistence NoPersistence _ = return ()
processPersistence (MinimalPersistence dbdir) graph = transactionGraphPersist NoDiskSync dbdir graph
processPersistence (CrashSafePersistence dbdir) graph = transactionGraphPersist FsyncDiskSync dbdir graph

typeForRelationalExpr :: Connection -> RelationalExpr -> IO (Either RelationalError Relation)
typeForRelationalExpr conn@(InProcessConnection _ _) relExpr = atomically $ typeForRelationalExprSTM conn relExpr
typeForRelationalExpr conn@(RemoteProcessConnection _ _) relExpr = remoteCall conn (ExecuteTypeForRelationalExpr relExpr)
    
typeForRelationalExprSTM :: Connection -> RelationalExpr -> STM (Either RelationalError Relation)    
typeForRelationalExprSTM (InProcessConnection _ tvar) relExpr = do
  (DisconnectedTransaction _ context, _) <- readTVar tvar
  pure $ evalState (RE.typeForRelationalExpr relExpr) context
typeForRelationalExprSTM _ _ = error "typeForRelationalExprSTM called on non-local connection"

inclusionDependencies :: Connection -> IO (M.Map IncDepName InclusionDependency)
inclusionDependencies (InProcessConnection _ tvar) = do
  atomically $ do
    ((DisconnectedTransaction _ context), _) <- readTVar tvar
    pure (B.inclusionDependencies context)
inclusionDependencies conn@(RemoteProcessConnection _ _) = remoteCall conn RetrieveInclusionDependencies

  
planForDatabaseContextExpr :: Connection -> DatabaseExpr -> IO (Either RelationalError DatabaseExpr)  
planForDatabaseContextExpr (InProcessConnection _ tvar) dbExpr = do
  atomically $ do
    ((DisconnectedTransaction _ context), _) <- readTVar tvar
    pure $ evalState (applyStaticDatabaseOptimization dbExpr) context
planForDatabaseContextExpr conn@(RemoteProcessConnection _ _) dbExpr = remoteCall conn (RetrievePlanForDatabaseContextExpr dbExpr)
             
transactionGraphAsRelation :: Connection -> IO (Either RelationalError Relation)
transactionGraphAsRelation (InProcessConnection _ tvar) = do
  atomically $ do
    (discon, graph) <- readTVar tvar
    pure $ graphAsRelation discon graph
transactionGraphAsRelation conn@(RemoteProcessConnection _ _) = remoteCall conn RetrieveTransactionGraph  

-- | Returns the UUID for the connection's disconnected transaction committed parent transaction.  
headTransactionUUID :: Connection -> IO (UUID)
headTransactionUUID (InProcessConnection _ tvar) = do 
  atomically $ do
    (DisconnectedTransaction parentUUID _, _) <- readTVar tvar
    pure parentUUID
headTransactionUUID conn@(RemoteProcessConnection _ _) = remoteCall conn RetrieveHeadTransactionUUID
    
-- | Returns Just the name of the head of the current disconnected transaction or Nothing.    
headName :: Connection -> IO (Maybe HeadName)
headName (InProcessConnection _ tvar) = do
  atomically $ do
    (DisconnectedTransaction parentUUID _, graph) <- readTVar tvar
    pure $ case transactionForUUID parentUUID graph of
      Left _ -> Nothing
      Right parentTrans -> headNameForTransaction parentTrans graph
headName (RemoteProcessConnection localNode serverProcessId) = do
  runProcessResult localNode $ do
    ret <- call serverProcessId ExecuteHeadName
    pure ret
    
                                                        
  
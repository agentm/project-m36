module ProjectM36.Server.EntryPoints where
import ProjectM36.Base hiding (inclusionDependencies)
import ProjectM36.IsomorphicSchema
import ProjectM36.Client
import ProjectM36.Error
import Control.Distributed.Process (Process, ProcessId)
import Control.Distributed.Process.ManagedProcess (ProcessReply)
import Control.Distributed.Process.ManagedProcess.Server (reply)
import Control.Distributed.Process.Async (async, task, waitCancelTimeout, AsyncResult(..))
import Control.Distributed.Process.Extras.Time (microSeconds)
import Control.Distributed.Process.Serializable (Serializable)
import Control.Monad.IO.Class (liftIO)
import Data.Map
import Control.Concurrent (threadDelay)

timeoutOrDie :: Serializable a => Timeout -> IO a -> Process (Either ServerError a)
timeoutOrDie micros act = do
  if micros == 0 then
    liftIO act >>= \x -> pure (Right x)
    else do
    asyncUnit <- async (task (liftIO act))
    asyncRes <- waitCancelTimeout (microSeconds micros) asyncUnit
    case asyncRes of
      AsyncDone x -> pure (Right x)
      AsyncCancelled -> pure (Left RequestTimeoutError)
    
type Timeout = Int

type Reply a = Process (ProcessReply (Either ServerError a) Connection)
    
handleExecuteRelationalExpr :: Timeout -> SessionId -> Connection -> RelationalExpr -> Reply (Either RelationalError Relation)
handleExecuteRelationalExpr ti sessionId conn expr = do
  ret <- timeoutOrDie ti (executeRelationalExpr sessionId conn expr)
  reply ret conn
  
handleExecuteDatabaseContextExpr :: Timeout -> SessionId -> Connection -> DatabaseContextExpr -> Reply (Maybe RelationalError)
handleExecuteDatabaseContextExpr ti sessionId conn dbexpr = do
  ret <- timeoutOrDie ti (executeDatabaseContextExpr sessionId conn dbexpr)
  reply ret conn
  
handleExecuteDatabaseContextIOExpr :: Timeout -> SessionId -> Connection -> DatabaseContextIOExpr -> Reply (Maybe RelationalError)
handleExecuteDatabaseContextIOExpr ti sessionId conn dbexpr = do
  ret <- timeoutOrDie ti (executeDatabaseContextIOExpr sessionId conn dbexpr)
  reply ret conn
  
handleExecuteHeadName :: Timeout -> SessionId -> Connection -> Reply (Maybe HeadName)
handleExecuteHeadName ti sessionId conn = do
  ret <- timeoutOrDie ti (headName sessionId conn)
  reply ret conn
  
handleLogin :: Timeout -> Connection -> ProcessId -> Reply Bool
handleLogin ti conn newClientProcessId = do
  ret <- timeoutOrDie ti (addClientNode conn newClientProcessId)
  case ret of
    Right () -> reply (Right True) conn
    Left err -> reply (Left err) conn
  
handleExecuteGraphExpr :: Timeout -> SessionId -> Connection -> TransactionGraphOperator -> Reply (Maybe RelationalError)
handleExecuteGraphExpr ti sessionId conn graphExpr = do
  ret <- timeoutOrDie ti (executeGraphExpr sessionId conn graphExpr)
  reply ret conn
  
handleExecuteTransGraphRelationalExpr :: Timeout -> SessionId -> Connection -> TransGraphRelationalExpr -> Reply (Either RelationalError Relation)
handleExecuteTransGraphRelationalExpr ti sessionId conn graphExpr = do
  ret <- timeoutOrDie ti (executeTransGraphRelationalExpr sessionId conn graphExpr)
  reply ret conn

handleExecuteTypeForRelationalExpr :: Timeout -> SessionId -> Connection -> RelationalExpr -> Reply (Either RelationalError Relation)
handleExecuteTypeForRelationalExpr ti sessionId conn relExpr = do
  ret <- timeoutOrDie ti (typeForRelationalExpr sessionId conn relExpr)
  reply ret conn
  
handleRetrieveInclusionDependencies :: Timeout -> SessionId -> Connection -> Reply (Either RelationalError (Map IncDepName InclusionDependency))
handleRetrieveInclusionDependencies ti sessionId conn = do
  ret <- timeoutOrDie ti (inclusionDependencies sessionId conn)
  reply ret conn
  
handleRetrievePlanForDatabaseContextExpr :: Timeout -> SessionId -> Connection -> DatabaseContextExpr -> Reply (Either RelationalError DatabaseContextExpr)
handleRetrievePlanForDatabaseContextExpr ti sessionId conn dbExpr = do
  ret <- timeoutOrDie ti (planForDatabaseContextExpr sessionId conn dbExpr)
  reply ret conn
  
handleRetrieveTransactionGraph :: Timeout -> SessionId -> Connection -> Reply (Either RelationalError Relation) 
handleRetrieveTransactionGraph ti sessionId conn = do  
  ret <- timeoutOrDie ti (transactionGraphAsRelation sessionId conn)
  reply ret conn
  
handleRetrieveHeadTransactionId :: Timeout -> SessionId -> Connection -> Reply (Maybe TransactionId)
handleRetrieveHeadTransactionId ti sessionId conn = do
  ret <- timeoutOrDie ti (headTransactionId sessionId conn)
  reply ret conn
  
handleCreateSessionAtCommit :: Timeout -> TransactionId -> Connection -> Reply (Either RelationalError SessionId)
handleCreateSessionAtCommit ti commitId conn = do
  ret <- timeoutOrDie ti (createSessionAtCommit commitId conn)
  reply ret conn
  
handleCreateSessionAtHead :: Timeout -> HeadName -> Connection -> Reply (Either RelationalError SessionId)
handleCreateSessionAtHead ti headn conn = do
  ret <- timeoutOrDie ti (createSessionAtHead headn conn)
  reply ret conn
  
handleCloseSession :: Timeout -> SessionId -> Connection -> Reply ()   
handleCloseSession ti sessionId conn = do
  ret <- timeoutOrDie ti (closeSession sessionId conn)
  case ret of
    Right () -> reply (Right ()) conn
    Left err -> reply (Left err) conn
  
handleRetrieveAtomTypesAsRelation :: Timeout -> SessionId -> Connection -> Reply (Either RelationalError Relation)
handleRetrieveAtomTypesAsRelation ti sessionId conn = do
  ret <- timeoutOrDie ti (atomTypesAsRelation sessionId conn)
  reply ret conn
  
-- | Returns a relation which lists the names of relvars in the current session as well as  its types.  
handleRetrieveRelationVariableSummary :: Timeout -> SessionId -> Connection -> Reply (Either RelationalError Relation)
handleRetrieveRelationVariableSummary ti sessionId conn = do
  ret <- timeoutOrDie ti (relationVariablesAsRelation sessionId conn)
  reply ret conn  
  
handleRetrieveCurrentSchemaName :: Timeout -> SessionId -> Connection -> Reply (Maybe SchemaName)
handleRetrieveCurrentSchemaName ti sessionId conn = do
  ret <- timeoutOrDie ti (currentSchemaName sessionId conn)
  reply ret conn  

handleExecuteSchemaExpr :: Timeout -> SessionId -> Connection -> SchemaExpr -> Reply (Maybe RelationalError)
handleExecuteSchemaExpr ti sessionId conn schemaExpr = do
  ret <- timeoutOrDie ti (executeSchemaExpr sessionId conn schemaExpr)
  reply ret conn
  
handleLogout :: Timeout -> Connection -> Reply Bool
handleLogout _ conn = do
  --liftIO $ closeRemote_ conn
  reply (pure True) conn
    
handleTestTimeout :: Timeout -> SessionId -> Connection -> Reply Bool  
handleTestTimeout ti _ conn = do
  ret <- timeoutOrDie ti (threadDelay 100000 >> pure True)
  reply ret conn

  
 
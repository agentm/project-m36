module ProjectM36.Server.EntryPoints where
import ProjectM36.Base hiding (inclusionDependencies)
import ProjectM36.IsomorphicSchema
import ProjectM36.Client
import ProjectM36.Error
import Control.Distributed.Process (Process, ProcessId, die)
import Control.Distributed.Process.ManagedProcess (ProcessReply)
import Control.Distributed.Process.ManagedProcess.Server (reply)
import Control.Monad.IO.Class (liftIO)
import Data.Map
import System.Timeout

timeoutOrDie :: Int -> IO a -> Process a
timeoutOrDie micros act = do
  if micros == 0 then
    liftIO act
    else do
    mRes <- liftIO (timeout micros act)
    case mRes of
      Just res -> pure res
      Nothing -> die ("TIMEOUT" :: String)
    
type Timeout = Int    
    
handleExecuteRelationalExpr :: Timeout -> SessionId -> Connection -> RelationalExpr -> Process (ProcessReply (Either RelationalError Relation) Connection)
handleExecuteRelationalExpr ti sessionId conn expr = do
  ret <- timeoutOrDie ti (executeRelationalExpr sessionId conn expr)
  reply ret conn
  
handleExecuteDatabaseContextExpr :: Timeout -> SessionId -> Connection -> DatabaseContextExpr -> Process (ProcessReply (Maybe RelationalError) Connection)
handleExecuteDatabaseContextExpr ti sessionId conn dbexpr = do
  ret <- timeoutOrDie ti (executeDatabaseContextExpr sessionId conn dbexpr)
  reply ret conn
  
handleExecuteDatabaseContextIOExpr :: Timeout -> SessionId -> Connection -> DatabaseContextIOExpr -> Process (ProcessReply (Maybe RelationalError) Connection)
handleExecuteDatabaseContextIOExpr ti sessionId conn dbexpr = do
  ret <- timeoutOrDie ti (executeDatabaseContextIOExpr sessionId conn dbexpr)
  reply ret conn
  
handleExecuteHeadName :: Timeout -> SessionId -> Connection -> Process (ProcessReply (Maybe HeadName) Connection)
handleExecuteHeadName ti sessionId conn = do
  ret <- timeoutOrDie ti (headName sessionId conn)
  reply ret conn
  
handleLogin :: Timeout -> Connection -> ProcessId -> Process (ProcessReply Bool Connection)
handleLogin ti conn newClientProcessId = do
  _ <- timeoutOrDie ti (addClientNode conn newClientProcessId)
  reply True conn
  
handleExecuteGraphExpr :: Timeout -> SessionId -> Connection -> TransactionGraphOperator -> Process (ProcessReply (Maybe RelationalError) Connection)
handleExecuteGraphExpr ti sessionId conn graphExpr = do
  ret <- timeoutOrDie ti (executeGraphExpr sessionId conn graphExpr)
  reply ret conn
  
handleExecuteTransGraphRelationalExpr :: Timeout -> SessionId -> Connection -> TransGraphRelationalExpr -> Process (ProcessReply (Either RelationalError Relation) Connection)
handleExecuteTransGraphRelationalExpr ti sessionId conn graphExpr = do
  ret <- timeoutOrDie ti (executeTransGraphRelationalExpr sessionId conn graphExpr)
  reply ret conn

handleExecuteTypeForRelationalExpr :: Timeout -> SessionId -> Connection -> RelationalExpr -> Process (ProcessReply (Either RelationalError Relation) Connection)  
handleExecuteTypeForRelationalExpr ti sessionId conn relExpr = do
  ret <- timeoutOrDie ti (typeForRelationalExpr sessionId conn relExpr)
  reply ret conn
  
handleRetrieveInclusionDependencies :: Timeout -> SessionId -> Connection -> Process (ProcessReply (Either RelationalError (Map IncDepName InclusionDependency)) Connection)
handleRetrieveInclusionDependencies ti sessionId conn = do
  ret <- timeoutOrDie ti (inclusionDependencies sessionId conn)
  reply ret conn
  
handleRetrievePlanForDatabaseContextExpr :: Timeout -> SessionId -> Connection -> DatabaseContextExpr -> Process (ProcessReply (Either RelationalError DatabaseContextExpr) Connection)
handleRetrievePlanForDatabaseContextExpr ti sessionId conn dbExpr = do
  ret <- timeoutOrDie ti (planForDatabaseContextExpr sessionId conn dbExpr)
  reply ret conn
  
handleRetrieveTransactionGraph :: Timeout -> SessionId -> Connection -> Process (ProcessReply (Either RelationalError Relation) Connection)
handleRetrieveTransactionGraph ti sessionId conn = do  
  ret <- timeoutOrDie ti (transactionGraphAsRelation sessionId conn)
  reply ret conn
  
handleRetrieveHeadTransactionId :: Timeout -> SessionId -> Connection -> Process (ProcessReply (Maybe TransactionId) Connection)
handleRetrieveHeadTransactionId ti sessionId conn = do
  ret <- timeoutOrDie ti (headTransactionId sessionId conn)
  reply ret conn
  
handleCreateSessionAtCommit :: Timeout -> TransactionId -> Connection -> Process (ProcessReply (Either RelationalError SessionId) Connection)  
handleCreateSessionAtCommit ti commitId conn = do
  ret <- timeoutOrDie ti (createSessionAtCommit commitId conn)
  reply ret conn
  
handleCreateSessionAtHead :: Timeout -> HeadName -> Connection -> Process (ProcessReply (Either RelationalError SessionId) Connection)
handleCreateSessionAtHead ti headn conn = do
  ret <- timeoutOrDie ti (createSessionAtHead headn conn)
  reply ret conn
  
handleCloseSession :: Timeout -> SessionId -> Connection -> Process (ProcessReply () Connection)  
handleCloseSession ti sessionId conn = do
  _ <- timeoutOrDie ti (closeSession sessionId conn)
  reply () conn
  
handleRetrieveAtomTypesAsRelation :: Timeout -> SessionId -> Connection -> Process (ProcessReply (Either RelationalError Relation) Connection)  
handleRetrieveAtomTypesAsRelation ti sessionId conn = do
  ret <- timeoutOrDie ti (atomTypesAsRelation sessionId conn)
  reply ret conn
  
-- | Returns a relation which lists the names of relvars in the current session as well as  its types.  
handleRetrieveRelationVariableSummary :: Timeout -> SessionId -> Connection -> Process (ProcessReply (Either RelationalError Relation) Connection)  
handleRetrieveRelationVariableSummary ti sessionId conn = do
  ret <- timeoutOrDie ti (relationVariablesAsRelation sessionId conn)
  reply ret conn  
  
handleRetrieveCurrentSchemaName :: Timeout -> SessionId -> Connection -> Process (ProcessReply (Maybe SchemaName) Connection)
handleRetrieveCurrentSchemaName ti sessionId conn = do
  ret <- timeoutOrDie ti (currentSchemaName sessionId conn)
  reply ret conn
  
handleExecuteSchemaExpr :: Timeout -> SessionId -> Connection -> SchemaExpr -> Process (ProcessReply (Maybe RelationalError) Connection)  
handleExecuteSchemaExpr ti sessionId conn schemaExpr = do
  ret <- timeoutOrDie ti (executeSchemaExpr sessionId conn schemaExpr)
  reply ret conn
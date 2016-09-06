module ProjectM36.Server.EntryPoints where
import ProjectM36.Base hiding (inclusionDependencies)
import ProjectM36.Client
import ProjectM36.Error
import Control.Distributed.Process (Process, ProcessId)
import Control.Distributed.Process.ManagedProcess (ProcessReply)
import Control.Distributed.Process.ManagedProcess.Server (reply)
import Control.Monad.IO.Class (liftIO)
import Data.Map

handleExecuteRelationalExpr :: SessionId -> Connection -> RelationalExpr -> Process (ProcessReply (Either RelationalError Relation) Connection)
handleExecuteRelationalExpr sessionId conn expr = do
  ret <- liftIO $ executeRelationalExpr sessionId conn expr
  reply ret conn
  
handleExecuteDatabaseContextExpr :: SessionId -> Connection -> DatabaseContextExpr -> Process (ProcessReply (Maybe RelationalError) Connection)
handleExecuteDatabaseContextExpr sessionId conn dbexpr = do
  ret <- liftIO $ executeDatabaseContextExpr sessionId conn dbexpr
  reply ret conn
  
handleExecuteDatabaseContextIOExpr :: SessionId -> Connection -> DatabaseContextIOExpr -> Process (ProcessReply (Maybe RelationalError) Connection)
handleExecuteDatabaseContextIOExpr sessionId conn dbexpr = do
  ret <- liftIO $ executeDatabaseContextIOExpr sessionId conn dbexpr
  reply ret conn
  
handleExecuteHeadName :: SessionId -> Connection -> Process (ProcessReply (Maybe HeadName) Connection)
handleExecuteHeadName sessionId conn = do
  ret <- liftIO $ headName sessionId conn 
  reply ret conn
  
handleLogin :: Connection -> ProcessId -> Process (ProcessReply Bool Connection)
handleLogin conn newClientProcessId = do
  liftIO (addClientNode conn newClientProcessId)
  reply True conn
  
handleExecuteGraphExpr :: SessionId -> Connection -> TransactionGraphOperator -> Process (ProcessReply (Maybe RelationalError) Connection)
handleExecuteGraphExpr sessionId conn graphExpr = do
  ret <- liftIO $ executeGraphExpr sessionId conn graphExpr
  reply ret conn
  
handleExecuteTransGraphRelationalExpr :: SessionId -> Connection -> TransGraphRelationalExpr -> Process (ProcessReply (Either RelationalError Relation) Connection)
handleExecuteTransGraphRelationalExpr sessionId conn graphExpr = do
  ret <- liftIO $ executeTransGraphRelationalExpr sessionId conn graphExpr
  reply ret conn

handleExecuteTypeForRelationalExpr :: SessionId -> Connection -> RelationalExpr -> Process (ProcessReply (Either RelationalError Relation) Connection)  
handleExecuteTypeForRelationalExpr sessionId conn relExpr = do
  ret <- liftIO $ typeForRelationalExpr sessionId conn relExpr
  reply ret conn
  
handleRetrieveInclusionDependencies :: SessionId -> Connection -> Process (ProcessReply (Either RelationalError (Map IncDepName InclusionDependency)) Connection)
handleRetrieveInclusionDependencies sessionId conn = do
  ret <- liftIO $ inclusionDependencies sessionId conn
  reply ret conn
  
handleRetrievePlanForDatabaseContextExpr :: SessionId -> Connection -> DatabaseContextExpr -> Process (ProcessReply (Either RelationalError DatabaseContextExpr) Connection)
handleRetrievePlanForDatabaseContextExpr sessionId conn dbExpr = do
  ret <- liftIO $ planForDatabaseContextExpr sessionId conn dbExpr
  reply ret conn
  
handleRetrieveTransactionGraph :: SessionId -> Connection -> Process (ProcessReply (Either RelationalError Relation) Connection)
handleRetrieveTransactionGraph sessionId conn = do  
  ret <- liftIO $ transactionGraphAsRelation sessionId conn
  reply ret conn
  
handleRetrieveHeadTransactionId :: SessionId -> Connection -> Process (ProcessReply (Maybe TransactionId) Connection)
handleRetrieveHeadTransactionId sessionId conn = do
  ret <- liftIO $ headTransactionId sessionId conn  
  reply ret conn
  
handleCreateSessionAtCommit :: TransactionId -> Connection -> Process (ProcessReply (Either RelationalError SessionId) Connection)  
handleCreateSessionAtCommit commitId conn = do
  ret <- liftIO $ createSessionAtCommit commitId conn
  reply ret conn
  
handleCreateSessionAtHead :: HeadName -> Connection -> Process (ProcessReply (Either RelationalError SessionId) Connection)
handleCreateSessionAtHead headn conn = do
  ret <- liftIO $ createSessionAtHead headn conn
  reply ret conn
  
handleCloseSession :: SessionId -> Connection -> Process (ProcessReply () Connection)  
handleCloseSession sessionId conn = do
  liftIO $ closeSession sessionId conn
  reply () conn
  
handleRetrieveAtomTypesAsRelation :: SessionId -> Connection -> Process (ProcessReply (Either RelationalError Relation) Connection)  
handleRetrieveAtomTypesAsRelation sessionId conn = do
  ret <- liftIO $ atomTypesAsRelation sessionId conn
  reply ret conn
  
-- | Returns a relation which lists the names of relvars in the current session as well as  its types.  
handleRetrieveRelationVariableSummary :: SessionId -> Connection -> Process (ProcessReply (Either RelationalError Relation) Connection)  
handleRetrieveRelationVariableSummary sessionId conn = do
  ret <- liftIO $ relationVariablesAsRelation sessionId conn
  reply ret conn  
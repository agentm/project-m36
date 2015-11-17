module ProjectM36.Daemon.EntryPoints where
import ProjectM36.Base hiding (inclusionDependencies)
import ProjectM36.Client
import ProjectM36.Error
import Control.Distributed.Process (Process)
import Control.Distributed.Process.ManagedProcess (ProcessReply)
import Control.Distributed.Process.ManagedProcess.Server (reply)
import Control.Monad.IO.Class (liftIO)
import Data.Map
import Data.UUID (UUID)

handleExecuteRelationalExpr :: Connection -> RelationalExpr -> Process (ProcessReply (Either RelationalError Relation) Connection)
handleExecuteRelationalExpr conn expr = do
  ret <- liftIO $ executeRelationalExpr conn expr
  reply ret conn
  
handleExecuteDatabaseContextExpr :: Connection -> DatabaseExpr -> Process (ProcessReply (Maybe RelationalError) Connection)
handleExecuteDatabaseContextExpr conn dbexpr = do
  ret <- liftIO $ executeDatabaseContextExpr conn dbexpr
  reply ret conn
  
handleExecuteHeadName :: Connection -> Process (ProcessReply (Maybe HeadName) Connection)
handleExecuteHeadName conn = do
  ret <- liftIO $ headName conn 
  reply ret conn
  
handleLogin :: Connection -> Process (ProcessReply Bool Connection)
handleLogin conn = reply True conn
  
handleExecuteGraphExpr :: Connection -> TransactionGraphOperator -> Process (ProcessReply (Maybe RelationalError) Connection)
handleExecuteGraphExpr conn graphExpr = do
  ret <- liftIO $ executeGraphExpr conn graphExpr
  reply ret conn
  
handleExecuteTypeForRelationalExpr :: Connection -> RelationalExpr -> Process (ProcessReply (Either RelationalError Relation) Connection)  
handleExecuteTypeForRelationalExpr conn relExpr = do
  ret <- liftIO $ typeForRelationalExpr conn relExpr
  reply ret conn
  
handleRetrieveInclusionDependencies :: Connection -> Process (ProcessReply (Map IncDepName InclusionDependency) Connection)
handleRetrieveInclusionDependencies conn = do
  ret <- liftIO $ inclusionDependencies conn
  reply ret conn
  
handleRetrievePlanForDatabaseContextExpr :: Connection -> DatabaseExpr -> Process (ProcessReply (Either RelationalError DatabaseExpr) Connection)
handleRetrievePlanForDatabaseContextExpr conn dbExpr = do
  ret <- liftIO $ planForDatabaseContextExpr conn dbExpr
  reply ret conn
  
handleRetrieveTransactionGraph :: Connection -> Process (ProcessReply (Either RelationalError Relation) Connection)
handleRetrieveTransactionGraph conn = do  
  ret <- liftIO $ transactionGraphAsRelation conn
  reply ret conn
  
handleRetrieveHeadTransactionUUID :: Connection -> Process (ProcessReply UUID Connection)
handleRetrieveHeadTransactionUUID conn = do
  ret <- liftIO $ headTransactionUUID conn  
  reply ret conn
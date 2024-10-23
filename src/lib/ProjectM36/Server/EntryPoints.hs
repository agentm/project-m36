{-# LANGUAGE MonoLocalBinds #-}
module ProjectM36.Server.EntryPoints where
import ProjectM36.Base hiding (inclusionDependencies)
import ProjectM36.IsomorphicSchema
import ProjectM36.HashSecurely
import ProjectM36.SQL.Select
import ProjectM36.SQL.DBUpdate
import ProjectM36.Client as C
import Data.Map
import Control.Concurrent (threadDelay)
import Network.RPC.Curryer.Server 
import System.Timeout hiding (Timeout)
import Network.Socket
import Control.Exception

timeoutOrDie :: Maybe Timeout -> IO a -> IO (Maybe a)
timeoutOrDie mMicros act = 
  case mMicros of
    Nothing -> Just <$> act
    Just micros ->
      timeout (fromIntegral micros) act

timeoutRelErr :: Maybe Timeout -> IO (Either RelationalError a) -> IO (Either RelationalError a)
timeoutRelErr mMicros act = do
  ret <- timeoutOrDie mMicros act
  case ret of
    Nothing -> throw TimeoutException
    Just v -> pure v
                                      

handleExecuteRelationalExpr :: Maybe Timeout -> SessionId -> Connection -> RelationalExpr -> IO (Either RelationalError Relation)
handleExecuteRelationalExpr ti sessionId conn expr = 
  timeoutRelErr ti (executeRelationalExpr sessionId conn expr)

handleExecuteDataFrameExpr :: Maybe Timeout -> SessionId -> Connection -> DataFrameExpr -> IO (Either RelationalError DataFrame)
handleExecuteDataFrameExpr ti sessionId conn expr =
  timeoutRelErr ti (executeDataFrameExpr sessionId conn expr)
  
handleExecuteDatabaseContextExpr :: Maybe Timeout -> SessionId -> Connection -> DatabaseContextExpr -> IO (Either RelationalError ())
handleExecuteDatabaseContextExpr ti sessionId conn dbexpr =
  timeoutRelErr ti (executeDatabaseContextExpr sessionId conn dbexpr)
  
handleExecuteDatabaseContextIOExpr :: Maybe Timeout -> SessionId -> Connection -> DatabaseContextIOExpr -> IO (Either RelationalError ())
handleExecuteDatabaseContextIOExpr ti sessionId conn dbexpr =
  timeoutRelErr ti (executeDatabaseContextIOExpr sessionId conn dbexpr)
  
handleExecuteHeadName :: Maybe Timeout -> SessionId -> Connection -> IO (Either RelationalError HeadName)
handleExecuteHeadName ti sessionId conn =
  timeoutRelErr ti (headName sessionId conn)
  
handleLogin :: Connection -> Locking Socket -> IO Bool
handleLogin conn lockSock = do
  addClientNode conn lockSock
  pure True
  
handleExecuteGraphExpr :: Maybe Timeout -> SessionId -> Connection -> TransactionGraphOperator -> IO (Either RelationalError ())
handleExecuteGraphExpr ti sessionId conn graphExpr =
  timeoutRelErr ti (executeGraphExpr sessionId conn graphExpr)
  
handleExecuteTransGraphRelationalExpr :: Maybe Timeout -> SessionId -> Connection -> TransGraphRelationalExpr -> IO (Either RelationalError Relation)
handleExecuteTransGraphRelationalExpr ti sessionId conn graphExpr =
  timeoutRelErr ti (executeTransGraphRelationalExpr sessionId conn graphExpr)

handleExecuteTypeForRelationalExpr :: Maybe Timeout -> SessionId -> Connection -> RelationalExpr -> IO (Either RelationalError Relation)
handleExecuteTypeForRelationalExpr ti sessionId conn relExpr =
  timeoutRelErr ti (typeForRelationalExpr sessionId conn relExpr)
  
handleRetrieveInclusionDependencies :: Maybe Timeout -> SessionId -> Connection -> IO (Either RelationalError (Map IncDepName InclusionDependency))
handleRetrieveInclusionDependencies ti sessionId conn =
  timeoutRelErr ti (inclusionDependencies sessionId conn)
  
handleRetrievePlanForDatabaseContextExpr :: Maybe Timeout -> SessionId -> Connection -> DatabaseContextExpr -> IO (Either RelationalError GraphRefDatabaseContextExpr)
handleRetrievePlanForDatabaseContextExpr ti sessionId conn dbExpr =
  timeoutRelErr ti (planForDatabaseContextExpr sessionId conn dbExpr)
  
handleRetrieveTransactionGraph :: Maybe Timeout -> SessionId -> Connection -> IO (Either RelationalError Relation) 
handleRetrieveTransactionGraph ti sessionId conn =
  timeoutRelErr ti (transactionGraphAsRelation sessionId conn)
  
handleRetrieveHeadTransactionId :: Maybe Timeout -> SessionId -> Connection -> IO (Either RelationalError TransactionId)
handleRetrieveHeadTransactionId ti sessionId conn =
  timeoutRelErr ti (headTransactionId sessionId conn)
  
handleCreateSessionAtCommit :: Maybe Timeout -> Connection -> TransactionId -> IO (Either RelationalError SessionId)
handleCreateSessionAtCommit ti conn commitId =
  timeoutRelErr ti (createSessionAtCommit conn commitId)
  
handleCreateSessionAtHead :: Maybe Timeout -> Connection -> HeadName -> IO (Either RelationalError SessionId)
handleCreateSessionAtHead ti conn headn = 
  timeoutRelErr ti (createSessionAtHead conn headn)
  
handleCloseSession :: SessionId -> Connection -> IO ()   
handleCloseSession  =
  closeSession
  
handleRetrieveAtomTypesAsRelation :: Maybe Timeout -> SessionId -> Connection -> IO (Either RelationalError Relation)
handleRetrieveAtomTypesAsRelation ti sessionId conn =
  timeoutRelErr ti (atomTypesAsRelation sessionId conn)
  
-- | Returns a relation which lists the names of relvars in the current session as well as  its types.  
handleRetrieveRelationVariableSummary :: Maybe Timeout -> SessionId -> Connection -> IO (Either RelationalError Relation)
handleRetrieveRelationVariableSummary ti sessionId conn =
  timeoutRelErr ti (relationVariablesAsRelation sessionId conn)
  
handleRetrieveAtomFunctionSummary :: Maybe Timeout -> SessionId -> Connection -> IO (Either RelationalError Relation)
handleRetrieveAtomFunctionSummary ti sessionId conn = 
  timeoutRelErr ti (atomFunctionsAsRelation sessionId conn)
  
handleRetrieveDatabaseContextFunctionSummary :: Maybe Timeout -> SessionId -> Connection -> IO (Either RelationalError Relation)
handleRetrieveDatabaseContextFunctionSummary ti sessionId conn = 
  timeoutRelErr ti (databaseContextFunctionsAsRelation sessionId conn)
  
handleRetrieveCurrentSchemaName :: Maybe Timeout -> SessionId -> Connection -> IO (Either RelationalError SchemaName)
handleRetrieveCurrentSchemaName ti sessionId conn =
  timeoutRelErr ti (currentSchemaName sessionId conn)

handleExecuteSchemaExpr :: Maybe Timeout -> SessionId -> Connection -> SchemaExpr -> IO (Either RelationalError ())
handleExecuteSchemaExpr ti sessionId conn schemaExpr =
  timeoutRelErr ti (executeSchemaExpr sessionId conn schemaExpr)
  
handleLogout :: Maybe Timeout -> Connection -> IO Bool
handleLogout _ _ = 
  --liftIO $ closeRemote_ conn
  pure True
    
handleTestTimeout :: Maybe Timeout -> SessionId -> Connection -> IO Bool  
handleTestTimeout ti _ _ = do
  ret <- timeoutRelErr ti (threadDelay 100000 >> pure (Right ()))
  case ret of
    Right () -> pure True
    Left _ -> pure False

 

handleRetrieveSessionIsDirty :: Maybe Timeout -> SessionId -> Connection -> IO (Either RelationalError Bool)
handleRetrieveSessionIsDirty ti sessionId conn =
  timeoutRelErr ti (disconnectedTransactionIsDirty sessionId conn)
  
handleExecuteAutoMergeToHead :: Maybe Timeout -> SessionId -> Connection -> MergeStrategy -> HeadName -> IO (Either RelationalError ())
handleExecuteAutoMergeToHead ti sessionId conn strat headName' =
  timeoutRelErr ti (autoMergeToHead sessionId conn strat headName')

handleRetrieveTypeConstructorMapping :: Maybe Timeout -> SessionId -> Connection -> IO (Either RelationalError TypeConstructorMapping)  
handleRetrieveTypeConstructorMapping ti sessionId conn =
  timeoutRelErr ti (C.typeConstructorMapping sessionId conn)
 
handleValidateMerkleHashes :: Maybe Timeout -> SessionId -> Connection -> IO (Either RelationalError ())
handleValidateMerkleHashes ti sessionId conn = 
  timeoutRelErr ti (C.validateMerkleHashes sessionId conn)

handleGetDDLHash :: Maybe Timeout -> SessionId -> Connection -> IO (Either RelationalError SecureHash)
handleGetDDLHash ti sessionId conn =
  timeoutRelErr ti (C.getDDLHash sessionId conn)

handleRetrieveDDLAsRelation :: Maybe Timeout -> SessionId -> Connection -> IO (Either RelationalError Relation)
handleRetrieveDDLAsRelation ti sessionId conn =
  timeoutRelErr ti (C.ddlAsRelation sessionId conn)

handleRetrieveRegisteredQueries :: Maybe Timeout -> SessionId -> Connection -> IO (Either RelationalError Relation)
handleRetrieveRegisteredQueries ti sessionId conn =
  timeoutRelErr ti (C.registeredQueriesAsRelation sessionId conn)

handleConvertSQLQuery :: Maybe Timeout -> SessionId -> Connection -> Query -> IO (Either RelationalError DataFrameExpr)
handleConvertSQLQuery ti sessionId conn sel = timeoutRelErr ti (C.convertSQLQuery sessionId conn sel)

handleConvertSQLUpdates :: Maybe Timeout -> SessionId -> Connection -> [DBUpdate] -> IO (Either RelationalError DatabaseContextExpr)
handleConvertSQLUpdates ti sessionId conn ups = timeoutRelErr ti (C.convertSQLDBUpdates sessionId conn ups)

handleRetrieveNotificationsAsRelation :: Maybe Timeout -> SessionId -> Connection -> IO (Either RelationalError Relation)
handleRetrieveNotificationsAsRelation ti sessionId conn =
  timeoutRelErr ti (C.notificationsAsRelation sessionId conn)

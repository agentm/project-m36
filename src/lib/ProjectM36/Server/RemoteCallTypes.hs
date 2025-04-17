{-# LANGUAGE DeriveGeneric, DerivingVia, CPP #-}
module ProjectM36.Server.RemoteCallTypes where
import ProjectM36.Base
import ProjectM36.IsomorphicSchema
import ProjectM36.TransactionGraph
import ProjectM36.DataFrame
import ProjectM36.TransGraphRelationalExpression
import ProjectM36.Session
import ProjectM36.IsomorphicSchema.Types
import ProjectM36.Serialise.DataFrame ()
import ProjectM36.Serialise.IsomorphicSchema ()
import ProjectM36.SQL.Select
import ProjectM36.SQL.DBUpdate
import GHC.Generics
import Codec.Winery

#define RPCData(typeName) deriving Generic \
  deriving Serialise via WineryVariant typeName

{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}
-- | The initial login message. The argument should be the process id of the initiating client. This ProcessId will receive notification callbacks.
data Login = Login DatabaseName
  RPCData(Login)
                    
data Logout = Logout
  RPCData(Logout)

data ExecuteRelationalExpr = ExecuteRelationalExpr SessionId RelationalExpr
  RPCData(ExecuteRelationalExpr)

data ExecuteDataFrameExpr = ExecuteDataFrameExpr SessionId DataFrameExpr
  RPCData(ExecuteDataFrameExpr)
  
data ExecuteDatabaseContextExpr = ExecuteDatabaseContextExpr SessionId DatabaseContextExpr
  RPCData(ExecuteDatabaseContextExpr)
  
data ExecuteDatabaseContextIOExpr = ExecuteDatabaseContextIOExpr SessionId DatabaseContextIOExpr
  RPCData(ExecuteDatabaseContextIOExpr)
  
data ExecuteGraphExpr = ExecuteGraphExpr SessionId TransactionGraphExpr
  RPCData(ExecuteGraphExpr)

data ExecuteAlterTransactionGraphExpr = ExecuteAlterTransactionGraphExpr SessionId AlterTransactionGraphExpr
  RPCData(ExecuteAlterTransactionGraphExpr)
  
data ExecuteTransGraphRelationalExpr = ExecuteTransGraphRelationalExpr SessionId TransGraphRelationalExpr                               
  RPCData(ExecuteTransGraphRelationalExpr)
  
data ExecuteHeadName = ExecuteHeadName SessionId
  RPCData(ExecuteHeadName)
  
data ExecuteTypeForRelationalExpr = ExecuteTypeForRelationalExpr SessionId RelationalExpr
  RPCData(ExecuteTypeForRelationalExpr)
  
data ExecuteSchemaExpr = ExecuteSchemaExpr SessionId SchemaExpr                            RPCData(ExecuteSchemaExpr)     
  
data ExecuteSetCurrentSchema = ExecuteSetCurrentSchema SessionId SchemaName
  RPCData(ExecuteSetCurrentSchema)
  
data RetrieveInclusionDependencies = RetrieveInclusionDependencies SessionId
  RPCData(RetrieveInclusionDependencies)
  
data RetrievePlanForDatabaseContextExpr = RetrievePlanForDatabaseContextExpr SessionId DatabaseContextExpr
  RPCData(RetrievePlanForDatabaseContextExpr)

data RetrievePlanForRelationalExpr = RetrievePlanForRelationalExpr SessionId RelationalExpr
  RPCData(RetrievePlanForRelationalExpr)
  
data RetrieveTransactionGraph = RetrieveTransactionGraph SessionId
  RPCData(RetrieveTransactionGraph)
  
data RetrieveHeadTransactionId = RetrieveHeadTransactionId SessionId
  RPCData(RetrieveHeadTransactionId)
  
data CreateSessionAtTransactionId = CreateSessionAtTransactionId TransactionId
  RPCData(CreateSessionAtTransactionId)
  
data CreateSessionAtHead = CreateSessionAtHead HeadName
  RPCData(CreateSessionAtHead)
  
data CloseSession = CloseSession SessionId
  RPCData(CloseSession)
  
data RetrieveAtomTypesAsRelation = RetrieveAtomTypesAsRelation SessionId
  RPCData(RetrieveAtomTypesAsRelation)

data RetrieveNotificationsAsRelation = RetrieveNotificationsAsRelation SessionId
  RPCData(RetrieveNotificationsAsRelation)
  
data RetrieveRelationVariableSummary = RetrieveRelationVariableSummary SessionId
  RPCData(RetrieveRelationVariableSummary)
  
data RetrieveAtomFunctionSummary = RetrieveAtomFunctionSummary SessionId
  RPCData(RetrieveAtomFunctionSummary)
  
data RetrieveDatabaseContextFunctionSummary = RetrieveDatabaseContextFunctionSummary SessionId
  RPCData(RetrieveDatabaseContextFunctionSummary)
  
data RetrieveCurrentSchemaName = RetrieveCurrentSchemaName SessionId
  RPCData(RetrieveCurrentSchemaName)
  
data TestTimeout = TestTimeout SessionId                                          
  RPCData(TestTimeout)
  
data RetrieveSessionIsDirty = RetrieveSessionIsDirty SessionId
  RPCData(RetrieveSessionIsDirty)
  
data ExecuteAutoMergeToHead = ExecuteAutoMergeToHead SessionId MergeStrategy HeadName
  RPCData(ExecuteAutoMergeToHead)
  
data RetrieveTypeConstructorMapping = RetrieveTypeConstructorMapping SessionId 
  RPCData(RetrieveTypeConstructorMapping)

data ExecuteValidateMerkleHashes = ExecuteValidateMerkleHashes SessionId
  RPCData(ExecuteValidateMerkleHashes)

data GetDDLHash = GetDDLHash SessionId
  RPCData(GetDDLHash)

data RetrieveDDLAsRelation = RetrieveDDLAsRelation SessionId
  RPCData(RetrieveDDLAsRelation)

data RetrieveRegisteredQueries = RetrieveRegisteredQueries SessionId
  RPCData(RetrieveRegisteredQueries)

data ConvertSQLQuery = ConvertSQLQuery SessionId Query
  RPCData(ConvertSQLQuery)

data ConvertSQLUpdates = ConvertSQLUpdates SessionId [DBUpdate]
  RPCData(ConvertSQLUpdates)

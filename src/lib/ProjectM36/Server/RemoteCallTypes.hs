{-# LANGUAGE DeriveGeneric, DerivingVia, CPP #-}
module ProjectM36.Server.RemoteCallTypes where
import ProjectM36.Base
import ProjectM36.IsomorphicSchema
import ProjectM36.TransactionGraph
import ProjectM36.DataFrame
import ProjectM36.TransGraphRelationalExpression
import ProjectM36.Session
import ProjectM36.Serialise.DataFrame ()
import ProjectM36.Serialise.IsomorphicSchema ()
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
  
data ExecuteGraphExpr = ExecuteGraphExpr SessionId TransactionGraphOperator 
  RPCData(ExecuteGraphExpr)
  
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
  
data RetrieveTransactionGraph = RetrieveTransactionGraph SessionId
  RPCData(RetrieveTransactionGraph)
  
data RetrieveHeadTransactionId = RetrieveHeadTransactionId SessionId
  RPCData(RetrieveHeadTransactionId)
  
data CreateSessionAtCommit = CreateSessionAtCommit TransactionId
  RPCData(CreateSessionAtCommit)
  
data CreateSessionAtHead = CreateSessionAtHead HeadName
  RPCData(CreateSessionAtHead)
  
data CloseSession = CloseSession SessionId
  RPCData(CloseSession)
  
data RetrieveAtomTypesAsRelation = RetrieveAtomTypesAsRelation SessionId
  RPCData(RetrieveAtomTypesAsRelation)
  
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

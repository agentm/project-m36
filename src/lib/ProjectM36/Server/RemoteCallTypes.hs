{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingVia #-}
module ProjectM36.Server.RemoteCallTypes where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Serialise.Error
import Data.Map (Map)
import ProjectM36.IsomorphicSchema
import ProjectM36.TransactionGraph
import ProjectM36.DataFrame
import ProjectM36.TransGraphRelationalExpression
import ProjectM36.Session
import ProjectM36.Serialise.DataFrame
import GHC.Generics
import Data.Binary
import Control.Distributed.Process (ProcessId)
import Codec.Winery

data Request = Login |
               Logout |
               ExecuteRelationalExpr SessionId RelationalExpr |
               ExecuteDataFrameExpr SessionId DataFrameExpr |
               ExecuteDatabaseConextExpr SessionId DatabaseContextExpr |
               ExecuteDatabaseContextIOExpr SessionId DatabaseContextIOExpr |
               ExecuteGraphExpr SessionId TransactionGraphOperator |
               ExecuteTransGraphRelationalExpr SessionId TransGraphRelationalExpr |
               ExecuteHeadName SessionId |
               ExecuteTypeForRelationalExpr SessionId RelationalExpr |
               ExecuteSchemaExpr SessionId SchemaExpr |
               ExecuteSetCurrentSchema SessionId SchemaName |
               RetrieveInclusionDependencies SessionId |
               RetrievePlanForDatabaseContextExpr SessionId DatabaseContextExpr |
               RetrieveTransactionGraph SessionId |
               RetrieveHeadTransactionId SessionId |
               CreateSessionAtCommit TransactionId |
               CreateSessionAtHead HeadName |
               CloseSession SessionId |
               RetrieveAtomTypesAsRelation SessionId |
               RetrieveRelationVariableSummary SessionId |
               RetrieveAtomFunctionSummary SessionId |
               RetrieveDatabaseContextFunctionSummary SessionId |
               RetrieveCurrentSchemaName SessionId |
               TestTimeout SessionId |
               RetrieveSessionIsDirty SessionId |
               ExecuteAutoMergeToHead SessionId MergeStrategy HeadName |
               RetrieveTypeConstructorMapping SessionId |
               ExecuteValidateMerkleHashes SessionId
  deriving (Generic)
  deriving Serialise via WineryVariant Request

data Response = RelationResp (Either RelationalError Relation) |
                DataFrameResp (Either RelationalError DataFrame) |
                EUnitResp (Either RelationalError ()) |
                HeadNameResp (Either RelationalError HeadName) |
                BoolResp Bool |
                InclusionDependenciesMapResp (Map IncDepName InclusionDependency) |
                GraphRefDatabaseContextExprResp (Either RelationalError GraphRefDatabaseContextExpr) |
                TransactionIdResp (Either RelationalError TransactionId) |
                SessionIdResp (Either RelationalError SessionId) |
                UnitResp () |
                SchemaNameResp (Either RelationalError SchemaName) |
                EBoolResp (Either RelationalError Bool) |
                TypeConstructorMappingResp (Either RelationalError TypeConstructorMapping)
              deriving (Generic)
              deriving Serialise via WineryVariant Response
  

               
{-               
               
{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}
-- | The initial login message. The argument should be the process id of the initiating client. This ProcessId will receive notification callbacks.
data Login = Login ProcessId
           deriving (Binary, Generic)
                    
data Logout = Logout
            deriving (Binary, Generic)
data ExecuteRelationalExpr = ExecuteRelationalExpr SessionId RelationalExpr 
                           deriving (Binary, Generic)
data ExecuteDataFrameExpr = ExecuteDataFrameExpr SessionId DataFrameExpr
                           deriving (Binary, Generic)
data ExecuteDatabaseContextExpr = ExecuteDatabaseContextExpr SessionId DatabaseContextExpr
                                deriving (Binary, Generic)
data ExecuteDatabaseContextIOExpr = ExecuteDatabaseContextIOExpr SessionId DatabaseContextIOExpr
                                deriving (Binary, Generic)                                         
data ExecuteGraphExpr = ExecuteGraphExpr SessionId TransactionGraphOperator 
                      deriving (Binary, Generic)
data ExecuteTransGraphRelationalExpr = ExecuteTransGraphRelationalExpr SessionId TransGraphRelationalExpr                               
                                     deriving (Binary, Generic)
data ExecuteHeadName = ExecuteHeadName SessionId
                     deriving (Binary, Generic)
data ExecuteTypeForRelationalExpr = ExecuteTypeForRelationalExpr SessionId RelationalExpr
                                  deriving (Binary, Generic)
data ExecuteSchemaExpr = ExecuteSchemaExpr SessionId SchemaExpr                                 
                         deriving (Binary, Generic)
data ExecuteSetCurrentSchema = ExecuteSetCurrentSchema SessionId SchemaName
                               deriving (Binary, Generic)
data RetrieveInclusionDependencies = RetrieveInclusionDependencies SessionId
                                   deriving (Binary, Generic)
data RetrievePlanForDatabaseContextExpr = RetrievePlanForDatabaseContextExpr SessionId DatabaseContextExpr
                                        deriving (Binary, Generic)
data RetrieveTransactionGraph = RetrieveTransactionGraph SessionId
                              deriving (Binary, Generic)
data RetrieveHeadTransactionId = RetrieveHeadTransactionId SessionId
                                 deriving (Binary, Generic)
data CreateSessionAtCommit = CreateSessionAtCommit TransactionId
                                    deriving (Binary, Generic)
data CreateSessionAtHead = CreateSessionAtHead HeadName
                                  deriving (Binary, Generic)
data CloseSession = CloseSession SessionId
                    deriving (Binary, Generic)
data RetrieveAtomTypesAsRelation = RetrieveAtomTypesAsRelation SessionId
                                   deriving (Binary, Generic)
data RetrieveRelationVariableSummary = RetrieveRelationVariableSummary SessionId
                                     deriving (Binary, Generic)
data RetrieveAtomFunctionSummary = RetrieveAtomFunctionSummary SessionId
                                   deriving (Binary, Generic)
data RetrieveDatabaseContextFunctionSummary = RetrieveDatabaseContextFunctionSummary SessionId
                                   deriving (Binary, Generic)
data RetrieveCurrentSchemaName = RetrieveCurrentSchemaName SessionId
                                 deriving (Binary, Generic)
data TestTimeout = TestTimeout SessionId                                          
                   deriving (Binary, Generic)
data RetrieveSessionIsDirty = RetrieveSessionIsDirty SessionId                            
                            deriving (Binary, Generic)
data ExecuteAutoMergeToHead = ExecuteAutoMergeToHead SessionId MergeStrategy HeadName
                              deriving (Binary, Generic)
data RetrieveTypeConstructorMapping = RetrieveTypeConstructorMapping SessionId 
                                      deriving (Binary, Generic)

data ExecuteValidateMerkleHashes = ExecuteValidateMerkleHashes SessionId
                          deriving (Binary, Generic)
-}

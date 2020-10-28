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
import Network.RPC.Curryer.Client

data Request = Login |
               Logout |
               ExecuteRelationalExpr SessionId RelationalExpr |
               ExecuteDataFrameExpr SessionId DataFrameExpr |
               ExecuteDatabaseContextExpr SessionId DatabaseContextExpr |
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
  
type RPCConnection = Connection Response

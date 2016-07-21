{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module ProjectM36.Server.RemoteCallTypes where
import ProjectM36.Base
import ProjectM36.TransactionGraph
import ProjectM36.Session
import GHC.Generics
import Data.Binary
import Control.Distributed.Process (ProcessId)

-- | The initial login message. The argument should be the process id of the initiating client. This ProcessId will receive notification callbacks.
data Login = Login ProcessId
           deriving (Binary, Generic)
                    
data Logout = Logout
            deriving (Binary, Generic)
data ExecuteRelationalExpr = ExecuteRelationalExpr SessionId RelationalExpr 
                           deriving (Binary, Generic)
data ExecuteDatabaseContextExpr = ExecuteDatabaseContextExpr SessionId DatabaseContextExpr
                                deriving (Binary, Generic)
data ExecuteGraphExpr = ExecuteGraphExpr SessionId TransactionGraphOperator 
                      deriving (Binary, Generic)
data ExecuteHeadName = ExecuteHeadName SessionId
                     deriving (Binary, Generic)
data ExecuteTypeForRelationalExpr = ExecuteTypeForRelationalExpr SessionId RelationalExpr
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
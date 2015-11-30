{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module ProjectM36.Server.RemoteCallTypes where
import ProjectM36.Base
import ProjectM36.TransactionGraph
import GHC.Generics
import Data.Binary
import Data.UUID

type SessionId = UUID

data Login = Login
           deriving (Binary, Generic)
data Logout = Logout
            deriving (Binary, Generic)
data ExecuteRelationalExpr = ExecuteRelationalExpr SessionId RelationalExpr 
                           deriving (Binary, Generic)
data ExecuteDatabaseContextExpr = ExecuteDatabaseContextExpr SessionId DatabaseExpr
                                deriving (Binary, Generic)
data ExecuteGraphExpr = ExecuteGraphExpr SessionId TransactionGraphOperator 
                      deriving (Binary, Generic)
data ExecuteHeadName = ExecuteHeadName SessionId
                     deriving (Binary, Generic)
data ExecuteTypeForRelationalExpr = ExecuteTypeForRelationalExpr SessionId RelationalExpr
                                  deriving (Binary, Generic)
data RetrieveInclusionDependencies = RetrieveInclusionDependencies SessionId
                                   deriving (Binary, Generic)
data RetrievePlanForDatabaseContextExpr = RetrievePlanForDatabaseContextExpr SessionId DatabaseExpr
                                        deriving (Binary, Generic)
data RetrieveTransactionGraph = RetrieveTransactionGraph SessionId
                              deriving (Binary, Generic)
data RetrieveHeadTransactionUUID = RetrieveHeadTransactionUUID SessionId
                                 deriving (Binary, Generic)
data CreateSessionAtCommit = CreateSessionAtCommit UUID
                                    deriving (Binary, Generic)
data CreateSessionAtHead = CreateSessionAtHead HeadName
                                  deriving (Binary, Generic)
data CloseSession = CloseSession SessionId
                    deriving (Binary, Generic)

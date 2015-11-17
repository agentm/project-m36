{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module ProjectM36.Server.RemoteCallTypes where
import ProjectM36.Base
import ProjectM36.TransactionGraph
import GHC.Generics
import Data.Binary

data Login = Login
           deriving (Binary, Generic)
data Logout = Logout
            deriving (Binary, Generic)
data ExecuteRelationalExpr = ExecuteRelationalExpr RelationalExpr 
                           deriving (Binary, Generic)
data ExecuteDatabaseContextExpr = ExecuteDatabaseContextExpr DatabaseExpr
                                deriving (Binary, Generic)
data ExecuteGraphExpr = ExecuteGraphExpr TransactionGraphOperator 
                      deriving (Binary, Generic)
data ExecuteHeadName = ExecuteHeadName 
                     deriving (Binary, Generic)
data ExecuteTypeForRelationalExpr = ExecuteTypeForRelationalExpr RelationalExpr
                                  deriving (Binary, Generic)
data RetrieveInclusionDependencies = RetrieveInclusionDependencies
                                   deriving (Binary, Generic)
data RetrievePlanForDatabaseContextExpr = RetrievePlanForDatabaseContextExpr DatabaseExpr
                                        deriving (Binary, Generic)
data RetrieveTransactionGraph = RetrieveTransactionGraph
                              deriving (Binary, Generic)
data RetrieveHeadTransactionUUID = RetrieveHeadTransactionUUID
                                 deriving (Binary, Generic)

{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module ProjectM36.Daemon.RemoteCallTypes where
import ProjectM36.Base
import ProjectM36.TransactionGraph
import GHC.Generics
import Data.Binary

data RemoteExecution = Login |
                       Logout |
                       ExecuteRelationalExpr RelationalExpr |
                       ExecuteDatabaseContextExpr DatabaseExpr |
                       ExecuteGraphExpr TransactionGraphOperator |
                       ExecuteHeadName
                       deriving (Binary, Generic)

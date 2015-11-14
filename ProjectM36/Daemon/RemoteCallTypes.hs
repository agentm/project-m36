{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module ProjectM36.Daemon.RemoteCallTypes where
import ProjectM36.Base
import GHC.Generics
import Data.Binary

data RemoteExecution = Login |
                       Logout |
                       ExecuteRelationalExpr RelationalExpr deriving (Binary, Generic)

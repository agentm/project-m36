{-# LANGUAGE DeriveGeneric, DerivingStrategies, DerivingVia, DeriveAnyClass #-}
module ProjectM36.SQL.Delete where
import ProjectM36.SQL.Select
import Control.DeepSeq
import Codec.Winery
import GHC.Generics

data Delete = Delete { target :: TableName,
                       restriction :: RestrictionExpr
                     }
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryRecord Delete

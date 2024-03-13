{-# LANGUAGE DeriveGeneric, DerivingStrategies, DerivingVia, DeriveAnyClass #-}
module ProjectM36.SQL.DropTable where
import ProjectM36.SQL.Select
import Control.DeepSeq
import Codec.Winery
import GHC.Generics

data DropTable = DropTable
  { target :: TableName }
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryRecord DropTable


  

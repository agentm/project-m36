{-# LANGUAGE DeriveGeneric, DerivingStrategies, DerivingVia, DeriveAnyClass #-}
module ProjectM36.SQL.DBUpdate where
import ProjectM36.SQL.Update
import ProjectM36.SQL.Insert
import ProjectM36.SQL.Delete
import Control.DeepSeq
import Codec.Winery
import GHC.Generics

-- | represents any SQL expression which can change the current transaction state such as
data DBUpdate = UpdateUpdate Update |
                UpdateInsert Insert |
                UpdateDelete Delete
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant DBUpdate
                


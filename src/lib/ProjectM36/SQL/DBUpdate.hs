{-# LANGUAGE DeriveGeneric, DerivingVia, DeriveAnyClass #-}
module ProjectM36.SQL.DBUpdate where
import ProjectM36.SQL.Update
import ProjectM36.SQL.Insert
import ProjectM36.SQL.Delete
import ProjectM36.SQL.CreateTable
import ProjectM36.SQL.DropTable
import Control.DeepSeq
import Codec.Winery
import GHC.Generics

-- | represents any SQL expression which can change the current transaction state such as
data DBUpdate = UpdateUpdate Update |
                UpdateInsert Insert |
                UpdateDelete Delete |
                UpdateCreateTable CreateTable |
                UpdateDropTable DropTable
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant DBUpdate
                


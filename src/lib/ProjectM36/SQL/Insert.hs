{-# LANGUAGE DeriveGeneric, DerivingVia, DeriveAnyClass #-}
module ProjectM36.SQL.Insert where
import ProjectM36.SQL.Select
import ProjectM36.Serialise.Base ()
import Control.DeepSeq
import Codec.Winery
import GHC.Generics

data Insert = Insert
  { target :: TableName,
    targetColumns :: [UnqualifiedColumnName], -- because ProjectM36 does not support default values in columns, all columns from the underlying table must be included here
    source :: Query
  }
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryRecord Insert


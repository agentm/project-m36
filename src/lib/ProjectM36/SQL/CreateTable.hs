{-# LANGUAGE DeriveGeneric, DerivingStrategies, DerivingVia, DeriveAnyClass #-}
module ProjectM36.SQL.CreateTable where
import ProjectM36.SQL.Select
import Control.DeepSeq
import Codec.Winery
import GHC.Generics

data CreateTable = CreateTable
  { target :: TableName,
    targetColumns :: [(UnqualifiedColumnName, ColumnType, PerColumnConstraints)]
  }
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryRecord CreateTable

data ColumnType =
  IntegerColumnType |
  TextColumnType |
  BoolColumnType |
  DoubleColumnType |
  DateTimeColumnType
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant ColumnType

-- | Used to represent constraints which are defined next to a column name and type.
data PerColumnConstraints = PerColumnConstraints {
  notNullConstraint :: Bool
  }
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryVariant PerColumnConstraints

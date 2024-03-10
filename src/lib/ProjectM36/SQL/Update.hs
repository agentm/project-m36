{-# LANGUAGE DeriveGeneric, DerivingStrategies, DerivingVia, DeriveAnyClass #-}
module ProjectM36.SQL.Update where
import ProjectM36.SQL.Select
import ProjectM36.Serialise.Base ()
import Control.DeepSeq
import Codec.Winery
import GHC.Generics

data Update = Update
  { target :: TableName,
--    targetAlias :: Maybe TableAlias,
    --SET
    setColumns :: [(UnqualifiedColumnName, ScalarExpr)], --we don't support multi-column SET yet
    mRestriction :: Maybe RestrictionExpr
  }
            --RETURNING not yet supported
  deriving (Show, Eq, Generic, NFData)
  deriving Serialise via WineryRecord Update


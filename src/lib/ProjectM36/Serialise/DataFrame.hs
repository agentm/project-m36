{-# LANGUAGE DerivingVia, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProjectM36.Serialise.DataFrame where
import Codec.Winery
import ProjectM36.DataFrame
import ProjectM36.Serialise.Base ()
  
deriving via WineryVariant AttributeOrderExpr instance Serialise AttributeOrderExpr
deriving via WineryVariant AttributeOrder instance Serialise AttributeOrder
deriving via WineryVariant Order instance Serialise Order
deriving via WineryRecord DataFrame instance Serialise DataFrame
deriving via WineryVariant DataFrameTuple instance Serialise DataFrameTuple
deriving via WineryRecord DataFrameExpr instance Serialise DataFrameExpr

{-# LANGUAGE DerivingVia, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProjectM36.Serialise.DatabaseContextFunctionError where
import ProjectM36.Error
import Codec.Winery
import ProjectM36.Serialise.Base
import ProjectM36.DatabaseContextFunctionError

deriving via WineryVariant DatabaseContextFunctionError instance Serialise DatabaseContextFunctionError

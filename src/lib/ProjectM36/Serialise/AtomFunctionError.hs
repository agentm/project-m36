{-# LANGUAGE DerivingVia, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProjectM36.Serialise.AtomFunctionError where
import Codec.Winery
import ProjectM36.AtomFunctionError

deriving via WineryVariant AtomFunctionError instance Serialise AtomFunctionError

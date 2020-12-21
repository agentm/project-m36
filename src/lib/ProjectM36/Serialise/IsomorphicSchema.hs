{-# LANGUAGE StandaloneDeriving, DerivingVia, TypeApplications, TypeSynonymInstances, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProjectM36.Serialise.IsomorphicSchema where
import Codec.Winery
import ProjectM36.IsomorphicSchema

deriving via WineryVariant SchemaExpr instance Serialise SchemaExpr

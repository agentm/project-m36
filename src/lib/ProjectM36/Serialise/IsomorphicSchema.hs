{-# LANGUAGE StandaloneDeriving, DerivingVia, TypeSynonymInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProjectM36.Serialise.IsomorphicSchema where
import Codec.Winery
import ProjectM36.IsomorphicSchema

deriving via WineryVariant SchemaExpr instance Serialise SchemaExpr

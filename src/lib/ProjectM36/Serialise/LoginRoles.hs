{-# LANGUAGE StandaloneDeriving, DerivingVia, ScopedTypeVariables, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProjectM36.Serialise.LoginRoles where
import ProjectM36.LoginRoles
import Codec.Winery hiding (Schema)

deriving via WineryVariant AlterLoginRolesExpr instance Serialise AlterLoginRolesExpr
deriving via WineryVariant LoginRoleError instance Serialise LoginRoleError
deriving via WineryVariant SuccessResult instance Serialise SuccessResult

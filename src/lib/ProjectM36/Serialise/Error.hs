{-# LANGUAGE DerivingVia, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProjectM36.Serialise.Error where
import ProjectM36.Error
import Codec.Winery
import ProjectM36.Serialise.Base ()
import ProjectM36.Serialise.AtomFunctionError ()
import ProjectM36.DatabaseContext.Fields

deriving via WineryVariant RelationalError instance Serialise RelationalError
deriving via WineryVariant MergeError instance Serialise MergeError
deriving via WineryVariant DatabaseContextField instance Serialise DatabaseContextField
deriving via WineryVariant ScriptCompilationError instance Serialise ScriptCompilationError
deriving via WineryVariant PersistenceError instance Serialise PersistenceError
deriving via WineryVariant SchemaError instance Serialise SchemaError
deriving via WineryVariant ImportError' instance Serialise ImportError'
deriving via WineryVariant SQLError instance Serialise SQLError


{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ProjectM36.DatabaseContext.Fields where
import GHC.Generics
import Control.DeepSeq (NFData)

data DatabaseContextField =
  InclusionDependenciesField |
  RelationVariablesField |
  NotificationsField |
  AtomFunctionsField |
  DbcFunctionsField |
  TypeConstructorMappingField |
  RegisteredQueriesField
  deriving (Show, Eq, NFData, Generic)


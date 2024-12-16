-- | Add a marker to database context fields so that if the field is modified, then the record field is marked.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ProjectM36.ChangeTrackingDatabaseContext where
import ProjectM36.Base
import ProjectM36.DatabaseContext
import Control.DeepSeq (NFData)
import GHC.Generics
import Optics.Core

-- | Indicate whether a record field has been changed. This is used in the DatabaseContext ot mark fields which have been modified relative to the previous transactions' state as an optimization when persisting transactions on disk.
data ChangedMarker a = ChangedMarker a | NotChangedMarker a
  deriving (NFData, Generic)

unmarked :: ChangedMarker a -> a
unmarked (ChangedMarker x) = x
unmarked (NotChangedMarker x) = x

isChanged :: ChangedMarker a -> Bool
isChanged ChangedMarker{} = True
isChanged NotChangedMarker{} = False

data ChangeTrackingDatabaseContext = ChangeTrackingDatabaseContext {
  _ctinclusionDependencies :: ChangedMarker InclusionDependencies,
  _ctrelationVariables :: ChangedMarker RelationVariables,
  _ctatomFunctions :: ChangedMarker AtomFunctions,
  _ctdbcFunctions :: ChangedMarker DatabaseContextFunctions, 
  _ctnotifications :: ChangedMarker Notifications,
  _cttypeConstructorMapping :: ChangedMarker TypeConstructorMapping,
  _ctregisteredQueries :: ChangedMarker RegisteredQueries
  } deriving (NFData, Generic)

-- | Once this ChangeTrackingDatabaseContext has been commited, we can create a new one which is not marked changed with this function.
makeClean :: ChangeTrackingDatabaseContext -> ChangeTrackingDatabaseContext
makeClean ctdbc = ChangeTrackingDatabaseContext {
  _ctinclusionDependencies = NotChangedMarker (ctdbc ^. inclusionDependencies),
  _ctrelationVariables = NotChangedMarker (ctdbc ^. relationVariables),
  _ctatomFunctions = NotChangedMarker (ctdbc ^. atomFunctions),
  _ctdbcFunctions = NotChangedMarker (ctdbc ^. dbcFunctions),
  _ctnotifications = NotChangedMarker (ctdbc ^. notifications),
  _cttypeConstructorMapping = NotChangedMarker (ctdbc ^. typeConstructorMapping),
  _ctregisteredQueries = NotChangedMarker (ctdbc ^. registeredQueries)
  }

instance IsDatabaseContext ChangeTrackingDatabaseContext where
  inclusionDependencies = lens (unmarked . _ctinclusionDependencies) (\ctx v -> ctx { _ctinclusionDependencies = ChangedMarker v})
  relationVariables = lens (unmarked . _ctrelationVariables) (\ctx v -> ctx { _ctrelationVariables = ChangedMarker v })
  atomFunctions = lens (unmarked . _ctatomFunctions) (\ctx v -> ctx { _ctatomFunctions = ChangedMarker v })
  dbcFunctions = lens (unmarked . _ctdbcFunctions) (\ctx v -> ctx { _ctdbcFunctions = ChangedMarker v })
  notifications = lens (unmarked . _ctnotifications) (\ctx v -> ctx { _ctnotifications = ChangedMarker v })
  typeConstructorMapping = lens (unmarked . _cttypeConstructorMapping) (\ctx v -> ctx { _cttypeConstructorMapping = ChangedMarker v })
  registeredQueries = lens (unmarked . _ctregisteredQueries) (\ctx v -> ctx { _ctregisteredQueries = ChangedMarker v })

isDirty :: ChangeTrackingDatabaseContext -> Bool
isDirty dbc = isChanged (_ctinclusionDependencies dbc) ||
                isChanged (_ctrelationVariables dbc) ||
                isChanged (_ctnotifications dbc) ||
                isChanged (_ctatomFunctions dbc) ||
                isChanged (_ctdbcFunctions dbc) ||
                isChanged (_cttypeConstructorMapping dbc) ||
                isChanged (_ctregisteredQueries dbc)

-- | create a fresh change-tracking database context with no field markers indicating a change
fromDatabaseContext :: DatabaseContext -> ChangeTrackingDatabaseContext
fromDatabaseContext ctx =
  ChangeTrackingDatabaseContext
  {
    _ctinclusionDependencies = NotChangedMarker (ctx ^. inclusionDependencies),
    _ctrelationVariables = NotChangedMarker (ctx ^. relationVariables),
    _ctatomFunctions = NotChangedMarker (ctx ^. atomFunctions),
    _ctdbcFunctions = NotChangedMarker (ctx ^. dbcFunctions),
    _ctnotifications = NotChangedMarker (ctx ^. notifications),
    _cttypeConstructorMapping = NotChangedMarker (ctx ^. typeConstructorMapping),
    _ctregisteredQueries = NotChangedMarker (ctx ^. registeredQueries)
  }

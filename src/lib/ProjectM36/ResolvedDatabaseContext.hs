-- | The resolved database context has all transaction id references resolved and all data structures populated to eliminate the need to read the context from disk. In the future, we may wish to read just the parts of the database context which will definitely be used by the query. The ResolvedDatabaseContext marked each field as "changed" if has been "modified" from its original using lenses.

module ResolvedDatabaseContext where
import ProjectM36.DatabaseContext

data ChangedMarker a = ChangedMarker a | NotChangedMarker a
  deriving (NFData, Generic)

data ResolvedDatabaseContext = ResolvedDatabaseContext {
  _resinclusionDependencies :: ChangedMarker InclusionDependencies,
  _resrelationVariables :: ChangedMarker RelationVariables,
  _resatomFunctions :: ChangedMarker AtomFunctions,
  _resdbcFunctions :: ChangedMarker DatabaseContextFunctions, 
  _resnotifications :: ChangedMarker Notifications,
  _restypeConstructorMapping :: ChangedMarker TypeConstructorMapping,
  _resregisteredQueries :: ChangedMarker RegisteredQueries
  } deriving (NFData, Generic)

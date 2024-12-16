module ProjectM36.DatabaseContext.Persist where
import ProjectM36.Base
import ProjectM36.Base.Serialise

-- | Return a database context with records marked as unchanged, if necessary. This is useful for saving disk space. When deserializing this structure, we read all the necessary transaction data to create a concrete database context so that our database context evaluation functions don't need to read from disk.
diffDatabaseContext :: DatabaseContext -> GraphRefDatabaseContext -> GraphRefDatabaseContext
diffDatabaseContext newContext oldContext =
  GraphRefDatabaseContext {
  inclusionDependencies = diffIncDeps,
  relationVariables = diffRelVars,
  atomFunctions = diffAtomFuncs,
  dbcFunctions = diffDBCFuncs,
  notifications = diffNotifications,
  typeConstructorMapping = diffTypeCons,
  registeredQueries = diffRegQueries
  }
  where
    updater :: forall a. ChangedMarker a -> a -> GraphRefOrUpdate a
    updater x previousVal =
      if isChanged x then
        UpdateValue (unmarked x)
      else
        previousVal
    diffIncDeps = updater (inclusionDependencies newContext) (inclusionDependencies oldContext)

readGraphRefDatabaseContext :: FilePath -> IO GraphRefDatabaseContext
readGraphRefDatabaseContext dbcRoot = do
  -- in the base case when nothing in the database context has changed, we merely record a single file with
  if nothingChanged
  pure $ GraphRefDatabaseContext {
    
                                 }


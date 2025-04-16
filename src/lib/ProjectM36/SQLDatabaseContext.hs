-- | Enables SQL-equivalent features such as NULL types in the database in addition to Project:M36 basic functions.
module ProjectM36.SQLDatabaseContext where
import ProjectM36.DatabaseContext.Basic
import ProjectM36.DataTypes.SQL.Null
import ProjectM36.DatabaseContext.Types
import Data.Functor.Identity

sqlDatabaseContext :: ResolvedDatabaseContext
sqlDatabaseContext =
  basicDatabaseContext {
      atomFunctions =
      atomFunctions basicDatabaseContext <> Identity nullAtomFunctions,
      typeConstructorMapping =
      typeConstructorMapping basicDatabaseContext <> Identity nullTypeConstructorMapping
      }


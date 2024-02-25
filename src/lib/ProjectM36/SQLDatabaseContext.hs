-- | Enables SQL-equivalent features such as NULL types in the database in addition to Project:M36 basic functions.
module ProjectM36.SQLDatabaseContext where
import ProjectM36.Base
import ProjectM36.DatabaseContext
import ProjectM36.DataTypes.SQL.Null

sqlDatabaseContext :: DatabaseContext
sqlDatabaseContext = basicDatabaseContext { atomFunctions =
                                            atomFunctions basicDatabaseContext <> nullAtomFunctions,
                                            typeConstructorMapping =
                                            typeConstructorMapping basicDatabaseContext <> nullTypeConstructorMapping
                                            }


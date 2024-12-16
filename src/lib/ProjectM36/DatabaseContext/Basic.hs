module ProjectM36.DatabaseContext.Basic where
import ProjectM36.DatabaseContext
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.DataTypes.Basic
import ProjectM36.AtomFunctions.Basic
import ProjectM36.DatabaseContextFunctions.Basic
import qualified Data.Map as M

basicDatabaseContext :: DatabaseContext
basicDatabaseContext = DatabaseContext { _inclusionDependencies = M.empty,
                                         _relationVariables = M.fromList [("true", ExistingRelation relationTrue),
                                                                         ("false", ExistingRelation relationFalse)],
                                         _atomFunctions = basicAtomFunctions,
                                         _dbcFunctions = basicDatabaseContextFunctions,
                                         _notifications = M.empty,
                                         _typeConstructorMapping = basicTypeConstructorMapping,
                                         _registeredQueries = M.singleton "booleans" (Union (RelationVariable "true" ()) (RelationVariable "false" ()))
                                         }

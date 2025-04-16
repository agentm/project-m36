module ProjectM36.DatabaseContext.Basic where
import ProjectM36.DatabaseContext.Types
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.DataTypes.Basic
import ProjectM36.AtomFunctions.Basic
import ProjectM36.DatabaseContextFunctions.Basic
import qualified Data.Map as M
import Data.Functor.Identity

basicDatabaseContext :: ResolvedDatabaseContext
basicDatabaseContext = DatabaseContext { inclusionDependencies = Identity mempty,
                                         relationVariables =
                                           Identity $ M.fromList [("true", ExistingRelation relationTrue),
                                                                      ("false", ExistingRelation relationFalse)],
                                         atomFunctions = Identity basicAtomFunctions,
                                         dbcFunctions = Identity basicDatabaseContextFunctions,
                                         notifications = Identity mempty,
                                         typeConstructorMapping = Identity basicTypeConstructorMapping,
                                         registeredQueries = Identity $ M.singleton "booleans" (Union (RelationVariable "true" ()) (RelationVariable "false" ()))
                                         }

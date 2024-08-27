{-# LANGUAGE RankNTypes #-}
module ProjectM36.DatabaseContext where
import ProjectM36.Base
import Control.Monad (void)
import qualified Data.Map as M
import qualified Data.HashSet as HS
import ProjectM36.DataTypes.Basic
import ProjectM36.AtomFunctions.Basic
import ProjectM36.Relation
import ProjectM36.DatabaseContextFunction

empty :: DatabaseContext
empty = DatabaseContext { inclusionDependencies = M.empty, 
                          relationVariables = M.empty, 
                          notifications = M.empty,
                          atomFunctions = HS.empty,
                          dbcFunctions = HS.empty,
                          typeConstructorMapping = mempty,
                          registeredQueries = mempty }

  
-- | Remove TransactionId markers on GraphRefRelationalExpr
stripGraphRefRelationalExpr :: GraphRefRelationalExpr -> RelationalExpr
stripGraphRefRelationalExpr = void
        
-- | convert an existing database context into its constituent expression.   
databaseContextAsDatabaseContextExpr :: DatabaseContext -> DatabaseContextExpr
databaseContextAsDatabaseContextExpr context = MultipleExpr $ relVarsExprs ++ incDepsExprs ++ funcsExprs
  where
    relVarsExprs = map (\(name, rel) -> Assign name (stripGraphRefRelationalExpr rel)) (M.toList (relationVariables context))
    incDepsExprs :: [DatabaseContextExpr]
    incDepsExprs = map (uncurry AddInclusionDependency) (M.toList (inclusionDependencies context))
    funcsExprs = [] -- map (\func -> ) (HS.toList funcs) -- there are no databaseExprs to add atom functions yet-}

basicDatabaseContext :: DatabaseContext
basicDatabaseContext = DatabaseContext { inclusionDependencies = M.empty,
                                         relationVariables = M.fromList [("true", ExistingRelation relationTrue),
                                                                         ("false", ExistingRelation relationFalse)],
                                         atomFunctions = basicAtomFunctions,
                                         dbcFunctions = basicDatabaseContextFunctions,
                                         notifications = M.empty,
                                         typeConstructorMapping = basicTypeConstructorMapping,
                                         registeredQueries = M.singleton "booleans" (Union (RelationVariable "true" ()) (RelationVariable "false" ()))
                                         }

someDatabaseContextExprs :: [DatabaseContextExpr] -> DatabaseContextExpr
someDatabaseContextExprs [s] = s
someDatabaseContextExprs (s:ss) = MultipleExpr (s:ss)
someDatabaseContextExprs [] = NoOperation



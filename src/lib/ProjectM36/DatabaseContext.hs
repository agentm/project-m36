{-# LANGUAGE RankNTypes, DeriveGeneric, DeriveAnyClass, MultiParamTypeClasses, ExistentialQuantification #-}
module ProjectM36.DatabaseContext where
import ProjectM36.Base
import Control.DeepSeq (NFData)
import ProjectM36.DatabaseContext.Types
import ProjectM36.TransactionGraph.Types
import Control.Monad (void)
import qualified Data.Map as M
import qualified Data.HashSet as HS
import ProjectM36.DatabaseContextFunctionError
import ProjectM36.Error
--import ProjectM36.DataTypes.Basic
--import ProjectM36.AtomFunctions.Basic
--import ProjectM36.Relation
--import ProjectM36.DatabaseContextFunction
import Optics.Core
import Data.Functor.Identity

-- | The DatabaseContext is a snapshot of a database's evolving state and contains everything a database client can change over time.
-- I spent some time thinking about whether the VirtualDatabaseContext/Schema and DatabaseContext data constructors should be the same constructor, but that would allow relation variables to be created in a "virtual" context which would appear to defeat the isomorphisms of the contexts. It should be possible to switch to an alternative schema to view the same equivalent information without information loss. However, allowing all contexts to reference another context while maintaining its own relation variables, new types, etc. could be interesting from a security perspective. For example, if a user creates a new relvar in a virtual context, then does it necessarily appear in all linked contexts? After deliberation, I think the relvar should appear in *all* linked contexts to retain the isomorphic properties, even when the isomorphism is for a subset of the context. This hints that the IsoMorphs should allow for "fall-through"; that is, when a relvar is not defined in the virtual context (for morphing), then the lookup should fall through to the underlying context.
-- | Used for read-only database context within a transaction.

-- | Indicate whether a record field has been changed in this transaction. This is used in the DatabaseContext ot mark fields which have been modified relative to the previous transactions' state as an optimization when persisting transactions on disk.

empty :: DatabaseContext
empty = DatabaseContext { inclusionDependencies = mempty, 
                          relationVariables = mempty, 
                          notifications = mempty,
                          atomFunctions = mempty,
                          dbcFunctions = mempty,
                          typeConstructorMapping = mempty,
                          registeredQueries = mempty }
  
-- | Remove TransactionId markers on GraphRefRelationalExpr
stripGraphRefRelationalExpr :: GraphRefRelationalExpr -> RelationalExpr
stripGraphRefRelationalExpr = void

-- | convert an existing database context into its constituent expression.   
databaseContextAsDatabaseContextExpr :: DatabaseContext -> TransactionGraph -> DatabaseContextExpr
databaseContextAsDatabaseContextExpr context graph = MultipleExpr $ relVarsExprs ++ incDepsExprs ++ funcsExprs
  where
    relVarsExprs = map (\(name, rel) -> Assign name (stripGraphRefRelationalExpr rel)) (M.toList (runIdentity (context & relationVariables)))
    incDepsExprs :: [DatabaseContextExpr]
    incDepsExprs = map (uncurry AddInclusionDependency) (M.toList (runIdentity (context & inclusionDependencies)))
    funcsExprs = [] -- map (\func -> ) (HS.toList funcs) -- there are no databaseExprs to add atom functions yet-}

someDatabaseContextExprs :: [DatabaseContextExpr] -> DatabaseContextExpr
someDatabaseContextExprs [s] = s
someDatabaseContextExprs (s:ss) = MultipleExpr (s:ss)
someDatabaseContextExprs [] = NoOperation

-- | Strip change tracking information for read-only queries
{-
asDatabaseContext :: IsDatabaseContext ctx => ctx -> DatabaseContext
asDatabaseContext ctx =
  DatabaseContext { _inclusionDependencies = ctx ^. inclusionDependencies,
                    _relationVariables = ctx ^. relationVariables,
                    _atomFunctions = ctx ^. atomFunctions,
                    _dbcFunctions = ctx ^. dbcFunctions,
                    _notifications = ctx ^. notifications,
                    _typeConstructorMapping = ctx ^. typeConstructorMapping,
                    _registeredQueries =  ctx ^. registeredQueries
                  }

-}

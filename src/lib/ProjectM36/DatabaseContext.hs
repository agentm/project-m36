{-# LANGUAGE RankNTypes, DeriveGeneric, DeriveAnyClass, MultiParamTypeClasses, ExistentialQuantification #-}
module ProjectM36.DatabaseContext where
import ProjectM36.Base
import ProjectM36.DatabaseContext.Types as DBT
import ProjectM36.TransactionGraph.Types
import qualified Data.Map as M
import Control.Monad (void)
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
empty = DatabaseContext { inclusionDependencies = emptyValue, 
                          relationVariables = emptyValue, 
                          notifications = emptyValue,
                          atomFunctions = emptyValue,
                          dbcFunctions = emptyValue,
                          typeConstructorMapping = emptyValue,
                          registeredQueries = emptyValue }
  
-- | Remove TransactionId markers on GraphRefRelationalExpr
stripGraphRefRelationalExpr :: GraphRefRelationalExpr -> RelationalExpr
stripGraphRefRelationalExpr = void

-- | If the database context has any values which do *not* reference previous transactions, it must be new data.
isUpdated :: DatabaseContext -> Bool
isUpdated ctx = or [DBT.valueIsUpdated (inclusionDependencies ctx),
                     DBT.valueIsUpdated (relationVariables ctx),
                     DBT.valueIsUpdated (atomFunctions ctx),
                     DBT.valueIsUpdated (dbcFunctions ctx),
                     DBT.valueIsUpdated (notifications ctx),
                     DBT.valueIsUpdated (typeConstructorMapping ctx),
                     DBT.valueIsUpdated (registeredQueries ctx)]

someDatabaseContextExprs :: [DatabaseContextExpr] -> DatabaseContextExpr
someDatabaseContextExprs [s] = s
someDatabaseContextExprs (s:ss) = MultipleExpr (s:ss)
someDatabaseContextExprs [] = NoOperation

-- | The "fresh" database context is created after a commit, so all values will refer to previous transactions.
freshDatabaseContext :: TransactionId -> DatabaseContext -> DatabaseContext
freshDatabaseContext previousTransactionId ctx =
  DatabaseContext { inclusionDependencies = freshen (inclusionDependencies ctx),
                    relationVariables = freshen (relationVariables ctx),
                    atomFunctions = freshen (atomFunctions ctx),
                    dbcFunctions = freshen (dbcFunctions ctx),
                    notifications = freshen (notifications ctx),
                    typeConstructorMapping = freshen (typeConstructorMapping ctx),
                    registeredQueries = freshen (registeredQueries ctx)
                  }
  where
    freshen :: forall a. ValueMarker a -> ValueMarker a
    freshen (ValueMarker _) = NotChangedSinceMarker previousTransactionId
    freshen m@NotChangedSinceMarker{} = m
                    

module ProjectM36.DatabaseContext where
import ProjectM36.Base
import qualified Data.Map as M
import qualified Data.HashSet as HS
import ProjectM36.DataTypes.Basic
import ProjectM36.AtomFunctions.Basic
import ProjectM36.Relation
import ProjectM36.TupleSet

empty :: DatabaseContext
empty = DatabaseContext { inclusionDependencies = M.empty, 
                          relationVariables = M.empty, 
                          notifications = M.empty,
                          atomFunctions = HS.empty,
                          typeConstructorMapping = [] }
        
-- | convert an existing database context into its constituent expression.   
databaseContextAsDatabaseContextExpr :: DatabaseContext -> DatabaseContextExpr
databaseContextAsDatabaseContextExpr context = MultipleExpr $ relVarsExprs ++ incDepsExprs ++ funcsExprs
  where
    relVarsExprs = map (\(name, rel) -> Assign name (ExistingRelation rel)) (M.toList (relationVariables context))
    incDepsExprs :: [DatabaseContextExpr]
    incDepsExprs = map (\(name, dep) -> AddInclusionDependency name dep) (M.toList (inclusionDependencies context))
    funcsExprs = [] -- map (\func -> ) (HS.toList funcs) -- there are no databaseExprs to add atom functions yet

basicDatabaseContext :: DatabaseContext
basicDatabaseContext = DatabaseContext { inclusionDependencies = M.empty,
                                         relationVariables = M.fromList [("true", relationTrue),
                                                                         ("false", relationFalse)],
                                         atomFunctions = basicAtomFunctions,
                                         notifications = M.empty,
                                         typeConstructorMapping = basicTypeConstructorMapping
                                         }

--returns a database context with all tuples removed
--this is useful for type checking and optimization
contextWithEmptyTupleSets :: DatabaseContext -> DatabaseContext
contextWithEmptyTupleSets contextIn = contextIn { relationVariables = relVars }
  where
    relVars = M.map (\rel -> Relation (attributes rel) emptyTupleSet) (relationVariables contextIn)

thunkless :: DatabaseContext -> Bool
thunkless context = case thunks context of
  NoOperation -> True
  MultipleExpr multi -> length (filter ((/=) NoOperation) multi) == 0
  _ -> False

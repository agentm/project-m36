module ProjectM36.DatabaseContext where
import ProjectM36.Base
import qualified Data.Map as M
import qualified Data.HashSet as HS

empty :: DatabaseContext
empty = DatabaseContext { inclusionDependencies = M.empty, 
                          relationVariables = M.empty, 
                          notifications = M.empty,
                          atomFunctions = HS.empty }
        
-- | convert an existing database context into its constituent expression.   
databaseContextAsDatabaseContextExpr :: DatabaseContext -> DatabaseExpr
databaseContextAsDatabaseContextExpr context = MultipleExpr $ incDepsExprs ++ relVarsExprs ++ funcsExprs
  where
    relVarsExprs = map (\(name, rel) -> Assign name (ExistingRelation rel)) (M.toList (relationVariables context))
    incDepsExprs :: [DatabaseExpr]
    incDepsExprs = map (\(name, dep) -> AddInclusionDependency name dep) (M.toList (inclusionDependencies context))
    funcsExprs = [] -- map (\func -> ) (HS.toList funcs) -- there are no databaseExprs to add atom functions yet


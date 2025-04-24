module ProjectM36.DatabaseContextExpr where
import ProjectM36.DatabaseContext.Types
import ProjectM36.DatabaseContext
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.RelationalExpression
import ProjectM36.TransactionGraph.Types
import qualified Data.Map as M
import Data.Functor.Identity

resolvedDatabaseContextAsDatabaseContextExpr :: ResolvedDatabaseContext -> DatabaseContextExpr
resolvedDatabaseContextAsDatabaseContextExpr context = do
  let relVarsExprs = map (\(name, rel) -> Assign name (stripGraphRefRelationalExpr rel)) (M.toList relVars)
      relVars = runIdentity (relationVariables context)
      incDeps = runIdentity (inclusionDependencies context)
      incDepsExprs = map (uncurry AddInclusionDependency) (M.toList incDeps)
      funcsExprs = []
  MultipleExpr $ relVarsExprs ++ incDepsExprs ++ funcsExprs
  
-- | convert an existing database context into its constituent expression.   
databaseContextAsDatabaseContextExpr :: DatabaseContext -> TransactionGraph -> Either RelationalError DatabaseContextExpr
databaseContextAsDatabaseContextExpr context graph = do
  relVars <- resolveDBC' graph context relationVariables
  incDeps <- resolveDBC' graph context inclusionDependencies
  let relVarsExprs = map (\(name, rel) -> Assign name (stripGraphRefRelationalExpr rel)) (M.toList relVars)
      incDepsExprs = map (uncurry AddInclusionDependency) (M.toList incDeps)
      funcsExprs = []
  pure $ MultipleExpr $ relVarsExprs ++ incDepsExprs ++ funcsExprs



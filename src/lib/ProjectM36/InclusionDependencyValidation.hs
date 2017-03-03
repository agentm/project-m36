module ProjectM36.InclusionDependencyValidation where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.RelationalExpression
import ProjectM36.RelationalExpressionState
import ProjectM36.Relation
import ProjectM36.StaticOptimizer.InclusionDependencies
import qualified Data.Map as M
import Control.Monad.State

--run verification on all constraints
checkConstraints :: DatabaseContextExpr -> DatabaseContext -> Maybe RelationalError
checkConstraints expr context = case optimizedIncDepMap of
  Left err -> Just err
  Right filteredIncDepMap -> case failures filteredIncDepMap of
    [] -> Nothing
    err:[] -> Just err
    list -> Just (MultipleErrors list)
  where
    optimizedIncDepMap = filterInclusionDependenciesForValidation expr deps
    failures incDeps = M.elems $ M.mapMaybeWithKey checkIncDep incDeps
    deps = inclusionDependencies context
    eval expr' = evalState (evalRelationalExpr expr') (RelationalExprStateElems context)
    checkIncDep depName (InclusionDependency subsetExpr supersetExpr) = do
      let checkExpr = Equals supersetExpr (Union subsetExpr supersetExpr)
      case eval checkExpr of
        Left err -> Just err
        Right resultRel -> if resultRel == relationTrue then
                                   Nothing
                                else 
                                  Just $ InclusionDependencyCheckError depName

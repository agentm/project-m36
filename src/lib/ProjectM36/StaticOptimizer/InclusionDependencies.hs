module ProjectM36.StaticOptimizer.InclusionDependencies where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.RelationalExpression
import qualified Data.Map as M
import qualified Data.Set as S

inclusionDependencyValidation :: DatabaseContextExpr -> InclusionDependency -> Validation

inclusionDependencyValidation NoOperation _ = NoValidationNeeded

-- a new relvar cannot possibly part of any constraint
inclusionDependencyValidation (Define _ _)  _ = NoValidationNeeded

-- if there are any foreign keys pointing to the relvar, then
inclusionDependencyValidation (Undefine name) incDep = if 
  _nameInIncDep name incDep then
    Violated
  else
    NoValidationNeeded
    
inclusionDependencyValidation (Assign name _) incDep = _nameInIncDepValidation name incDep
                                                              
inclusionDependencyValidation (Insert name _) incDep = _nameInIncDepValidation name incDep

inclusionDependencyValidation (Delete name _) incDep =  _nameInIncDepValidation name incDep

inclusionDependencyValidation (Update name _ _) incDep = _nameInIncDepValidation name incDep

--inc deps can't violate another inc dep- there is no change in relvars
inclusionDependencyValidation (AddInclusionDependency _ _) _ = NoValidationNeeded

inclusionDependencyValidation (RemoveInclusionDependency _) _ = NoValidationNeeded

inclusionDependencyValidation (AddNotification _ _ _) _ = NoValidationNeeded

inclusionDependencyValidation (RemoveNotification _) _ = NoValidationNeeded

inclusionDependencyValidation (AddTypeConstructor _ _) _ = NoValidationNeeded

inclusionDependencyValidation (RemoveTypeConstructor _) _ = NoValidationNeeded

inclusionDependencyValidation (RemoveAtomFunction _) _ = NoValidationNeeded

inclusionDependencyValidation (MultipleExpr exprs) incDep = checkValidation validationSet
  where
    validationSet = S.fromList (map (flip inclusionDependencyValidation incDep) exprs)

_nameInIncDep :: RelVarName -> InclusionDependency -> Bool
_nameInIncDep name incDep = S.member name (relvarReferences incDep)

_nameInIncDepValidation :: RelVarName -> InclusionDependency -> Validation
_nameInIncDepValidation name incDep = if _nameInIncDep name incDep then
                                        ValidationNeeded
                                      else
                                        NoValidationNeeded
                        
-- | Return what validation is needed after looking at optimizations over multiple expressions.
checkValidation :: S.Set Validation -> Validation
checkValidation vSet = if S.member Violated vSet then
                         Violated
                       else if S.member ValidationNeeded vSet || S.null vSet then
                              ValidationNeeded
                            else
                              NoValidationNeeded
                              
-- | Potentially optimize away constraints which need not be checked- they are tautologically valid because the update cannot possibly violate the constraint.
data Validation = NoValidationNeeded |
                  ValidationNeeded |
                  Violated --sometimes, we statically know that the inclusion dependency will be violated
                  deriving (Eq, Show, Ord)

filterInclusionDependenciesForValidation :: DatabaseContextExpr -> InclusionDependencies -> Either RelationalError InclusionDependencies
filterInclusionDependenciesForValidation context incDeps = if length errors > 0 then
                                                             Left (MultipleErrors errors)
                                                           else
                                                             Right filteredIncDeps
  where 
    filteredIncDeps = M.filterWithKey (\incDepName _ -> M.member incDepName needValidation) incDeps
    violated = M.filter ((==) Violated) validationMap
    errors = map InclusionDependencyCheckError (M.keys violated)
    validationMap = M.map (inclusionDependencyValidation context) incDeps
    needValidation = M.filter ((==) ValidationNeeded) validationMap
                              
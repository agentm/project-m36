module ProjectM36.FunctionalDependency where
import ProjectM36.Base hiding (GraphRefRelationalExpr)
import ProjectM36.AttributeNamesBase
import qualified Data.Set as S

data FunctionalDependency = FunctionalDependency AttributeNames AttributeNames GraphRefRelationalExpr

--(s{city} group ({city} as x) : {z:=count(@x)}) {z}
-- as defined in Relational Algebra and All That Jazz page 21
inclusionDependenciesForFunctionalDependency :: FunctionalDependency -> (InclusionDependency, InclusionDependency)
inclusionDependenciesForFunctionalDependency (FunctionalDependency attrNamesSource attrNamesDependent relExpr) = (
  InclusionDependency countSource countDep,            
  InclusionDependency countDep countSource)
  where
    countDep = relExprCount relExpr (S.union attrNamesSource attrNamesDependent)
    countSource = relExprCount relExpr attrNamesSource
    projectZName = Project (S.singleton "z")
    zCount = FunctionAtomExpr "count" [AttributeAtomExpr "x"] UncommittedContextMarker
    extendZName = Extend (AttributeExtendTupleExpr "z" zCount)
    relExprCount expr projectionAttrNames = projectZName (extendZName
       (Group projectionAttrNames "x" (Project projectionAttrNames expr)))

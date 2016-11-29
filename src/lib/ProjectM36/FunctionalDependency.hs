module ProjectM36.FunctionalDependency where
import ProjectM36.Base 
import qualified Data.Set as S

data FunctionalDependency = FunctionalDependency AttributeNames AttributeNames RelationalExpr

--(s{city} group ({city} as x) : {z:=count(@x)}) {z}
-- as defined in Relational Algebra and All That Jazz page 21
inclusionDependenciesForFunctionalDependency :: FunctionalDependency -> (InclusionDependency, InclusionDependency)
inclusionDependenciesForFunctionalDependency (FunctionalDependency attrNamesSource attrNamesDependent relExpr) = (InclusionDependency countSource countDep,
                                                                                                                InclusionDependency countDep countSource)
  where
    countSource = relExprCount relExpr attrNamesSource
    countDep = relExprCount relExpr (UnionAttributeNames attrNamesSource attrNamesDependent)
    projectZName = Project (AttributeNames (S.singleton "z"))
    zCount = FunctionAtomExpr "count" [AttributeAtomExpr "x"] ()
    extendZName = Extend (AttributeExtendTupleExpr "z" zCount)
    relExprCount expr projectionAttrNames = projectZName (extendZName
       (Group projectionAttrNames "x" (Project projectionAttrNames expr)))
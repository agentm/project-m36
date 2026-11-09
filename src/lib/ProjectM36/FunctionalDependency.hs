module ProjectM36.FunctionalDependency where
import ProjectM36.Base
import qualified Data.Set as S

data FunctionalDependencyBase attrNames a = FunctionalDependency attrNames attrNames (RelationalExprBase attrNames a)

--(s{city} group ({city} as x) : {z:=count(@x)}) {z}
-- as defined in Relational Algebra and All That Jazz page 21
inclusionDependenciesForFunctionalDependency ::
  (Semigroup attrNames,
   DefaultTransactionMarker a,
   BareAttributeNames attrNames) =>
  FunctionalDependencyBase attrNames a ->
  (InclusionDependencyBase attrNames a,
   InclusionDependencyBase attrNames a)
inclusionDependenciesForFunctionalDependency (FunctionalDependency attrNamesSource attrNamesDependent relExpr) = (
  InclusionDependency countSource countDep,            
  InclusionDependency countDep countSource)
  where
    countDep = relExprCount relExpr (attrNamesSource <> attrNamesDependent)
    countSource = relExprCount relExpr attrNamesSource
--    projectZName :: BareAttributeNames attrNames => RelationalExprBase attrNames a -> RelationalExprBase attrNames a
    projectZName = Project (bareAttributeNames (S.singleton "z"))
--    zCount :: DefaultTransactionMarker a => AtomExprBase attrNames a
    zCount = FunctionAtomExpr "count" [AttributeAtomExpr "x"] defaultMarker
--    extendZName :: DefaultTransactionMarker a => RelationalExprBase attrNames a -> RelationalExprBase attrNames a
    extendZName = Extend (AttributeExtendTupleExpr "z" zCount)
--    relExprCount :: (DefaultTransactionMarker a, BareAttributeNames attrNames) => RelationalExprBase attrNames a -> attrNames -> RelationalExprBase attrNames a
    relExprCount expr projectionAttrNames =
      let xattr = "x"
      in
      projectZName (extendZName
       (Group projectionAttrNames xattr (Project projectionAttrNames expr)))

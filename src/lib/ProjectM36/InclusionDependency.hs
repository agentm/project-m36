module ProjectM36.InclusionDependency where
import ProjectM36.Base (Relation, InclusionDependencies, InclusionDependency(..), RelVarName, Attribute(..), AtomType(..), Atom(..), RelationalExprBase(..), RestrictionPredicateExprBase(..), GraphRefTransactionMarker(..))
import ProjectM36.AttributeNamesBase (GraphRefAtomExpr)
import ProjectM36.Attribute
import ProjectM36.Error
import ProjectM36.Relation
import qualified Data.Map as M

inclusionDependenciesAsRelation :: InclusionDependencies -> Either RelationalError Relation
inclusionDependenciesAsRelation incDeps =
  mkRelationFromList attrs (map incDepAsAtoms (M.toList incDeps))
  where
    attrs = attributesFromList [Attribute "name" TextAtomType,
                                Attribute "sub" RelationalExprAtomType,
                                Attribute "super" RelationalExprAtomType
                                ]
    incDepAsAtoms (name, InclusionDependency exprA exprB) = [TextAtom name,
                                                             RelationalExprAtom exprA,
                                                             RelationalExprAtom exprB]

-- validate that the given AtomExpr is true for an relvar
inclusionDependencyForAtomExpr :: RelVarName -> GraphRefAtomExpr -> InclusionDependency
inclusionDependencyForAtomExpr rvname atomExpr =
  InclusionDependency
  (NotEquals (ExistingRelation relationTrue)
    (Project mempty (Restrict check (RelationVariable rvname UncommittedContextMarker)))
  )
  (ExistingRelation relationFalse)
  where
    check = AtomExprPredicate atomExpr


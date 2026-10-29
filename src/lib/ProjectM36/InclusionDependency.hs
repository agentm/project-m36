module ProjectM36.InclusionDependency where
import ProjectM36.Base (Relation, InclusionDependencies, InclusionDependencyBase(..), RelVarName, Attribute(..), AtomType(..), Atom(..), RelationalExprBase(..), RestrictionPredicateExprBase(..), BareAttributeNames, DefaultTransactionMarker, bareAttributeNames, defaultMarker, AtomExprBase(..))
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
inclusionDependencyForAtomExpr :: BareAttributeNames attrNames => RelVarName -> AtomExprBase attrNames () -> InclusionDependencyBase attrNames ()
inclusionDependencyForAtomExpr rvname atomExpr =
  InclusionDependency
  (NotEquals (ExistingRelation relationTrue)
    (Project (bareAttributeNames mempty) (Restrict check (RelationVariable rvname () )))
  )
  (ExistingRelation relationFalse)
  where
    check = AtomExprPredicate atomExpr


module ProjectM36.InclusionDependency where
import ProjectM36.Base
import ProjectM36.Attribute
import ProjectM36.Error
import ProjectM36.Relation
import qualified Data.Map as M
import qualified Data.Text as T
import ProjectM36.PrettyPrinter
import Data.Text.Prettyprint.Doc 

inclusionDependenciesAsRelation :: InclusionDependencies -> Either RelationalError Relation
inclusionDependenciesAsRelation incDeps =
  mkRelationFromList attrs (map incDepAsAtoms (M.toList incDeps))
  where
    attrs = attributesFromList [Attribute "name" TextAtomType,
                                Attribute "sub" TextAtomType,
                                Attribute "super" TextAtomType
                                ]
    incDepAsAtoms (name, InclusionDependency exprA exprB) = [TextAtom name,
                                                               TextAtom (T.pack (show (pretty exprA))),
                                                               TextAtom (T.pack (show (pretty exprB)))]
  

module ProjectM36.RegisteredQuery where
import ProjectM36.Base
import ProjectM36.Attribute
import ProjectM36.Error
import ProjectM36.IsomorphicSchema
import ProjectM36.Relation
import qualified Data.Map as M

registeredQueriesAsRelationInSchema :: Schema -> RegisteredQueries -> Either RelationalError Relation
registeredQueriesAsRelationInSchema schema regQs = do
  tups <- mapM regQToTuple (M.toList regQs)
  mkRelationFromList attrs tups
  where
    attrs = attributesFromList [Attribute "name" TextAtomType,
                                Attribute "expr" RelationalExprAtomType]
    regQToTuple (qname, qexpr) = do
      qexpr' <- processRelationalExprInSchema schema qexpr
      pure [TextAtom qname, RelationalExprAtom qexpr']
  

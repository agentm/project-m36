module ProjectM36.GraphRefRelationalExpr where
--evaluate relational expressions across the entire transaction graph to support cross-transaction referencing
import ProjectM36.Base
import ProjectM36.Error
import qualified ProjectM36.Attribute as A
import Control.Monad.Trans.Except

-- core evaluation function- all other relational expr forms compile to GraphRefRelationalExpr
eval :: GraphRefRelationalExpr -> TransactionGraph -> Either RelationalError Relation
eval = undefined
{-eval (MakeRelationFromExprs mAttrExprs tupleExprs) graph = do
  mAttrs <- case mAttrExprs of
              Just _ ->
                Just . A.attributesFromList <$> mapM (evalAttrExpr tConss) (fromMaybe [] mAttrExprs)
              Nothing -> pure Nothing
  tuples <- mapM (evalTupleExpr mAttrs) tupleExprs
  let attrs = fromMaybe firstTupleAttrs mAttrs
      firstTupleAttrs = if null tuples then A.emptyAttributes else tupleAttributes (head tuples)
  mkRelation attrs (RelationTupleSet tuples)
                

-}

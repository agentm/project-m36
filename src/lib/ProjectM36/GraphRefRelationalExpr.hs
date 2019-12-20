module ProjectM36.GraphRefRelationalExpr where
--evaluate relational expressions across the entire transaction graph to support cross-transaction referencing
import ProjectM36.Base
import ProjectM36.Error
import qualified Data.Set as S

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

data SingularTransactionRef = SingularTransactionRef TransactionId |
                              MultipleTransactionsRef |
                              NoTransactionsRef
                              deriving (Eq, Show)
  
-- | return `Just transid` if this GraphRefRelationalExpr refers to just one transaction in the graph. This is useful for determining if certain optimizations can apply.
singularTransaction :: GraphRefRelationalExpr -> SingularTransactionRef
singularTransaction expr =
  if S.null transSet then
    NoTransactionsRef
  else if S.size transSet == 1 then
    SingularTransactionRef (head (S.toList transSet))
    else
    MultipleTransactionsRef
  where
    transSet = foldr (\transId acc -> S.insert transId acc) S.empty expr

-- | Return True if two 'GraphRefRelationalExpr's both refer exclusively to the same transaction (or none at all).
inSameTransaction :: GraphRefRelationalExpr -> GraphRefRelationalExpr -> Maybe TransactionId
inSameTransaction exprA exprB = case (stA, stB) of
  (SingularTransactionRef tA, SingularTransactionRef tB) | tA == tB -> Just tA
  _ -> Nothing
  where stA = singularTransaction exprA
        stB = singularTransaction exprB

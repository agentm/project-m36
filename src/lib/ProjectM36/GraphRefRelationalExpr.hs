module ProjectM36.GraphRefRelationalExpr where
--evaluate relational expressions across the entire transaction graph to support cross-transaction referencing
import ProjectM36.Base
import qualified Data.Set as S
import Data.Void

data SingularTransactionRef = SingularTransactionRef GraphRefTransactionMarker |
                              MultipleTransactionsRef |
                              NoTransactionsRef
                              deriving (Eq, Show)

instance Semigroup SingularTransactionRef where
  NoTransactionsRef <> x = x
  MultipleTransactionsRef <> _ = MultipleTransactionsRef
  SingularTransactionRef tidA <> s@(SingularTransactionRef tidB) =
    if tidA == tidB then
      s
    else
      MultipleTransactionsRef
  s@SingularTransactionRef{} <> NoTransactionsRef = s
  _ <> MultipleTransactionsRef = MultipleTransactionsRef

instance Monoid SingularTransactionRef where
  mempty = NoTransactionsRef
  
-- | return `Just transid` if this GraphRefRelationalExpr refers to just one transaction in the graph. This is useful for determining if certain optimizations can apply.
singularTransaction :: Foldable f => f GraphRefTransactionMarker -> SingularTransactionRef
singularTransaction expr
  | S.null transSet = NoTransactionsRef
  | S.size transSet == 1 = SingularTransactionRef (head (S.toList transSet))
  | otherwise = MultipleTransactionsRef
  where
    transSet = foldr S.insert S.empty expr

-- | Return True if two 'GraphRefRelationalExpr's both refer exclusively to the same transaction (or none at all).
inSameTransaction :: GraphRefRelationalExpr -> GraphRefRelationalExpr -> Maybe GraphRefTransactionMarker
inSameTransaction exprA exprB = case (stA, stB) of
  (SingularTransactionRef tA, SingularTransactionRef tB) | tA == tB -> Just tA
  _ -> Nothing
  where stA = singularTransaction exprA
        stB = singularTransaction exprB

singularTransactions :: (Foldable f, Traversable t) => t (f GraphRefTransactionMarker) -> SingularTransactionRef
singularTransactions = foldMap singularTransaction

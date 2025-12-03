module ProjectM36.GraphRefRelationalExpr where
--evaluate relational expressions across the entire transaction graph to support cross-transaction referencing
import ProjectM36.Base
import qualified Data.Set as S
import qualified Data.Functor.Foldable as Fold

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
singularTransaction :: Foldable t => t GraphRefTransactionMarker -> SingularTransactionRef
singularTransaction expr = case S.toList $ foldr S.insert S.empty expr of
  [] -> NoTransactionsRef
  x : xs -> case xs of
    [] -> SingularTransactionRef x
    _ -> MultipleTransactionsRef

-- | Return True if two 'GraphRefRelationalExpr's both refer exclusively to the same transaction (or none at all).
inSameTransaction :: GraphRefRelationalExpr -> GraphRefRelationalExpr -> Maybe GraphRefTransactionMarker
inSameTransaction exprA exprB = case (stA, stB) of
  (SingularTransactionRef tA, SingularTransactionRef tB) | tA == tB -> Just tA
  _ -> Nothing
  where stA = singularTransaction exprA
        stB = singularTransaction exprB

singularTransactions :: (Foldable f, Foldable t) => f (t GraphRefTransactionMarker) -> SingularTransactionRef
singularTransactions = foldMap singularTransaction

-- | Decompose a GraphRefRelationalExpr into its constituent references to relvars at transactionIds. Useful for determining which relvars in the transaction graph the expression references; for example, for security checks.
decomposeGraphRefTransactionMarkers :: GraphRefRelationalExpr -> S.Set (GraphRefTransactionMarker, RelVarName)
decomposeGraphRefTransactionMarkers = Fold.cata decomp
  where
    decomp :: RelationalExprBaseF GraphRefTransactionMarker (S.Set (GraphRefTransactionMarker, RelVarName)) -> S.Set (GraphRefTransactionMarker, RelVarName)
    decomp (RelationVariableF rvname marker) = S.singleton (marker, rvname)
    decomp other = foldr S.union mempty other

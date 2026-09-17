module ProjectM36.TransGraphRelationalExpression.Types where
import ProjectM36.Base
import ProjectM36.TransactionGraph.Types

-- | The TransGraphRelationalExpression is equivalent to a relational expression except that relation variables can reference relative points in the transaction graph (at previous points in time).
type TransGraphRelationalExpr = RelationalExprBase TransGraphAttributeNamesExpr TransactionIdLookup

type TransGraphAttributeNamesExpr = AttributeNamesExprBase TransactionIdLookup

type TransGraphExtendTupleExpr = ExtendTupleExprBase TransGraphAttributeNamesExpr TransactionIdLookup

type TransGraphTupleExpr = TupleExprBase TransGraphAttributeNamesExpr TransactionIdLookup

type TransGraphTupleExprs = TupleExprsBase TransGraphAttributeNamesExpr TransactionIdLookup

type TransGraphRestrictionPredicateExpr = RestrictionPredicateExprBase TransGraphAttributeNamesExpr TransactionIdLookup

type TransGraphAtomExpr = AtomExprBase TransGraphAttributeNamesExpr TransactionIdLookup

type TransGraphAttributeExpr = AttributeExprBase TransactionIdLookup

type TransGraphWithNameExpr = WithNameExprBase TransactionIdLookup

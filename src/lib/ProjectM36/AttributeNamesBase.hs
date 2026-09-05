-- | Variants of Base where the AttributeNames are fully resolved.
module ProjectM36.AttributeNamesBase where
import ProjectM36.Base
import ProjectM36.AccessControlList (RoleId)

type RelationalExpr = RelationalExprBase AttributeNames ()

type GraphRefRelationalExpr = RelationalExprBase AttributeNames GraphRefTransactionMarker

type RestrictionPredicateExpr = RestrictionPredicateExprBase AttributeNames GraphRefTransactionMarker

type AtomExpr = AtomExprBase AttributeNames ()

type DatabaseContextExpr = DatabaseContextExprBase AttributeNames () RoleName

type DatabaseContextExpr' = DatabaseContextExprBase AttributeNames () RoleId

type GraphRefDatabaseContextExpr = DatabaseContextExprBase AttributeNames GraphRefTransactionMarker RoleName

type GraphRefDatabaseContextExpr' = DatabaseContextExprBase AttributeNames GraphRefTransactionMarker RoleId

type TupleExpr = TupleExprBase AttributeNames ()

type TupleExprs = TupleExprsBase AttributeNames ()

type ExtendTupleExpr = ExtendTupleExprBase AttributeNames ()

type GraphRefTupleExprs = TupleExprsBase AttributeNames GraphRefTransactionMarker

type GraphRefTupleExpr = TupleExprBase AttributeNames GraphRefTransactionMarker

type GraphRefAttributeNamesExpr = AttributeNamesExprBase GraphRefTransactionMarker

type GraphRefRestrictionPredicateExpr = RestrictionPredicateExprBase AttributeNames GraphRefTransactionMarker

type GraphRefExtendTupleExpr = ExtendTupleExprBase AttributeNames GraphRefTransactionMarker

type GraphRefAtomExpr = AtomExprBase AttributeNames GraphRefTransactionMarker

type GraphRefWithNameAssocs = [(GraphRefWithNameExpr, RelationalExprBase AttributeNames GraphRefTransactionMarker)]

type WithNamesAssocs = WithNamesAssocsBase AttributeNames ()

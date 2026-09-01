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

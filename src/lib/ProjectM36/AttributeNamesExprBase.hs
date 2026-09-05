-- | Variants of Base where the AttributeNames are unresolved for user-convenience.
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TypeSynonymInstances, FlexibleInstances #-}
module ProjectM36.AttributeNamesExprBase where
import qualified ProjectM36.Base as B
import qualified Data.Map as M
import ProjectM36.AccessControlList
--import Data.Hashable (Hashable)

type RelationalExpr = B.RelationalExprBase B.AttributeNamesExpr ()

type GraphRefAtomExpr = B.AtomExprBase B.GraphRefAttributeNamesExpr B.GraphRefTransactionMarker

type AtomExpr = B.AtomExprBase B.AttributeNamesExpr ()

type TupleExpr = B.TupleExprBase B.AttributeNamesExpr ()

type GraphRefTupleExpr = B.TupleExprBase B.GraphRefAttributeNamesExpr B.GraphRefTransactionMarker

-- a fundamental relational expr to which other relational expressions compile
type GraphRefRelationalExpr = B.RelationalExprBase B.GraphRefAttributeNamesExpr B.GraphRefTransactionMarker 

type GraphRefWithNameAssocs = [(B.GraphRefWithNameExpr, GraphRefRelationalExpr)]

type RelationVariables = M.Map B.RelVarName GraphRefRelationalExpr

type RegisteredQueries = M.Map B.RegisteredQueryName RelationalExpr


type InclusionDependencies = M.Map B.IncDepName B.InclusionDependency

type DatabaseContextExpr = B.DatabaseContextExprBase B.AttributeNamesExpr () B.RoleName 

type DatabaseContextExpr' = B.DatabaseContextExprBase B.AttributeNamesExpr () RoleId 

--instance Hashable DatabaseContextExpr

type GraphRefDatabaseContextExpr = B.DatabaseContextExprBase B.GraphRefAttributeNamesExpr B.GraphRefTransactionMarker B.RoleName

type GraphRefDatabaseContextExpr' = B.DatabaseContextExprBase B.AttributeNamesExpr B.GraphRefTransactionMarker RoleId

type TransactionDiffExpr = DatabaseContextExpr

type GraphRefTupleExprs = B.TupleExprsBase B.GraphRefAttributeNamesExpr B.GraphRefTransactionMarker

type TupleExprs = B.TupleExprsBase B.AttributeNamesExpr ()

type RestrictionPredicateExpr = B.RestrictionPredicateExprBase B.AttributeNamesExpr ()

type GraphRefRestrictionPredicateExpr = B.RestrictionPredicateExprBase B.GraphRefAttributeNamesExpr B.GraphRefTransactionMarker

type GraphRefExtendTupleExpr = B.ExtendTupleExprBase B.GraphRefAttributeNamesExpr B.GraphRefTransactionMarker

type ExtendTupleExpr = B.ExtendTupleExprBase B.AttributeNamesExpr ()


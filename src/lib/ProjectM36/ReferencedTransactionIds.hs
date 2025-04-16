{-# LANGUAGE FlexibleInstances #-}
module ProjectM36.ReferencedTransactionIds where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.DatabaseContext.Types
import ProjectM36.Transaction.Types
import ProjectM36.TransactionGraph.Types
import ProjectM36.RelationalExpression
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad (foldM)
import Optics.Core

type TransactionIds = S.Set TransactionId

-- return all transactionIds referenced recursively- can be used to create subgraph of transaction dependencies
class ReferencedTransactionIds a where
  referencedTransactionIds :: a -> TransactionIds

instance ReferencedTransactionIds a => ReferencedTransactionIds (RelationalExprBase a) where
  referencedTransactionIds x = case x of
    MakeRelationFromExprs (Just attrExprs) tupleExprs ->
      S.unions (referencedTransactionIds tupleExprs : map referencedTransactionIds attrExprs)
    MakeRelationFromExprs Nothing tupleExprs ->
      referencedTransactionIds tupleExprs
    MakeStaticRelation{} -> S.empty
    ExistingRelation{} -> S.empty
    RelationVariable _ marker -> referencedTransactionIds marker
    RelationValuedAttribute _ -> S.empty
    Project attrNames expr -> S.union (referencedTransactionIds attrNames) (referencedTransactionIds expr)
    Union exprA exprB -> S.union (referencedTransactionIds exprA) (referencedTransactionIds exprB)
    Join exprA exprB -> S.union (referencedTransactionIds exprA) (referencedTransactionIds exprB)
    Rename _ expr -> referencedTransactionIds expr
    Difference exprA exprB -> S.union (referencedTransactionIds exprA) (referencedTransactionIds exprB)
    Group attrNames _ expr -> S.union (referencedTransactionIds attrNames) (referencedTransactionIds expr)
    Ungroup _ expr -> referencedTransactionIds expr
    Restrict pred' expr -> S.union (referencedTransactionIds pred') (referencedTransactionIds expr)
    Equals exprA exprB -> S.union (referencedTransactionIds exprA) (referencedTransactionIds exprB)
    NotEquals exprA exprB -> S.union (referencedTransactionIds exprA) (referencedTransactionIds exprB)
    Extend extendTupleExpr expr -> S.union (referencedTransactionIds extendTupleExpr) (referencedTransactionIds expr)
    With assocs expr -> S.unions (referencedTransactionIds expr : map tAssocs assocs)
      where
        tAssocs (withNameExpr, rExpr) = S.union (referencedTransactionIds withNameExpr) (referencedTransactionIds rExpr)

instance ReferencedTransactionIds a => ReferencedTransactionIds (AttributeExprBase a) where
  referencedTransactionIds NakedAttributeExpr{} = S.empty
  referencedTransactionIds (AttributeAndTypeNameExpr _ _ marker) = referencedTransactionIds marker

instance ReferencedTransactionIds a => ReferencedTransactionIds (TupleExprBase a) where
  referencedTransactionIds (TupleExpr tMap) =
    S.unions (referencedTransactionIds <$> M.elems tMap)

instance ReferencedTransactionIds a => ReferencedTransactionIds (TupleExprsBase a) where
  referencedTransactionIds (TupleExprs marker tupleExprs) =
    S.unions (referencedTransactionIds marker : (referencedTransactionIds <$> tupleExprs))

instance ReferencedTransactionIds GraphRefTransactionMarker where
  referencedTransactionIds (TransactionMarker tid) = S.singleton tid
  referencedTransactionIds UncommittedContextMarker = S.empty -- we have other methods to determine if there is an uncommitted transaction marker in the expr

instance ReferencedTransactionIds a => ReferencedTransactionIds (AttributeNamesBase a) where
  referencedTransactionIds names =
    case names of
      AttributeNames{} -> S.empty
      InvertedAttributeNames{} -> S.empty
      UnionAttributeNames exprA exprB ->
        S.union (referencedTransactionIds exprA) (referencedTransactionIds exprB)
      IntersectAttributeNames exprA exprB ->
        S.union (referencedTransactionIds exprA) (referencedTransactionIds exprB)
      RelationalExprAttributeNames rExpr ->
        referencedTransactionIds rExpr

instance ReferencedTransactionIds a => ReferencedTransactionIds (RestrictionPredicateExprBase a) where
  referencedTransactionIds expr =
    case expr of
      TruePredicate -> mempty
      AndPredicate exprA exprB ->
        S.union (referencedTransactionIds exprA) (referencedTransactionIds exprB)
      OrPredicate exprA exprB ->
        S.union (referencedTransactionIds exprA) (referencedTransactionIds exprB)
      NotPredicate exprA ->
        referencedTransactionIds exprA
      RelationalExprPredicate rExpr ->
        referencedTransactionIds rExpr
      AtomExprPredicate aExpr ->
        referencedTransactionIds aExpr
      AttributeEqualityPredicate _ aExpr ->
        referencedTransactionIds aExpr

instance ReferencedTransactionIds a => ReferencedTransactionIds (ExtendTupleExprBase a) where
  referencedTransactionIds (AttributeExtendTupleExpr _ aExpr) =
    referencedTransactionIds aExpr

instance ReferencedTransactionIds a => ReferencedTransactionIds (WithNameExprBase a) where
  referencedTransactionIds (WithNameExpr _ marker) = referencedTransactionIds marker

instance ReferencedTransactionIds a => ReferencedTransactionIds (AtomExprBase a) where
  referencedTransactionIds expr =
    case expr of
      AttributeAtomExpr{} -> mempty
      NakedAtomExpr{} -> mempty
      SubrelationAttributeAtomExpr{} -> mempty
      FunctionAtomExpr _ args marker ->
        S.unions (referencedTransactionIds marker : (referencedTransactionIds <$> args))
      RelationAtomExpr rExpr ->
        referencedTransactionIds rExpr
      ConstructedAtomExpr _ args marker ->
        S.unions (referencedTransactionIds marker : (referencedTransactionIds <$> args))
      IfThenAtomExpr ifE thenE elseE ->
        S.unions [referencedTransactionIds ifE,
                  referencedTransactionIds thenE,
                  referencedTransactionIds elseE]

-- only the relvars can reference other transactions
instance ReferencedTransactionIds DatabaseContext where
  referencedTransactionIds dbc =
    S.unions [
    --referencedTransactionIds (inclusionDependencies dbc),
    referencedTransactionIds (relationVariables dbc)
    --referencedTransactionIds (atomFunctions dbc),
    --referencedTransactionIds (dbcFunctions dbc),
    --referencedTransactionIds (notifications dbc),
    --referencedTransactionIds (typeConstructorMapping dbc),
    --referencedTransactionIds (registeredQueries dbc)
    ]

instance ReferencedTransactionIds a => ReferencedTransactionIds (ValueMarker a) where
  referencedTransactionIds (ValueMarker a) = referencedTransactionIds a
  referencedTransactionIds (NotChangedSinceMarker tid) = S.singleton tid

instance ReferencedTransactionIds RelationVariables where
  referencedTransactionIds relVars =
    S.unions (referencedTransactionIds <$> M.elems relVars)

-- | Recurse relvars references and transaction parents to extract a subset of relevant transactions.
-- probably could do some trimming of transactions that are not referenced by relvars, but that is rare, so probably of not much benefit
-- should be trim merge parents that don't contribute to the relvars? maybe
referencedTransactionIdsForTransaction :: Transaction -> TransactionGraph -> Either RelationalError (S.Set Transaction)
referencedTransactionIdsForTransaction trans graph
  | parentIds' == rootParent = pure (S.singleton trans)
  | otherwise = 
    foldM folder (S.singleton trans) parentIds'
  where
    parentIds' = parents (transactionInfo trans)
    folder acc transId' = do
      trans' <- transactionForId transId' graph
      transSet <- referencedTransactionIdsForTransaction trans' graph
      pure (S.union acc transSet)





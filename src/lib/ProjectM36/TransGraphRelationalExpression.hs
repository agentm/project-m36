--really, a better name for this module could be "TransTransactionGraphRelationalExpr", but that name is too long
module ProjectM36.TransGraphRelationalExpression where
import ProjectM36.Base
import ProjectM36.TransactionGraph
import ProjectM36.RelationalExpression
import ProjectM36.Error
import Control.Monad.State

-- | The TransGraphRelationalExpression is equivalent to a relational expression except that relation variables can reference points in the transaction graph (at previous points in time).
type TransGraphRelationalExpr = RelationalExprBase TransactionIdLookup

type TransGraphExtendTupleExpr = ExtendTupleExprBase TransactionIdLookup

type TransGraphTupleExpr = TupleExprBase TransactionIdLookup

type TransGraphRestrictionPredicateExpr = RestrictionPredicateExprBase TransactionIdLookup

evalTransGraphRelationalExpr :: TransGraphRelationalExpr -> TransactionGraph -> Either RelationalError RelationalExpr
evalTransGraphRelationalExpr (MakeRelationFromExprs mAttrs tupleExprs) graph = do
  tupleExprs' <- mapM (flip evalTransGraphTupleExpr graph) tupleExprs
  pure $ MakeRelationFromExprs mAttrs tupleExprs'
evalTransGraphRelationalExpr (MakeStaticRelation attrs tupSet) _ = pure (MakeStaticRelation attrs tupSet)
evalTransGraphRelationalExpr (ExistingRelation rel) _ = pure (ExistingRelation rel)
evalTransGraphRelationalExpr (RelationVariable rvname transLookup) graph = do
  trans <- lookupTransaction graph transLookup
  rel <- evalState (evalRelationalExpr (RelationVariable rvname ())) (transactionContext trans)
  pure (ExistingRelation rel)
evalTransGraphRelationalExpr (Project attrNames expr) graph = do
  expr' <- evalTransGraphRelationalExpr expr graph
  pure (Project attrNames expr')
evalTransGraphRelationalExpr (Union exprA exprB) graph = do
  exprA' <- evalTransGraphRelationalExpr exprA graph
  exprB' <- evalTransGraphRelationalExpr exprB graph
  pure (Union exprA' exprB')
evalTransGraphRelationalExpr (Join exprA exprB) graph = do
  exprA' <- evalTransGraphRelationalExpr exprA graph
  exprB' <- evalTransGraphRelationalExpr exprB graph
  pure (Join exprA' exprB')
evalTransGraphRelationalExpr (Rename attrName1 attrName2 expr) graph = do
  expr' <- evalTransGraphRelationalExpr expr graph  
  pure (Rename attrName1 attrName2 expr')
evalTransGraphRelationalExpr (Difference exprA exprB) graph = do  
  exprA' <- evalTransGraphRelationalExpr exprA graph
  exprB' <- evalTransGraphRelationalExpr exprB graph
  pure (Difference exprA' exprB')
evalTransGraphRelationalExpr (Group attrNames attrName expr) graph = do  
  expr' <- evalTransGraphRelationalExpr expr graph  
  pure (Group attrNames attrName expr')
evalTransGraphRelationalExpr (Ungroup attrName expr) graph = do  
  expr' <- evalTransGraphRelationalExpr expr graph    
  pure (Ungroup attrName expr')
evalTransGraphRelationalExpr (Restrict predicateExpr expr) graph = do
  expr' <- evalTransGraphRelationalExpr expr graph  
  predicateExpr' <- evalTransGraphRestrictionPredicateExpr predicateExpr graph
  pure (Restrict predicateExpr' expr')
evalTransGraphRelationalExpr (Equals exprA exprB) graph = do  
  exprA' <- evalTransGraphRelationalExpr exprA graph
  exprB' <- evalTransGraphRelationalExpr exprB graph
  pure (Equals exprA' exprB')
evalTransGraphRelationalExpr (NotEquals exprA exprB) graph = do  
  exprA' <- evalTransGraphRelationalExpr exprA graph
  exprB' <- evalTransGraphRelationalExpr exprB graph
  pure (NotEquals exprA' exprB')
evalTransGraphRelationalExpr (Extend extendExpr expr) graph = do
  extendExpr' <- evalTransGraphExtendTupleExpr extendExpr graph
  expr' <- evalTransGraphRelationalExpr expr graph
  pure (Extend extendExpr' expr')
  
evalTransGraphTupleExpr :: TransGraphTupleExpr -> TransactionGraph -> Either RelationalError TupleExpr
evalTransGraphTupleExpr = undefined

evalTransGraphRestrictionPredicateExpr :: TransGraphRestrictionPredicateExpr -> TransactionGraph -> Either RelationalError RestrictionPredicateExpr
evalTransGraphRestrictionPredicateExpr = undefined

evalTransGraphExtendTupleExpr :: TransGraphExtendTupleExpr -> TransactionGraph -> Either RelationalError ExtendTupleExpr
evalTransGraphExtendTupleExpr = undefined 

  
{-
-- | A simple functor which maps all trans graph relational expressions to the standard relational expression, discarding any time travel in the graph.
transGraphRelExprToRelExpr :: TransGraphRelationalExpr -> RelationalExpr
transGraphRelExprToRelExpr (MakeRelationFromExprs a b) = MakeRelationFromExprs a (transGraphTupleExprToTupleExpr b)
transGraphRelExprToRelExpr (MakeStaticRelation a b) = MakeStaticRelation a b
transGraphRelExprToRelExpr (ExistingRelation a) = ExistingRelation a
transGraphRelExprToRelExpr (RelationVariable (TransGraphRelVarName nam _)) = RelationVariable nam
transGraphRelExprToRelExpr (Project a b) = Project a b
transGraphRelExprToRelExpr (Union a b) = Union a b
transGraphRelExprToRelExpr (Join a b) = Join a b
transGraphRelExprToRelExpr (Rename a b c) = Rename a b c
transGraphRelExprToRelExpr (Difference a b) = Difference a b
transGraphRelExprToRelExpr (Group a b c) = Group a b c
transGraphRelExprToRelExpr (Ungroup a b) = Ungroup a b
transGraphRelExprToRelExpr (Restrict a b) = Restrict a b
transGraphRelExprToRelExpr (Equals a b) = Equals a b
transGraphRelExprToRelExpr (NotEquals a b) = NotEquals a b
transGraphRelExprToRelExpr (Extend a b) = Extend a b
-}
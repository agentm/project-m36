{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--really, a better name for this module could be "TransTransactionGraphRelationalExpr", but that name is too long
module ProjectM36.TransGraphRelationalExpression where
import ProjectM36.Base
import ProjectM36.TransactionGraph
import ProjectM36.Transaction
import ProjectM36.RelationalExpression
import ProjectM36.RelationalExpressionState
import ProjectM36.Error
import Control.Monad.State
import ProjectM36.Tuple
import ProjectM36.AtomType
import qualified Data.Map as M

-- | The TransGraphRelationalExpression is equivalent to a relational expression except that relation variables can reference points in the transaction graph (at previous points in time).
type TransGraphRelationalExpr = RelationalExprBase TransactionIdLookup

type TransGraphExtendTupleExpr = ExtendTupleExprBase TransactionIdLookup

type TransGraphTupleExpr = TupleExprBase TransactionIdLookup

type TransGraphRestrictionPredicateExpr = RestrictionPredicateExprBase TransactionIdLookup

type TransGraphAtomExpr = AtomExprBase TransactionIdLookup

type TransGraphAttributeExpr = AttributeExprBase TransactionIdLookup

-- OUTDATED a previous attempt at this function attempted to convert TransGraphRelationalExpr to RelationalExpr by resolving the transaction lookups. However, there is no way to resolve a FunctionAtomExpr to an Atom without fully evaluating the higher levels (TupleExpr, etc.). An anonymous function expression cannot be serialized, so that workaround is out. Still, I suspect we can reuse the existing static optimizer logic to work on both structures. The current conversion reduces the chance of whole-query optimization due to full-evaluation on top of full-evaluation, so this function would benefit from some re-design.
evalTransGraphRelationalExpr :: TransGraphRelationalExpr -> TransactionGraph -> Either RelationalError RelationalExpr
evalTransGraphRelationalExpr (MakeRelationFromExprs mAttrExprs tupleExprs) graph = do
  tupleExprs' <- mapM (evalTransGraphTupleExpr graph) tupleExprs
  case mAttrExprs of
    Nothing -> pure (MakeRelationFromExprs Nothing tupleExprs')
    Just attrExprs -> do
      attrExprs' <- mapM (evalTransGraphAttributeExpr graph) attrExprs
      pure (MakeRelationFromExprs (Just attrExprs') tupleExprs')
evalTransGraphRelationalExpr (MakeStaticRelation attrs tupSet) _ = pure (MakeStaticRelation attrs tupSet)
evalTransGraphRelationalExpr (ExistingRelation rel) _ = pure (ExistingRelation rel)
evalTransGraphRelationalExpr (RelationVariable rvname transLookup) graph = do
  trans <- lookupTransaction graph transLookup
  rel <- evalState (evalRelationalExpr (RelationVariable rvname ())) (RelationalExprStateElems (concreteDatabaseContext trans))
  pure (ExistingRelation rel)
evalTransGraphRelationalExpr (Project attrNames expr) graph = do
  expr' <- evalTransGraphRelationalExpr expr graph
  pure (Project attrNames expr')
evalTransGraphRelationalExpr (Union exprA exprB) graph = do
  exprA' <- evalTransGraphRelationalExpr exprA graph
  exprB' <- evalTransGraphRelationalExpr exprB graph
  pure (Union exprA'  exprB')
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
  
evalTransGraphTupleExpr :: TransactionGraph -> TransGraphTupleExpr -> Either RelationalError TupleExpr
evalTransGraphTupleExpr graph (TupleExpr attrMap) = do
  attrAssoc <- mapM (\(attrName, atomExpr) -> do 
                        aExpr <- evalTransGraphAtomExpr graph atomExpr
                        pure (attrName, aExpr)
                    ) (M.toList attrMap)
  pure (TupleExpr (M.fromList attrAssoc))
  
evalTransGraphAtomExpr :: TransactionGraph -> TransGraphAtomExpr -> Either RelationalError AtomExpr
evalTransGraphAtomExpr _ (AttributeAtomExpr aname) = pure $ AttributeAtomExpr aname
evalTransGraphAtomExpr _ (NakedAtomExpr atom) = pure $ NakedAtomExpr atom
evalTransGraphAtomExpr graph (FunctionAtomExpr funcName args tLookup) = do
  trans <- lookupTransaction graph tLookup
  --I can't return a FunctionAtomExpr because the function needs to be resolved at a specific context
  args' <- mapM (evalTransGraphAtomExpr graph) args
  atom <- evalState (evalAtomExpr emptyTuple (FunctionAtomExpr funcName args' ())) (RelationalExprStateElems (concreteDatabaseContext trans))
  pure (NakedAtomExpr atom)
evalTransGraphAtomExpr graph (RelationAtomExpr expr) = do
  expr' <- evalTransGraphRelationalExpr expr graph 
  pure (RelationAtomExpr expr')
evalTransGraphAtomExpr graph (ConstructedAtomExpr dConsName args tLookup) = do
  trans <- lookupTransaction graph tLookup  
  args' <- mapM (evalTransGraphAtomExpr graph) args
  atom <- evalState (evalAtomExpr emptyTuple (ConstructedAtomExpr dConsName args' ())) (RelationalExprStateElems (concreteDatabaseContext trans))
  pure (NakedAtomExpr atom)

evalTransGraphRestrictionPredicateExpr :: TransGraphRestrictionPredicateExpr -> TransactionGraph -> Either RelationalError RestrictionPredicateExpr
evalTransGraphRestrictionPredicateExpr TruePredicate _ = pure TruePredicate
evalTransGraphRestrictionPredicateExpr (AndPredicate exprA exprB) graph = do
  exprA' <- evalTransGraphRestrictionPredicateExpr exprA graph
  exprB' <- evalTransGraphRestrictionPredicateExpr exprB graph
  pure (AndPredicate exprA' exprB')
evalTransGraphRestrictionPredicateExpr (OrPredicate exprA exprB) graph = do  
  exprA' <- evalTransGraphRestrictionPredicateExpr exprA graph
  exprB' <- evalTransGraphRestrictionPredicateExpr exprB graph
  pure (OrPredicate exprA' exprB')
evalTransGraphRestrictionPredicateExpr (NotPredicate expr) graph = do
  expr' <- evalTransGraphRestrictionPredicateExpr expr graph
  pure (NotPredicate expr')
evalTransGraphRestrictionPredicateExpr (RelationalExprPredicate expr) graph = do  
  expr' <- evalTransGraphRelationalExpr expr graph
  pure (RelationalExprPredicate expr')
evalTransGraphRestrictionPredicateExpr (AtomExprPredicate expr) graph = do
  expr' <- evalTransGraphAtomExpr graph expr
  pure (AtomExprPredicate expr')
evalTransGraphRestrictionPredicateExpr (AttributeEqualityPredicate attrName expr) graph = do  
  expr' <- evalTransGraphAtomExpr graph expr
  pure (AttributeEqualityPredicate attrName expr')
  
evalTransGraphExtendTupleExpr :: TransGraphExtendTupleExpr -> TransactionGraph -> Either RelationalError ExtendTupleExpr
evalTransGraphExtendTupleExpr (AttributeExtendTupleExpr attrName expr) graph = do
  expr' <- evalTransGraphAtomExpr graph expr
  pure (AttributeExtendTupleExpr attrName expr')

evalTransGraphAttributeExpr :: TransactionGraph -> TransGraphAttributeExpr -> Either RelationalError AttributeExpr
evalTransGraphAttributeExpr graph (AttributeAndTypeNameExpr attrName tCons tLookup) = do
  trans <- lookupTransaction graph tLookup
  aType <- atomTypeForTypeConstructor tCons (typeConstructorMapping (concreteDatabaseContext trans))
  pure (NakedAttributeExpr (Attribute attrName aType))
evalTransGraphAttributeExpr _ (NakedAttributeExpr attr) = pure (NakedAttributeExpr attr)  

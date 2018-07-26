{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--really, a better name for this module could be "TransTransactionGraphRelationalExpr", but that name is too long
module ProjectM36.TransGraphRelationalExpression where
import ProjectM36.Base
import ProjectM36.TransactionGraph
import ProjectM36.Transaction
import ProjectM36.RelationalExpression
import ProjectM36.Error
import ProjectM36.Tuple
import ProjectM36.AtomType
import qualified Data.Map as M
import Control.Monad.Trans.Reader
import Data.Binary

-- | The TransGraphRelationalExpression is equivalent to a relational expression except that relation variables can reference points in the transaction graph (at previous points in time).
type TransGraphRelationalExpr = RelationalExprBase TransactionIdLookup

instance Binary TransGraphRelationalExpr

type TransGraphAttributeNames = AttributeNamesBase TransactionIdLookup

instance Binary TransGraphAttributeNames

type TransGraphExtendTupleExpr = ExtendTupleExprBase TransactionIdLookup

instance Binary TransGraphExtendTupleExpr

type TransGraphTupleExpr = TupleExprBase TransactionIdLookup

instance Binary TransGraphTupleExpr

type TransGraphRestrictionPredicateExpr = RestrictionPredicateExprBase TransactionIdLookup

instance Binary TransGraphRestrictionPredicateExpr

type TransGraphAtomExpr = AtomExprBase TransactionIdLookup

instance Binary TransGraphAtomExpr 

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
  rel <- runReader (evalRelationalExpr (RelationVariable rvname ())) (RelationalExprStateElems (concreteDatabaseContext trans))
  pure (ExistingRelation rel)
evalTransGraphRelationalExpr (Project transAttrNames expr) graph = do
  expr' <- evalTransGraphRelationalExpr expr graph
  attrNames <- evalTransAttributeNames transAttrNames graph
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
  let expr' = evalTransGraphRelationalExpr expr graph  
  Rename attrName1 attrName2 <$> expr'
evalTransGraphRelationalExpr (Difference exprA exprB) graph = do  
  exprA' <- evalTransGraphRelationalExpr exprA graph
  exprB' <- evalTransGraphRelationalExpr exprB graph
  pure (Difference exprA' exprB')
evalTransGraphRelationalExpr (Group transAttrNames attrName expr) graph = do  
  expr' <- evalTransGraphRelationalExpr expr graph
  attrNames <- evalTransAttributeNames transAttrNames graph
  pure (Group attrNames attrName expr')
evalTransGraphRelationalExpr (Ungroup attrName expr) graph = do  
  let expr' = evalTransGraphRelationalExpr expr graph    
  Ungroup attrName <$> expr'
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
evalTransGraphRelationalExpr (With views expr) graph = do
  evaldViews <- mapM (\(vname, vexpr) -> do
                         vexpr' <- evalTransGraphRelationalExpr vexpr graph
                         pure (vname, vexpr')
                     ) views
  expr' <- evalTransGraphRelationalExpr expr graph
  pure (With evaldViews expr')
  
evalTransGraphTupleExpr :: TransactionGraph -> TransGraphTupleExpr -> Either RelationalError TupleExpr
evalTransGraphTupleExpr graph (TupleExpr attrMap) = do
  let attrAssoc = mapM (\(attrName, atomExpr) -> do 
                        aExpr <- evalTransGraphAtomExpr graph atomExpr
                        pure (attrName, aExpr)
                    ) (M.toList attrMap)
  TupleExpr . M.fromList <$> attrAssoc
  
evalTransGraphAtomExpr :: TransactionGraph -> TransGraphAtomExpr -> Either RelationalError AtomExpr
evalTransGraphAtomExpr _ (AttributeAtomExpr aname) = pure $ AttributeAtomExpr aname
evalTransGraphAtomExpr _ (NakedAtomExpr atom) = pure $ NakedAtomExpr atom
evalTransGraphAtomExpr graph (FunctionAtomExpr funcName args tLookup) = do
  trans <- lookupTransaction graph tLookup
  --I can't return a FunctionAtomExpr because the function needs to be resolved at a specific context
  args' <- mapM (evalTransGraphAtomExpr graph) args
  atom <- runReader (evalAtomExpr emptyTuple (FunctionAtomExpr funcName args' ())) (RelationalExprStateElems (concreteDatabaseContext trans))
  pure (NakedAtomExpr atom)
evalTransGraphAtomExpr graph (RelationAtomExpr expr) = do
  let expr' = evalTransGraphRelationalExpr expr graph 
  RelationAtomExpr <$> expr'
evalTransGraphAtomExpr graph (ConstructedAtomExpr dConsName args tLookup) = do
  trans <- lookupTransaction graph tLookup  
  args' <- mapM (evalTransGraphAtomExpr graph) args
  atom <- runReader (evalAtomExpr emptyTuple (ConstructedAtomExpr dConsName args' ())) (RelationalExprStateElems (concreteDatabaseContext trans))
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
  let expr' = evalTransGraphRestrictionPredicateExpr expr graph
  NotPredicate <$> expr'
evalTransGraphRestrictionPredicateExpr (RelationalExprPredicate expr) graph = do  
  let expr' = evalTransGraphRelationalExpr expr graph
  RelationalExprPredicate <$> expr'
evalTransGraphRestrictionPredicateExpr (AtomExprPredicate expr) graph = do
  let expr' = evalTransGraphAtomExpr graph expr
  AtomExprPredicate <$> expr'
evalTransGraphRestrictionPredicateExpr (AttributeEqualityPredicate attrName expr) graph = do  
  let expr' = evalTransGraphAtomExpr graph expr
  AttributeEqualityPredicate attrName <$> expr'
  
evalTransGraphExtendTupleExpr :: TransGraphExtendTupleExpr -> TransactionGraph -> Either RelationalError ExtendTupleExpr
evalTransGraphExtendTupleExpr (AttributeExtendTupleExpr attrName expr) graph = do
  let expr' = evalTransGraphAtomExpr graph expr
  AttributeExtendTupleExpr attrName <$> expr'

evalTransGraphAttributeExpr :: TransactionGraph -> TransGraphAttributeExpr -> Either RelationalError AttributeExpr
evalTransGraphAttributeExpr graph (AttributeAndTypeNameExpr attrName tCons tLookup) = do
  trans <- lookupTransaction graph tLookup
  aType <- atomTypeForTypeConstructor tCons (typeConstructorMapping (concreteDatabaseContext trans)) M.empty
  pure (NakedAttributeExpr (Attribute attrName aType))
evalTransGraphAttributeExpr _ (NakedAttributeExpr attr) = pure (NakedAttributeExpr attr)  

evalTransAttributeNames :: TransGraphAttributeNames -> TransactionGraph -> Either RelationalError AttributeNames
evalTransAttributeNames (AttributeNames names) _ = Right (AttributeNames names)
evalTransAttributeNames (InvertedAttributeNames names) _ = Right (InvertedAttributeNames names)
evalTransAttributeNames (UnionAttributeNames namesA namesB) graph = do
  nA <- evalTransAttributeNames namesA graph
  nB <- evalTransAttributeNames namesB graph
  Right (UnionAttributeNames nA nB)
evalTransAttributeNames (IntersectAttributeNames namesA namesB) graph = do
  nA <- evalTransAttributeNames namesA graph
  nB <- evalTransAttributeNames namesB graph
  Right (IntersectAttributeNames nA nB)
evalTransAttributeNames (RelationalExprAttributeNames expr) graph = do
  evaldExpr <- evalTransGraphRelationalExpr expr graph
  Right (RelationalExprAttributeNames evaldExpr)

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--really, a better name for this module could be "TransTransactionGraphRelationalExpr", but that name is too long
module ProjectM36.TransGraphRelationalExpression where
import ProjectM36.Base
import ProjectM36.TransactionGraph
import ProjectM36.Transaction
import ProjectM36.GraphRefRelationalExpr
import ProjectM36.RelationalExpression hiding (liftE)
import ProjectM36.Error
import ProjectM36.Tuple
import ProjectM36.AtomType
import qualified Data.Map as M
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Functor.Identity
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

data TransGraphEvalEnv = TransGraphEvalEnv { transId :: TransactionId,
                                             graph :: TransactionGraph
                                           }

type TransGraphEvalMonad a = ReaderT TransGraphEvalEnv (ExceptT RelationalError Identity) a

run :: TransGraphEvalEnv -> TransGraphRelationalExpr -> Either RelationalError GraphRefRelationalExpr
run env texpr = runIdentity (runExceptT (runReaderT (evalTransGraphRelationalExpr texpr) env))

liftE :: Either RelationalError a -> TransGraphEvalMonad a
liftE = lift . except


askTid :: TransGraphEvalMonad TransactionId
askTid = transId <$> ask

askGraph :: TransGraphEvalMonad TransactionGraph
askGraph = graph <$> ask

findTransId :: TransactionIdLookup -> TransGraphEvalMonad TransactionId
findTransId tlook = transactionId <$> findTrans tlook

findTrans :: TransactionIdLookup -> TransGraphEvalMonad Transaction
findTrans tlook = do
  graph <- askGraph
  liftE $ lookupTransaction graph tlook

-- OUTDATED a previous attempt at this function attempted to convert TransGraphRelationalExpr to RelationalExpr by resolving the transaction lookups. However, there is no way to resolve a FunctionAtomExpr to an Atom without fully evaluating the higher levels (TupleExpr, etc.). An anonymous function expression cannot be serialized, so that workaround is out. Still, I suspect we can reuse the existing static optimizer logic to work on both structures. The current conversion reduces the chance of whole-query optimization due to full-evaluation on top of full-evaluation, so this function would benefit from some re-design.
evalTransGraphRelationalExpr :: TransGraphRelationalExpr -> TransGraphEvalMonad GraphRefRelationalExpr
evalTransGraphRelationalExpr (MakeRelationFromExprs mAttrExprs tupleExprs) = do
  tupleExprs' <- mapM evalTransGraphTupleExpr tupleExprs
  case mAttrExprs of
    Nothing -> pure (MakeRelationFromExprs Nothing tupleExprs')
    Just attrExprs -> do
      attrExprs' <- mapM evalTransGraphAttributeExpr attrExprs
      pure (MakeRelationFromExprs (Just attrExprs') tupleExprs')
evalTransGraphRelationalExpr (MakeStaticRelation attrs tupSet) = pure (MakeStaticRelation attrs tupSet)
evalTransGraphRelationalExpr (ExistingRelation rel) = pure (ExistingRelation rel)
evalTransGraphRelationalExpr (RelationVariable rvname transLookup) = 
  RelationVariable rvname <$> findTransId transLookup
evalTransGraphRelationalExpr (Project transAttrNames expr) = 
  Project <$> evalTransAttributeNames transAttrNames <*> evalTransGraphRelationalExpr expr
evalTransGraphRelationalExpr (Union exprA exprB) =
  Union <$> evalTransGraphRelationalExpr exprA <*> evalTransGraphRelationalExpr exprB
evalTransGraphRelationalExpr (Join exprA exprB) =
  Join <$> evalTransGraphRelationalExpr exprA <*> evalTransGraphRelationalExpr exprB
evalTransGraphRelationalExpr (Rename attrName1 attrName2 expr) =
  Rename attrName1 attrName2 <$> evalTransGraphRelationalExpr expr
evalTransGraphRelationalExpr (Difference exprA exprB) =
  Difference <$> evalTransGraphRelationalExpr exprA <*> evalTransGraphRelationalExpr exprB
evalTransGraphRelationalExpr (Group transAttrNames attrName expr) = 
  Group <$>
    evalTransAttributeNames transAttrNames <*>
    pure attrName <*>
    evalTransGraphRelationalExpr expr
evalTransGraphRelationalExpr (Ungroup attrName expr) = 
  Ungroup attrName <$> evalTransGraphRelationalExpr expr
evalTransGraphRelationalExpr (Restrict predicateExpr expr) =
  Restrict <$> evalTransGraphRestrictionPredicateExpr predicateExpr <*>
    evalTransGraphRelationalExpr expr
evalTransGraphRelationalExpr (Equals exprA exprB) = do  
  exprA' <- evalTransGraphRelationalExpr exprA
  exprB' <- evalTransGraphRelationalExpr exprB 
  pure (Equals exprA' exprB')
evalTransGraphRelationalExpr (NotEquals exprA exprB) = do  
  exprA' <- evalTransGraphRelationalExpr exprA 
  exprB' <- evalTransGraphRelationalExpr exprB 
  pure (NotEquals exprA' exprB')
evalTransGraphRelationalExpr (Extend extendExpr expr) = do
  extendExpr' <- evalTransGraphExtendTupleExpr extendExpr 
  expr' <- evalTransGraphRelationalExpr expr 
  pure (Extend extendExpr' expr')
evalTransGraphRelationalExpr (With views expr) = do
  evaldViews <- mapM (\(vname, vexpr) -> do
                         vexpr' <- evalTransGraphRelationalExpr vexpr 
                         pure (vname, vexpr')
                     ) views
  expr' <- evalTransGraphRelationalExpr expr 
  pure (With evaldViews expr')
  
evalTransGraphTupleExpr :: TransGraphTupleExpr -> TransGraphEvalMonad GraphRefTupleExpr
evalTransGraphTupleExpr (TupleExpr attrMap) = do
  graph <- askGraph
  let attrAssoc = mapM (\(attrName, atomExpr) -> do 
                        aExpr <- evalTransGraphAtomExpr atomExpr
                        pure (attrName, aExpr)
                    ) (M.toList attrMap)
  TupleExpr . M.fromList <$> attrAssoc
  
evalTransGraphAtomExpr :: TransGraphAtomExpr -> TransGraphEvalMonad GraphRefAtomExpr
evalTransGraphAtomExpr (AttributeAtomExpr aname) = pure $ AttributeAtomExpr aname
evalTransGraphAtomExpr (NakedAtomExpr atom) = pure $ NakedAtomExpr atom
evalTransGraphAtomExpr (FunctionAtomExpr funcName args tLookup) =
  FunctionAtomExpr funcName <$> mapM evalTransGraphAtomExpr args <*> findTransId tLookup
evalTransGraphAtomExpr (RelationAtomExpr expr) =
  RelationAtomExpr <$> evalTransGraphRelationalExpr expr
evalTransGraphAtomExpr (ConstructedAtomExpr dConsName args tLookup) =
  ConstructedAtomExpr dConsName <$> mapM evalTransGraphAtomExpr args <*> findTransId tLookup
evalTransGraphRestrictionPredicateExpr :: TransGraphRestrictionPredicateExpr -> TransGraphEvalMonad GraphRefRestrictionPredicateExpr
evalTransGraphRestrictionPredicateExpr TruePredicate = pure TruePredicate
evalTransGraphRestrictionPredicateExpr (AndPredicate exprA exprB) = do
  exprA' <- evalTransGraphRestrictionPredicateExpr exprA
  exprB' <- evalTransGraphRestrictionPredicateExpr exprB
  pure (AndPredicate exprA' exprB')
evalTransGraphRestrictionPredicateExpr (OrPredicate exprA exprB) = do  
  exprA' <- evalTransGraphRestrictionPredicateExpr exprA 
  exprB' <- evalTransGraphRestrictionPredicateExpr exprB
  pure (OrPredicate exprA' exprB')
evalTransGraphRestrictionPredicateExpr (NotPredicate expr) = do
  let expr' = evalTransGraphRestrictionPredicateExpr expr
  NotPredicate <$> expr'
evalTransGraphRestrictionPredicateExpr (RelationalExprPredicate expr) = do  
  let expr' = evalTransGraphRelationalExpr expr
  RelationalExprPredicate <$> expr'
evalTransGraphRestrictionPredicateExpr (AtomExprPredicate expr) = do
  let expr' = evalTransGraphAtomExpr expr
  AtomExprPredicate <$> expr'
evalTransGraphRestrictionPredicateExpr (AttributeEqualityPredicate attrName expr) = do  
  let expr' = evalTransGraphAtomExpr expr
  AttributeEqualityPredicate attrName <$> expr'
  
evalTransGraphExtendTupleExpr :: TransGraphExtendTupleExpr -> TransGraphEvalMonad GraphRefExtendTupleExpr
evalTransGraphExtendTupleExpr (AttributeExtendTupleExpr attrName expr) = do
  AttributeExtendTupleExpr attrName <$> evalTransGraphAtomExpr expr

evalTransGraphAttributeExpr :: TransGraphAttributeExpr -> TransGraphEvalMonad GraphRefAttributeExpr
evalTransGraphAttributeExpr (AttributeAndTypeNameExpr attrName tCons tLookup) =
  AttributeAndTypeNameExpr attrName tCons <$> findTransId tLookup
evalTransGraphAttributeExpr (NakedAttributeExpr attr) = pure (NakedAttributeExpr attr)  

evalTransAttributeNames :: TransGraphAttributeNames -> TransGraphEvalMonad GraphRefAttributeNames
evalTransAttributeNames (AttributeNames names) = pure (AttributeNames names)
evalTransAttributeNames (InvertedAttributeNames names) = pure (InvertedAttributeNames names)
evalTransAttributeNames (UnionAttributeNames namesA namesB) = do
  nA <- evalTransAttributeNames namesA 
  nB <- evalTransAttributeNames namesB
  pure (UnionAttributeNames nA nB)
evalTransAttributeNames (IntersectAttributeNames namesA namesB) = do
  nA <- evalTransAttributeNames namesA 
  nB <- evalTransAttributeNames namesB 
  pure (IntersectAttributeNames nA nB)
evalTransAttributeNames (RelationalExprAttributeNames expr) = do
  evaldExpr <- evalTransGraphRelationalExpr expr 
  pure (RelationalExprAttributeNames evaldExpr)

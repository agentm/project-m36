{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--really, a better name for this module could be "TransTransactionGraphRelationalExpr", but that name is too long
module ProjectM36.TransGraphRelationalExpression where
import ProjectM36.Base
import ProjectM36.TransactionGraph
import ProjectM36.Error
import qualified Data.Map as M
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Functor.Identity

-- | The TransGraphRelationalExpression is equivalent to a relational expression except that relation variables can reference points in the transaction graph (at previous points in time).
type TransGraphRelationalExpr = RelationalExprBase TransactionIdLookup

type TransGraphAttributeNames = AttributeNamesBase TransactionIdLookup

type TransGraphExtendTupleExpr = ExtendTupleExprBase TransactionIdLookup

type TransGraphTupleExpr = TupleExprBase TransactionIdLookup

type TransGraphTupleExprs = TupleExprsBase TransactionIdLookup

type TransGraphRestrictionPredicateExpr = RestrictionPredicateExprBase TransactionIdLookup

type TransGraphAtomExpr = AtomExprBase TransactionIdLookup

type TransGraphAttributeExpr = AttributeExprBase TransactionIdLookup

type TransGraphWithNameExpr = WithNameExprBase TransactionIdLookup

newtype TransGraphEvalEnv = TransGraphEvalEnv {
  tge_graph :: TransactionGraph
  }

type TransGraphEvalMonad a = ReaderT TransGraphEvalEnv (ExceptT RelationalError Identity) a

process :: TransGraphEvalEnv -> TransGraphRelationalExpr -> Either RelationalError GraphRefRelationalExpr
process env texpr = runIdentity (runExceptT (runReaderT (processTransGraphRelationalExpr texpr) env))

liftE :: Either RelationalError a -> TransGraphEvalMonad a
liftE = lift . except

askGraph :: TransGraphEvalMonad TransactionGraph
askGraph = tge_graph <$> ask

findTransId :: TransactionIdLookup -> TransGraphEvalMonad GraphRefTransactionMarker
findTransId tlook = TransactionMarker . transactionId <$> findTrans tlook

findTrans :: TransactionIdLookup -> TransGraphEvalMonad Transaction
findTrans tlook = do
  graph <- askGraph
  liftE $ lookupTransaction graph tlook

-- OUTDATED a previous attempt at this function attempted to convert TransGraphRelationalExpr to RelationalExpr by resolving the transaction lookups. However, there is no way to resolve a FunctionAtomExpr to an Atom without fully evaluating the higher levels (TupleExpr, etc.). An anonymous function expression cannot be serialized, so that workaround is out. Still, I suspect we can reuse the existing static optimizer logic to work on both structures. The current conversion reduces the chance of whole-query optimization due to full-evaluation on top of full-evaluation, so this function would benefit from some re-design.
processTransGraphRelationalExpr :: TransGraphRelationalExpr -> TransGraphEvalMonad GraphRefRelationalExpr
processTransGraphRelationalExpr (MakeRelationFromExprs mAttrExprs tupleExprs) = do
  tupleExprs' <- processTransGraphTupleExprs tupleExprs
  case mAttrExprs of
    Nothing -> pure (MakeRelationFromExprs Nothing tupleExprs')
    Just attrExprs -> do
      attrExprs' <- mapM processTransGraphAttributeExpr attrExprs
      pure (MakeRelationFromExprs (Just attrExprs') tupleExprs')
processTransGraphRelationalExpr (MakeStaticRelation attrs tupSet) = pure (MakeStaticRelation attrs tupSet)
processTransGraphRelationalExpr (ExistingRelation rel) = pure (ExistingRelation rel)
processTransGraphRelationalExpr (RelationVariable rvname transLookup) = 
  RelationVariable rvname <$> findTransId transLookup
processTransGraphRelationalExpr (Project transAttrNames expr) = 
  Project <$> processTransGraphAttributeNames transAttrNames <*> processTransGraphRelationalExpr expr
processTransGraphRelationalExpr (Union exprA exprB) =
  Union <$> processTransGraphRelationalExpr exprA <*> processTransGraphRelationalExpr exprB
processTransGraphRelationalExpr (Join exprA exprB) =
  Join <$> processTransGraphRelationalExpr exprA <*> processTransGraphRelationalExpr exprB
processTransGraphRelationalExpr (Rename attrName1 attrName2 expr) =
  Rename attrName1 attrName2 <$> processTransGraphRelationalExpr expr
processTransGraphRelationalExpr (Difference exprA exprB) =
  Difference <$> processTransGraphRelationalExpr exprA <*> processTransGraphRelationalExpr exprB
processTransGraphRelationalExpr (Group transAttrNames attrName expr) = 
  Group <$>
    processTransGraphAttributeNames transAttrNames <*>
    pure attrName <*>
    processTransGraphRelationalExpr expr
processTransGraphRelationalExpr (Ungroup attrName expr) = 
  Ungroup attrName <$> processTransGraphRelationalExpr expr
processTransGraphRelationalExpr (Restrict predicateExpr expr) =
  Restrict <$> evalTransGraphRestrictionPredicateExpr predicateExpr <*>
    processTransGraphRelationalExpr expr
processTransGraphRelationalExpr (Equals exprA exprB) = do  
  exprA' <- processTransGraphRelationalExpr exprA
  exprB' <- processTransGraphRelationalExpr exprB 
  pure (Equals exprA' exprB')
processTransGraphRelationalExpr (NotEquals exprA exprB) = do  
  exprA' <- processTransGraphRelationalExpr exprA 
  exprB' <- processTransGraphRelationalExpr exprB 
  pure (NotEquals exprA' exprB')
processTransGraphRelationalExpr (Extend extendExpr expr) = do
  extendExpr' <- processTransGraphExtendTupleExpr extendExpr 
  expr' <- processTransGraphRelationalExpr expr 
  pure (Extend extendExpr' expr')
processTransGraphRelationalExpr (With views expr) = do
  evaldViews <- mapM (\(wnexpr, vexpr) -> do
                         wnexpr' <- processTransGraphWithNameExpr wnexpr
                         vexpr' <- processTransGraphRelationalExpr vexpr 
                         pure (wnexpr', vexpr')
                     ) views
  expr' <- processTransGraphRelationalExpr expr 
  pure (With evaldViews expr')

processTransGraphTupleExprs :: TransGraphTupleExprs -> TransGraphEvalMonad GraphRefTupleExprs
processTransGraphTupleExprs (TupleExprs marker texprs) =
  TupleExprs <$> findTransId marker <*> mapM processTransGraphTupleExpr texprs

processTransGraphTupleExpr :: TransGraphTupleExpr -> TransGraphEvalMonad GraphRefTupleExpr
processTransGraphTupleExpr (TupleExpr attrMap) = do
  let attrAssoc = mapM (\(attrName, atomExpr) -> do 
                        aExpr <- processTransGraphAtomExpr atomExpr
                        pure (attrName, aExpr)
                    ) (M.toList attrMap)
  TupleExpr . M.fromList <$> attrAssoc
  
processTransGraphAtomExpr :: TransGraphAtomExpr -> TransGraphEvalMonad GraphRefAtomExpr
processTransGraphAtomExpr (AttributeAtomExpr aname) = pure $ AttributeAtomExpr aname
processTransGraphAtomExpr (NakedAtomExpr atom) = pure $ NakedAtomExpr atom
processTransGraphAtomExpr (FunctionAtomExpr funcName args tLookup) =
  FunctionAtomExpr funcName <$> mapM processTransGraphAtomExpr args <*> findTransId tLookup
processTransGraphAtomExpr (RelationAtomExpr expr) =
  RelationAtomExpr <$> processTransGraphRelationalExpr expr
processTransGraphAtomExpr (ConstructedAtomExpr dConsName args tLookup) =
  ConstructedAtomExpr dConsName <$> mapM processTransGraphAtomExpr args <*> findTransId tLookup
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
  let expr' = processTransGraphRelationalExpr expr
  RelationalExprPredicate <$> expr'
evalTransGraphRestrictionPredicateExpr (AtomExprPredicate expr) = do
  let expr' = processTransGraphAtomExpr expr
  AtomExprPredicate <$> expr'
evalTransGraphRestrictionPredicateExpr (AttributeEqualityPredicate attrName expr) = do  
  let expr' = processTransGraphAtomExpr expr
  AttributeEqualityPredicate attrName <$> expr'
  
processTransGraphExtendTupleExpr :: TransGraphExtendTupleExpr -> TransGraphEvalMonad GraphRefExtendTupleExpr
processTransGraphExtendTupleExpr (AttributeExtendTupleExpr attrName expr) =
  AttributeExtendTupleExpr attrName <$> processTransGraphAtomExpr expr

processTransGraphAttributeExpr :: TransGraphAttributeExpr -> TransGraphEvalMonad GraphRefAttributeExpr
processTransGraphAttributeExpr (AttributeAndTypeNameExpr attrName tCons tLookup) =
  AttributeAndTypeNameExpr attrName tCons <$> findTransId tLookup
processTransGraphAttributeExpr (NakedAttributeExpr attr) = pure (NakedAttributeExpr attr)  

processTransGraphAttributeNames :: TransGraphAttributeNames -> TransGraphEvalMonad GraphRefAttributeNames
processTransGraphAttributeNames (AttributeNames names) = pure (AttributeNames names)
processTransGraphAttributeNames (InvertedAttributeNames names) = pure (InvertedAttributeNames names)
processTransGraphAttributeNames (UnionAttributeNames namesA namesB) = do
  nA <- processTransGraphAttributeNames namesA 
  nB <- processTransGraphAttributeNames namesB
  pure (UnionAttributeNames nA nB)
processTransGraphAttributeNames (IntersectAttributeNames namesA namesB) = do
  nA <- processTransGraphAttributeNames namesA 
  nB <- processTransGraphAttributeNames namesB 
  pure (IntersectAttributeNames nA nB)
processTransGraphAttributeNames (RelationalExprAttributeNames expr) = do
  evaldExpr <- processTransGraphRelationalExpr expr 
  pure (RelationalExprAttributeNames evaldExpr)

processTransGraphWithNameExpr :: TransGraphWithNameExpr -> TransGraphEvalMonad GraphRefWithNameExpr
processTransGraphWithNameExpr (WithNameExpr rvname tlookup) =
  WithNameExpr rvname <$> findTransId tlookup

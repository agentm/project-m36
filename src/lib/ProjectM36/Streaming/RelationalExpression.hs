module ProjectM36.Streaming.RelationalExpression where
import ProjectM36.Base
import ProjectM36.Streaming.Tuple
import Streamly
import Control.Monad.Catch
import qualified Data.Set as S

--this will become more useful once we have multiple join strategies, etc.
data RelExprExecPlan = StreamTuplesFromFilePlan FilePath |
                       ReadTuplesFromMemoryPlan RelationTupleSet |
                       
                       RestrictTupleStreamPlan RestrictionFilter RelExprExprPlan |
                       ProjectTupleStreamPlan (S.Set AttributeName) RelExprExecPlan |
                       
                       UnionTupleStreamsPlan ReadTuplesPlan RelExprPlan |
                       JoinTupleStreamsPlan RelExprExecPlan RelExprExecPlan |
                       DifferenceStreamsPlan RelExprExecPlan RelExprExecPlan
                       
                      
planRelationalExpr :: MonadAsync m => RelationalExpr -> M.Map RelVarName RelExprPlan -> RelationalExprStateElems -> Either RelationalError RelExprExecPlan
planRelationalExpr (RelationVariable name _) rvMap _ = case M.lookup name rvMap of
  Nothing -> Left (RelVarNotDefinedError name)
  Just plan -> Right plan
  
planRelationalExpr (Project attrNames expr) rvMap state = do
  attrNameSet <- evalAttributeNames attrNames expr  
  subExpr <- planRelationalExpr expr rvMap state
  pure (ProjectTupleStreamPlan attrNameSet subExpr)
  
planRelationalExpr (Union exprA exprB) rvMap state = do  
  planA <- planRelationalExpr exprA rvMap state
  planB <- planRelationalExpr exprB rvMap state
  pure (UnionTupleStreamPlan planA planB)
  
planRelationalExpr (Join exprA exprB) rvMap state = do  
  planA <- planRelationalExpr exprA rvMap state
  planB <- planRelationalExpr exprB rvMap state
  pure (JoinTupleStreamsPlan planA planB)
  
planRelationalExpr (Difference exprA exprB) rvMap state = do
  planA <- planRelationalExpr exprA rvMap state
  planB <- planRelationalExpr exprB rvMap state
  pure (DifferenceStreamsPlan planA planB)

planRelationalExpr (MakeStaticRelation attributeSet tupleSet) = do
  
  
evalRelationalExpr (MakeRelationFromExprs mAttrExprs tupleExprs) = do  
  
evalRelationalExpr (ExistingRelation rel) = pure (Right rel)  

evalRelationalExpr (Rename oldAttrName newAttrName relExpr) = do
  
evalRelationalExpr (Group oldAttrNames newAttrName relExpr) = do  
  
evalRelationalExpr (Ungroup attrName relExpr) = do
  
evalRelationalExpr (Restrict predicateExpr relExpr) = do  
  
evalRelationalExpr (Equals relExprA relExprB) = do  
  
evalRelationalExpr (NotEquals relExprA relExprB) = do  
  
evalRelationalExpr (Extend tupleExpression relExpr) = do  
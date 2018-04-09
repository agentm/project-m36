module ProjectM36.Streaming.RelationalExpression where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.RelationalExpression
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.Trans.Reader

type RelExprExecPlan = RelExprExecPlanBase ()

--this will become more useful once we have multiple join strategies, etc.
data RelExprExecPlanBase a = StreamTuplesFromFilePlan FilePath |
                       ReadTuplesFromMemoryPlan RelationTupleSet |
                       
                       RestrictTupleStreamPlan (RestrictionPredicateExprBase a) (RelExprExecPlanBase a) |
                       ProjectTupleStreamPlan (S.Set AttributeName) (RelExprExecPlanBase a) |
                       RenameTupleStreamPlan AttributeName AttributeName (RelExprExecPlanBase a) |
                       GroupTupleStreamPlan (AttributeNamesBase a) AttributeName (RelExprExecPlanBase a) |
                       UngroupTupleStreamPlan AttributeName (RelExprExecPlanBase a) |
                       ExtendTupleStreamPlan (ExtendTupleExprBase a) (RelExprExecPlanBase a) |
                       
                       UnionTupleStreamsPlan (RelExprExecPlanBase a) (RelExprExecPlanBase a) |
                       JoinTupleStreamsPlan (RelExprExecPlanBase a) (RelExprExecPlanBase a) |
                       DifferenceStreamsPlan (RelExprExecPlanBase a) (RelExprExecPlanBase a) |
                       EqualTupleStreamsPlan (RelExprExecPlanBase a) (RelExprExecPlanBase a) |
                       NotEqualTupleStreamsPlan (RelExprExecPlanBase a) (RelExprExecPlanBase a) |
                       
                       MakeStaticRelationPlan Attributes RelationTupleSet |
                       MakeRelationFromExprsPlan (Maybe [AttributeExprBase a]) [TupleExprBase a] |
                       ExistingRelationPlan Relation
                      
planRelationalExpr :: RelationalExpr -> 
                      M.Map RelVarName RelExprExecPlan -> 
                      RelationalExprStateElems -> 
                      Either RelationalError RelExprExecPlan
planRelationalExpr (RelationVariable name _) rvMap _ = case M.lookup name rvMap of
  Nothing -> Left (RelVarNotDefinedError name)
  Just plan -> Right plan
  
planRelationalExpr (Project attrNames expr) rvMap state = do
  attrNameSet <- runReader (evalAttributeNames attrNames expr) state
  subExpr <- planRelationalExpr expr rvMap state
  pure (ProjectTupleStreamPlan attrNameSet subExpr)
  
planRelationalExpr (Union exprA exprB) rvMap state = do  
  planA <- planRelationalExpr exprA rvMap state
  planB <- planRelationalExpr exprB rvMap state
  pure (UnionTupleStreamsPlan planA planB)
  
planRelationalExpr (Join exprA exprB) rvMap state = do  
  planA <- planRelationalExpr exprA rvMap state
  planB <- planRelationalExpr exprB rvMap state
  pure (JoinTupleStreamsPlan planA planB)
  
planRelationalExpr (Difference exprA exprB) rvMap state = do
  planA <- planRelationalExpr exprA rvMap state
  planB <- planRelationalExpr exprB rvMap state
  pure (DifferenceStreamsPlan planA planB)

planRelationalExpr (MakeStaticRelation attributeSet tupleSet) _ _ = pure (MakeStaticRelationPlan attributeSet tupleSet)
  
planRelationalExpr (MakeRelationFromExprs mAttrExprs tupleExprs) _ _ = pure (MakeRelationFromExprsPlan mAttrExprs tupleExprs)
  
planRelationalExpr (ExistingRelation rel) _ _ = pure (ExistingRelationPlan rel)  

planRelationalExpr (Rename oldAttrName newAttrName relExpr) rvMap state = 
  RenameTupleStreamPlan oldAttrName newAttrName <$> planRelationalExpr relExpr rvMap state
  
planRelationalExpr (Group oldAttrNames newAttrName relExpr) rvMap state = 
  GroupTupleStreamPlan oldAttrNames newAttrName <$> planRelationalExpr relExpr rvMap state
  
planRelationalExpr (Ungroup attrName relExpr) rvMap state = 
  UngroupTupleStreamPlan attrName <$> planRelationalExpr relExpr rvMap state
  
planRelationalExpr (Restrict predicateExpr relExpr) rvMap state =
  RestrictTupleStreamPlan predicateExpr <$> planRelationalExpr relExpr rvMap state
  
planRelationalExpr (Equals relExprA relExprB) rvMap state =   
  EqualTupleStreamsPlan <$> planRelationalExpr relExprA rvMap state <*> planRelationalExpr relExprB rvMap state
  
planRelationalExpr (NotEquals relExprA relExprB) rvMap state =
  NotEqualTupleStreamsPlan <$> planRelationalExpr relExprA rvMap state <*> planRelationalExpr relExprB rvMap state
  
planRelationalExpr (Extend tupleExpression relExpr) rvMap state = 
  ExtendTupleStreamPlan tupleExpression <$> planRelationalExpr relExpr rvMap state
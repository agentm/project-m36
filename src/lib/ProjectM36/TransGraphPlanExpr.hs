module ProjectM36.TransGraphPlanExpr where
import ProjectM36.Base
-- used to create an execution plan which can potentially read from the query cache

data TransGraphPlanExpr = ExecuteTransGraphRelationalExprPlan TransGraphRelationalExpr

module ProjectM36.Streaming.Executor where
import ProjectM36.Streaming.RelationalExpression
import Streamly

type TupleStream = TupleStream IO RelationTuple

execRelExprPlan :: RelExprExecPlan -> TupleStream
execRelExprPlan (StreamTuplesFromFilePlan path) = 
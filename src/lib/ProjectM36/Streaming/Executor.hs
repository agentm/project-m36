module ProjectM36.Streaming.Executor where
import ProjectM36.Streaming.RelationalExpression
import Streamly

type TupleStream = TupleStream IO RelationTuple

execRelExprPlan :: RelExprExecPlan -> TupleStream

--read tuples from a cache streaming blocks
execRelExprPlan (StreamTuplesFromFilePlan path) = 
  undefined

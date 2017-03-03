module ProjectM36.RelationalExpressionState where
import ProjectM36.Base
import ProjectM36.Tuple
import Control.Monad.State
import qualified Data.Map as M
import qualified ProjectM36.Attribute as A

type RelationalExprState a = State RelationalExprStateElems a

-- we need to pass around a higher level RelationTuple and Attributes in order to solve #52
data RelationalExprStateElems = RelationalExprStateTupleElems DatabaseContext RelationTuple | -- used when fully evaluating a relexpr
                                RelationalExprStateAttrsElems DatabaseContext Attributes | --used when evaluating the type of a relexpr
                                RelationalExprStateElems DatabaseContext --used by default at the top level of evaluation
                                
instance Show RelationalExprStateElems where                                
  show (RelationalExprStateTupleElems _ tup) = "RelationalExprStateTupleElems " ++ show tup
  show (RelationalExprStateAttrsElems _ attrs) = "RelationalExprStateAttrsElems" ++ show attrs
  show (RelationalExprStateElems _) = "RelationalExprStateElems"
                                
mkRelationalExprState :: DatabaseContext -> RelationalExprStateElems
mkRelationalExprState ctx = RelationalExprStateElems ctx

mergeTuplesIntoRelationalExprState :: RelationTuple -> RelationalExprStateElems -> RelationalExprStateElems
mergeTuplesIntoRelationalExprState tupIn (RelationalExprStateElems ctx) = RelationalExprStateTupleElems ctx tupIn
mergeTuplesIntoRelationalExprState _ st@(RelationalExprStateAttrsElems _ _) = st
mergeTuplesIntoRelationalExprState tupIn (RelationalExprStateTupleElems ctx existingTuple) = let mergedTupMap = M.union (tupleToMap tupIn) (tupleToMap existingTuple) in
  RelationalExprStateTupleElems ctx (mkRelationTupleFromMap mergedTupMap)
  
mergeAttributesIntoRelationalExprState :: Attributes -> RelationalExprStateElems -> RelationalExprStateElems
mergeAttributesIntoRelationalExprState attrs (RelationalExprStateElems ctx) = RelationalExprStateAttrsElems ctx attrs
mergeAttributesIntoRelationalExprState _ st@(RelationalExprStateTupleElems _ _) = st
mergeAttributesIntoRelationalExprState attrsIn (RelationalExprStateAttrsElems ctx attrs) = RelationalExprStateAttrsElems ctx (A.union attrsIn attrs)

stateContext :: RelationalExprStateElems -> DatabaseContext
stateContext (RelationalExprStateTupleElems ctx _) = ctx
stateContext (RelationalExprStateElems ctx) = ctx
stateContext (RelationalExprStateAttrsElems ctx _) = ctx

setStateContext :: RelationalExprStateElems -> DatabaseContext -> RelationalExprStateElems
setStateContext (RelationalExprStateTupleElems _ tup) ctx = RelationalExprStateTupleElems ctx tup
setStateContext (RelationalExprStateElems _) ctx = RelationalExprStateElems ctx
setStateContext (RelationalExprStateAttrsElems _ attrs) ctx = RelationalExprStateAttrsElems ctx attrs

module RelationTupleSet where
import RelationType
import RelationTuple
import RelationAttribute
import RelationalError
import qualified Data.Map as M
import qualified Data.HashSet as HS
import Control.Monad

emptyTupleSet = HS.empty
emptyAttributeSet = M.empty  

--ensure that all maps have the same keys and key count

verifyRelationTupleSet :: Attributes -> RelationTupleSet -> Either RelationalError RelationTupleSet
verifyRelationTupleSet attrs tupleSet = do
  --check that all tuples have the same types
  tupleList <- forM (HS.toList tupleSet) verifyTuple
  return $ HS.fromList tupleList
  where 
    verifyTuple tuple = if tupleAttributes tuple == attrs then
                          Right tuple
                        else
                          Left $ TupleAttributeTypeMismatchError (attributesDifference (tupleAttributes tuple) attrs)

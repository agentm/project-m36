module ProjectM36.TupleSet where
import ProjectM36.Base
import ProjectM36.Tuple
import ProjectM36.Attribute
import ProjectM36.Error
import qualified Data.Map as M
import qualified Data.HashSet as HS
import Control.Monad

emptyTupleSet :: RelationTupleSet
emptyTupleSet = HS.empty

emptyAttributeSet :: Attributes
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

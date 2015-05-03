module ProjectM36.TupleSet where
import ProjectM36.Base
import ProjectM36.Tuple
import ProjectM36.Error
import qualified Data.HashSet as HS
import Control.Monad
import qualified Data.Vector as V

emptyTupleSet :: RelationTupleSet
emptyTupleSet = HS.empty

singletonTupleSet :: RelationTupleSet
singletonTupleSet = HS.singleton emptyTuple

--ensure that all maps have the same keys and key count

verifyTupleSet :: Attributes -> RelationTupleSet -> Either RelationalError RelationTupleSet
verifyTupleSet attrs tupleSet = do
  --check that all tuples have the same types
  tupleList <- forM (HS.toList tupleSet) verifyTuple
  return $ HS.fromList tupleList

mkTupleSet :: Attributes -> [RelationTuple] -> Either RelationalError RelationTupleSet
mkTupleSet attrs tuples = verifyTupleSet attrs tupSet
  where 
    tupSet = HS.fromList tuples

mkTupleSetFromList :: Attributes -> [[Atom]] -> Either RelationalError RelationTupleSet
mkTupleSetFromList attrs atomMatrix = verifyTupleSet attrs tupSet
  where
    tupSet = HS.fromList $ map (\atomList -> mkRelationTuple attrs (V.fromList atomList)) atomMatrix
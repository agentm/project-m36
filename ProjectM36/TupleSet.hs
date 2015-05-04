module ProjectM36.TupleSet where
import ProjectM36.Base
import ProjectM36.Tuple
import ProjectM36.Error
import qualified Data.HashSet as HS
--import Control.Monad (forM)
import qualified Data.Vector as V
import qualified Control.Parallel.Strategies as P
import Data.Either

emptyTupleSet :: RelationTupleSet
emptyTupleSet = HS.empty

singletonTupleSet :: RelationTupleSet
singletonTupleSet = HS.singleton emptyTuple

--ensure that all maps have the same keys and key count

verifyTupleSet :: Attributes -> RelationTupleSet -> Either RelationalError RelationTupleSet
verifyTupleSet attrs tupleSet = do
  --check that all tuples have the same attributes and that the atom types match
  let tupleList = (map (verifyTuple attrs) (HS.toList tupleSet)) `P.using` P.parListChunk chunkSize P.rdeepseq
      chunkSize = HS.size tupleSet `div` 24                                                                                     
  --let tupleList = P.parMap P.rdeepseq (verifyTuple attrs) (HS.toList tupleSet)
  if length (lefts tupleList) > 0 then
    Left $ head (lefts tupleList)
   else
     return $ HS.fromList (rights tupleList)

mkTupleSet :: Attributes -> [RelationTuple] -> Either RelationalError RelationTupleSet
mkTupleSet attrs tuples = verifyTupleSet attrs tupSet
  where 
    tupSet = HS.fromList tuples

mkTupleSetFromList :: Attributes -> [[Atom]] -> Either RelationalError RelationTupleSet
mkTupleSetFromList attrs atomMatrix = verifyTupleSet attrs tupSet
  where
    tupSet = HS.fromList $ map (\atomList -> mkRelationTuple attrs (V.fromList atomList)) atomMatrix
module ProjectM36.TupleSet where
import ProjectM36.Base
import ProjectM36.Tuple
import ProjectM36.Error
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Control.Parallel.Strategies as P
import Data.Either

import Debug.Trace

emptyTupleSet :: RelationTupleSet
emptyTupleSet = RelationTupleSet []

singletonTupleSet :: RelationTupleSet
singletonTupleSet = RelationTupleSet [emptyTuple]

--ensure that all maps have the same keys and key count

verifyTupleSet :: Attributes -> RelationTupleSet -> Either RelationalError RelationTupleSet
verifyTupleSet attrs tupleSet = do
  --check that all tuples have the same attributes and that the atom types match
  let tupleList = map (verifyTuple attrs) (asList tupleSet) `P.using` P.parListChunk chunkSize P.r0
      chunkSize = (length . asList) tupleSet `div` 24
  --let tupleList = P.parMap P.rdeepseq (verifyTuple attrs) (HS.toList tupleSet)
  if not (null (lefts tupleList)) then
    Left $ head (lefts tupleList)
   else
     return $ RelationTupleSet $ (HS.toList . HS.fromList) (rights tupleList)

mkTupleSet :: Attributes -> [RelationTuple] -> Either RelationalError RelationTupleSet
mkTupleSet attrs tuples = verifyTupleSet attrs (RelationTupleSet tuples)

mkTupleSetFromList :: Attributes -> [[Atom]] -> Either RelationalError RelationTupleSet
mkTupleSetFromList attrs atomMatrix = mkTupleSet attrs $ map (mkRelationTuple attrs . V.fromList) atomMatrix


-- | Target attributes must match the first tuple set.
tupleSetUnion :: Attributes -> RelationTupleSet -> RelationTupleSet -> RelationTupleSet
tupleSetUnion targetAttrs tupSet1 tupSet2 = RelationTupleSet $ HS.toList . HS.fromList $ asList tupSet1 ++ map (reorderTuple targetAttrs) (asList tupSet2)

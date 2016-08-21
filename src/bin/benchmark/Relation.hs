import Criterion.Main
import ProjectM36.Relation
import ProjectM36.Base
import ProjectM36.Error
import qualified ProjectM36.Attribute as A
import qualified Data.Text as T
import qualified Data.Vector as V

-- returns a relation with tupleCount tuples with a set of integer attributes attributesCount long
-- this is useful for performance and resource usage testing
matrixRelation :: Int -> Int -> Either RelationalError Relation
matrixRelation attributeCount tupleCount = do
  let attrs = A.attributesFromList $ map (\c-> Attribute (T.pack $ "a" ++ show c) IntAtomType) [0 .. attributeCount-1]
      tuple tupleX = RelationTuple attrs (V.generate attributeCount (\_ -> IntAtom tupleX))
      tuples = map (\c -> tuple c) [0 .. tupleCount]
  mkRelationDeferVerify attrs (RelationTupleSet tuples)

main :: IO ()
main = defaultMain [
  bgroup "Big Relation" [ 
     bench "100" $ whnf (matrixRelation 10) 100,
     bench "1000" $ whnf (matrixRelation 10) 1000,
     bench "10000" $ whnf (matrixRelation 10) 10000
     ]
  ]
       


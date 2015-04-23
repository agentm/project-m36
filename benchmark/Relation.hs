import Criterion.Main
import ProjectM36.Relation

main :: IO ()
main = defaultMain [
  bgroup "Big Relation" [ 
     bench "100" $ whnf (matrixRelation 10) 100,
     bench "1000" $ whnf (matrixRelation 10) 1000,
     bench "10000" $ whnf (matrixRelation 10) 10000,
     ]
  ]
       


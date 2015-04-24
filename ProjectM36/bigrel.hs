{-# LANGUAGE FlexibleInstances #-}
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.Tuple
import ProjectM36.Error
import ProjectM36.Relation.Show.CSV
import qualified Data.Map as M
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.IntMap as IM
import qualified Data.Hashable as Hash
import qualified Data.Vector as V
import Control.DeepSeq
                     
dumpcsv :: Relation -> IO ()
dumpcsv = BS.putStrLn . relationAsCSV

main = vectorMatrixRun 
       --intmapMatrixRun
  
-- takes 30 minutes to run and 1.1 GB       
matrixRun = do  
  case matrixRelation 100 100000 of
    Left err -> putStrLn (show err)
    Right rel -> case restrict (\(RelationTuple tupMap) -> tupMap M.! "0" == IntAtom 5) rel of
      Left err -> putStrLn (show err)
      Right rel -> dumpcsv rel
      
intmapMatrixRun = do      
  let matrix = intmapMatrixRelation 100 100000
  putStrLn (show matrix)
  
--compare IntMap speed and size
--this is about 3 times faster (9 minutes) for 10x100000 and uses 800 MB
intmapMatrixRelation :: Int -> Int -> HS.HashSet (IM.IntMap Atom)
intmapMatrixRelation attributeCount tupleCount = HS.fromList $ map mapper [0..tupleCount]
  where
    mapper tupCount = IM.fromList $ map (\c-> (c, IntAtom tupCount)) [0..attributeCount]

instance Hash.Hashable (IM.IntMap Atom) where
  hashWithSalt salt tupMap = Hash.hashWithSalt salt (show tupMap)

vectorMatrixRun = do
  let matrix = vectorMatrixRelation 100 100000
  putStrLn (show matrix)

-- 20 s 90 MBs- a clear win- ideal size is 10 * 100000 * 8 bytes = 80 MB! without IntAtom wrapper
--with IntAtom wrapper: 1m12s 90 MB
vectorMatrixRelation :: Int -> Int -> HS.HashSet (V.Vector Atom)
vectorMatrixRelation attributeCount tupleCount = HS.fromList $ map mapper [0..tupleCount]
  where
    mapper tupCount = V.replicate attributeCount (IntAtom tupCount)
    
instance Hash.Hashable (V.Vector Atom) where    
  hashWithSalt salt vec = Hash.hashWithSalt salt (show vec)
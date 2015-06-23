{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.Relation.Show.Gnuplot where
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.Tuple
import qualified ProjectM36.Attribute as A
import Graphics.EasyPlot
import qualified Data.Vector as V

--this module support plotting relations containing integer attributes with arity 1,2, or 3 only
--nested relations?

data PlotError = InvalidAttributeCountError |
                 InvalidAttributeTypeError 
                 deriving (Show)
                 
--plotRelation :: Relation -> Either PlotError 

intFromAtomIndex :: Int -> RelationTuple -> Int
intFromAtomIndex index tup = (\(IntAtom i) -> i) $ (tupleAtoms tup) V.! index

graph1DRelation :: Relation -> Graph2D Double Double
graph1DRelation rel = Data2D [Title "rel"] [] $ map (\i -> (fromIntegral i, 0.0)) (points1DRelation rel)
    
points1DRelation :: Relation -> [Int]
points1DRelation rel = relFold folder [] rel
  where    
    folder tup acc = intFromAtomIndex 0 tup : acc

graph2DRelation :: Relation -> Graph2D Double Double
graph2DRelation rel = Data2D [Title "rel"] [Step 1.0] $ map (\(x,y) -> (fromIntegral x, fromIntegral y)) $ points2DRelation rel

points2DRelation :: Relation -> [(Int, Int)]
points2DRelation rel = relFold folder [] rel
  where
    folder tup acc = (intFromAtomIndex 0 tup, intFromAtomIndex 1 tup) : acc

graph3DRelation :: Relation -> Graph3D Double Double Double
graph3DRelation rel = Data3D [Title "rel"] [StepX 1.0, StepY 1.0] $ map (\(x,y,z) -> (fromIntegral x, fromIntegral y, fromIntegral z)) $ points3DRelation rel
                                                                         
points3DRelation :: Relation -> [(Int, Int, Int)]                      
points3DRelation rel = relFold folder [] rel
  where
    folder tup acc = (intFromAtomIndex 0 tup, intFromAtomIndex 1 tup, intFromAtomIndex 2 tup) : acc

sample1DRelation = case mkRelationFromList (A.attributesFromList [Attribute "x" IntAtomType]) [[IntAtom 2], [IntAtom 3]] of
  Right rel -> rel
  Left _ -> undefined
  
sample2dRelation = case mkRelationFromList (A.attributesFromList [Attribute "x" IntAtomType, Attribute "y" IntAtomType]) [[IntAtom 2, IntAtom 3],   
                             [IntAtom 2, IntAtom 4]] of                                                                                             
                     Right rel -> rel
                     Left _ -> undefined
                     
sample3dRelation = case mkRelationFromList (A.attributesFromList [Attribute "x" IntAtomType, Attribute "y" IntAtomType, Attribute "z" IntAtomType]) [[IntAtom 2, IntAtom 3, IntAtom 3],   
                             [IntAtom 2, IntAtom 4, IntAtom 4]] of                                                                                             
                     Right rel -> rel
                     Left _ -> undefined
                     
type G2D = Graph2D Double Double                     
type G3D = Graph3D Double Double Double

plotRelation :: Relation -> Either PlotError (Either G2D G3D)
plotRelation rel = let attrTypes = V.replicate (arity rel) IntAtomType in
  if attrTypes /= A.atomTypes (attributes rel) then
    Left InvalidAttributeTypeError
  else
    case arity rel of
      1 -> Right $ Left $ graph1DRelation rel
      2 -> Right $ Left $ graph2DRelation rel
      3 -> Right $ Right $ graph3DRelation rel
      _ -> Left InvalidAttributeCountError
      
--PNG
savePlottedRelation :: String -> Relation -> IO ()
savePlottedRelation path rel = case plotRelation rel of 
  Left err -> putStrLn (show err)
  Right (Left g2d) -> plot' [] (PNG path) g2d >> return ()
  Right (Right g3d) -> plot' [] (PNG path) g3d >> return ()
  
--X11  
showPlottedRelation :: Relation -> IO ()
showPlottedRelation rel = case plotRelation rel of 
  Left err -> putStrLn (show err)
  Right (Left g2d) -> plot' [] X11 g2d >> return ()
  Right (Right g3d) -> plot' [] X11 g3d >> return ()
  
                     
ezplot :: Relation -> IO ()                     
ezplot rel = do
  case plotRelation rel of
    Left err -> putStrLn $ show err
    Right (Left g2d) -> plot' [Interactive] X11 g2d >> return ()
    Right (Right g3d) -> plot' [Interactive] X11 g3d >> return ()


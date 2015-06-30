{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.Relation.Show.Gnuplot where
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.Tuple
import qualified ProjectM36.Attribute as A
import qualified Data.Vector as V
import qualified Graphics.Gnuplot.Simple as GP
import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import qualified Graphics.Gnuplot.Advanced as GPA

--this module support plotting relations containing integer attributes with arity 1,2, or 3 only
--nested relations?

data PlotError = InvalidAttributeCountError |
                 InvalidAttributeTypeError 
                 deriving (Show)
                 
--plotRelation :: Relation -> Either PlotError 

intFromAtomIndex :: Int -> RelationTuple -> Int
intFromAtomIndex index tup = (\(IntAtom i) -> i) $ (tupleAtoms tup) V.! index

pointStyle :: GP.PlotStyle
pointStyle = GP.PlotStyle { GP.plotType = GP.Points,
                            GP.lineSpec = GP.CustomStyle [GP.PointSize 2.0]}

graph1DRelation :: Relation -> IO ()
graph1DRelation rel = GP.plotPathStyle [] pointStyle $ map (\i -> (i, 0)) (points1DRelation rel)
    
points1DRelation :: Relation -> [Int]
points1DRelation rel = relFold folder [] rel
  where    
    folder tup acc = intFromAtomIndex 0 tup : acc

graph2DRelation :: Relation -> IO ()
graph2DRelation rel = GP.plotPathStyle [] pointStyle $ points2DRelation rel

points2DRelation :: Relation -> [(Int, Int)]
points2DRelation rel = relFold folder [] rel
  where
    folder tup acc = (intFromAtomIndex 0 tup, intFromAtomIndex 1 tup) : acc

graph3DRelation :: Relation -> IO ()
graph3DRelation rel = do
  _ <- GPA.plotDefault $ Plot3D.cloud Graph3D.points $ points3DRelation rel
  return ()
                                                                         
points3DRelation :: Relation -> [(Int, Int, Int)]                      
points3DRelation rel = relFold folder [] rel
  where
    folder tup acc = (intFromAtomIndex 0 tup, intFromAtomIndex 1 tup, intFromAtomIndex 2 tup) : acc

{-
sample1DRelation = case mkRelationFromList (A.attributesFromList [Attribute "x" IntAtomType]) [[IntAtom 2], [IntAtom 3]] of
  Right rel -> rel
  Left _ -> undefined
  
sample2DRelation = case mkRelationFromList (A.attributesFromList [Attribute "x" IntAtomType, Attribute "y" IntAtomType]) [[IntAtom 2, IntAtom 3],   
                                                                                                                          [IntAtom 2, IntAtom 4]] of                                                                                             
                     Right rel -> rel
                     Left _ -> undefined
                     
sample3DRelation = case mkRelationFromList (A.attributesFromList [Attribute "x" IntAtomType, Attribute "y" IntAtomType, Attribute "z" IntAtomType]) [[IntAtom 2, IntAtom 3, IntAtom 3],   
                             [IntAtom 2, IntAtom 4, IntAtom 4]] of                                                                                             
                     Right rel -> rel
                     Left _ -> undefined

-}
                     
plotRelation :: Relation -> IO (Maybe PlotError)
plotRelation rel = let attrTypes = V.replicate (arity rel) IntAtomType in
  if attrTypes /= A.atomTypes (attributes rel) then
    return $ Just InvalidAttributeTypeError
  else
    case arity rel of
      1 -> graph1DRelation rel >> return Nothing
      2 -> graph2DRelation rel >> return Nothing 
      3 -> graph3DRelation rel >> return Nothing 
      _ -> return $ Just InvalidAttributeCountError
      
      
{-
--PNG
savePlottedRelation :: String -> Relation -> IO ()
savePlottedRelation path rel = case plotRelation rel of 
  Left err -> putStrLn (show err)
  Right (Left g2d) -> plot' [] (PNG path) g2d >> return ()
  Right (Right g3d) -> plot' [] (PNG path) g3d >> return ()
-}  


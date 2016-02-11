{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.Relation.Show.Gnuplot where
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.Tuple
import ProjectM36.DataTypes.Primitive
import ProjectM36.AtomFunctions.Primitive
import qualified ProjectM36.Attribute as A
import qualified Data.Vector as V

import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import qualified Graphics.Gnuplot.Advanced as GPA

--this module support plotting relations containing integer attributes with arity 1,2, or 3 only
--nested relations?

data PlotError = InvalidAttributeCountError |
                 InvalidAttributeTypeError
                 deriving (Show)

--plotRelation :: Relation -> Either PlotError

intFromAtomIndex :: Int -> RelationTuple -> Int
intFromAtomIndex index tup = (\i -> castInt i) $ (tupleAtoms tup) V.! index

graph1DRelation :: Relation -> Plot2D.T Int Int
graph1DRelation rel = Plot2D.list Graph2D.listPoints $ points1DRelation rel

points1DRelation :: Relation -> [Int]
points1DRelation rel = relFold folder [] rel
  where
    folder tup acc = intFromAtomIndex 0 tup : acc

graph2DRelation :: Relation -> Plot2D.T Int Int
graph2DRelation rel = Plot2D.list Graph2D.points (points2DRelation rel)

points2DRelation :: Relation -> [(Int, Int)]
points2DRelation rel = relFold folder [] rel
  where
    folder tup acc = (intFromAtomIndex 0 tup, intFromAtomIndex 1 tup) : acc

graph3DRelation :: Relation -> Plot3D.T Int Int Int
graph3DRelation rel = do
  Plot3D.cloud Graph3D.points $ points3DRelation rel

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
plotRelation rel = let attrTypes = V.replicate (arity rel) intAtomType in
  if attrTypes /= A.atomTypes (attributes rel) then
    return $ Just InvalidAttributeTypeError
  else do
    case arity rel of
      1 -> (GPA.plotDefault $ graph1DRelation rel) >> return Nothing
      2 -> (GPA.plotDefault $ graph2DRelation rel) >> return Nothing
      3 -> (GPA.plotDefault $ graph3DRelation rel) >> return Nothing
      _ -> return $ Just InvalidAttributeCountError


{-
--PNG
savePlottedRelation :: String -> Relation -> IO ()
savePlottedRelation path rel = case plotRelation rel of
  Left err -> putStrLn (show err)
  Right (Left g2d) -> plot' [] (PNG path) g2d >> return ()
  Right (Right g3d) -> plot' [] (PNG path) g3d >> return ()
-}


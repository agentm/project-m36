{-# LANGUAGE OverloadedStrings #-}
--writes Relation to a String suitable for terminal output
module ProjectM36.Relation.Show.Term where
import ProjectM36.Base
import ProjectM36.Tuple
import ProjectM36.Relation
import qualified Data.Set as S
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Text as T

boxV :: StringType
boxV = "│"
boxH :: StringType
boxH = "─"

boxTL :: StringType
boxTL = "┌"
boxTR :: StringType
boxTR = "┐"
boxBL :: StringType
boxBL = "└"
boxBR :: StringType
boxBR = "┘"

boxLB :: StringType
boxLB = "├"
boxRB :: StringType
boxRB = "┤"
boxTB :: StringType
boxTB = "┬"
boxBB :: StringType
boxBB = "┴"

boxC :: StringType
boxC = "┼"

dboxH :: StringType
dboxH = "═"
dboxL :: StringType
dboxL = "╞"
dboxR :: StringType
dboxR = "╡"

class TermSize a where
  termLength :: a -> Int
  
--represent a relation as a table similar to those drawn by Date  
type Cell = StringType
type Table = ([Cell], [[Cell]]) --header, body

addRow :: [Cell] -> Table -> Table
addRow cells (header,body) = (header, body ++ [cells])

--calculate maximum per-row and per-column sizes    

cellLocations :: Table -> ([Int],[Int]) --column size, row size
cellLocations tab@(header, _) = (maxWidths, maxHeights)
  where
    cellSizeMatrix = cellSizes tab
    maxWidths = foldl mergeMax (baseSize (length header)) (map fst cellSizeMatrix)
    baseSize num = take num (repeat 0)
    rowHeights = map snd cellSizeMatrix
    maxHeights = map (L.maximumBy compare) rowHeights
    mergeMax a b = map (\(c,d) -> max c d) (zip a b)
    
--the normal "lines" function returns an empty list for an empty string which is not what we want
breakLines :: StringType -> [StringType]
breakLines "" = [""]
breakLines x = T.lines x
    
cellSizes :: Table -> [([Int], [Int])]    
cellSizes (header, body) = map (\row -> (map maxRowWidth row, map (length . breakLines) row)) allRows
  where
    maxRowWidth row = L.maximumBy compare (map T.length (breakLines row))
    allRows = [header] ++ body

relationAsTable :: Relation -> Table
relationAsTable rel@(Relation _ tupleSet) = (header, body)
  where
    oAttrs = orderedAttributeNames rel
    header = oAttrs
    body :: [[Cell]]
    body = HS.foldl' tupleFolder [] tupleSet
    tupleFolder acc tuple = acc ++ [map (\attrName -> case atomForAttributeName attrName tuple of
                                            Left _ -> "?"
                                            Right atom -> showAtom atom
                                            ) oAttrs]
    
showAtom :: Atom -> StringType    
showAtom (StringAtom atom) = atom
showAtom (IntAtom atom) = T.pack $ show atom
showAtom (RelationAtom rel) = renderTable $ relationAsTable rel

renderTable :: Table -> StringType
renderTable table = renderHeader table (fst cellLocs) `T.append` renderBody (snd table) cellLocs
  where
    cellLocs = cellLocations table  
    
renderHeader :: Table -> [Int] -> StringType
renderHeader (header, body) columnLocations = renderTopBar `T.append` renderHeaderNames `T.append` renderBottomBar
  where
    renderTopBar = boxTL `T.append` T.concat (L.intersperse boxTB (map (\x -> repeatString x boxH) columnLocations)) `T.append` boxTR `T.append` "\n"
    renderHeaderNames = renderRow header columnLocations 1 boxV
    renderBottomBar = if length body == 0 then ""
                      else renderHBar boxLB boxC boxRB columnLocations `T.append` "\n"
                        
renderHBar :: StringType -> StringType -> StringType -> [Int] -> StringType
renderHBar left middle end columnLocations = left `T.append` T.concat (L.intersperse middle (map (\x -> repeatString x boxH) columnLocations)) `T.append` end

leftPaddedString :: Int -> Int -> StringType -> StringType
leftPaddedString lineNum size str = paddedLines !! lineNum
  where
    paddedLines = map (\line -> line `T.append` repeatString (size - T.length line) " ") (breakLines str)
  
renderRow :: [Cell] -> [Int] -> Int -> StringType -> StringType
renderRow cells columnLocations rowHeight interspersed = T.unlines $ map renderOneLine [0..rowHeight-1]
  where
    renderOneLine lineNum = boxV `T.append` T.concat (L.intersperse interspersed (map (\(size, value) -> leftPaddedString lineNum size value) (zip columnLocations cells))) `T.append` boxV

renderBody :: [[Cell]] -> ([Int],[Int]) -> StringType
renderBody cellMatrix cellLocs = renderRows `T.append` renderBottomBar
  where
    columnLocations = fst cellLocs
    rowLocations = snd cellLocs
    renderRows = T.concat (map (\(row, rowHeight)-> renderRow row columnLocations rowHeight boxV) rowHeightMatrix)
    rowHeightMatrix = zip cellMatrix (tail rowLocations)
    renderBottomBar = renderHBar boxBL boxBB boxBR columnLocations

orderedAttributeNames :: Relation -> [AttributeName]
orderedAttributeNames = S.toAscList . attributeNames
                    
repeatString :: Int -> StringType -> StringType
repeatString c s = T.concat (take c (repeat s))

showRelation :: Relation -> StringType
showRelation rel
  | rel == relationTrue = "true"
  | rel == relationFalse = "false"                     
  | otherwise = renderTable (relationAsTable rel)
  

{-
groupedExample :: Relation
groupedExample = case group (S.fromList ["CITY"]) "CITYREL" s of {Right rel -> rel}
-}

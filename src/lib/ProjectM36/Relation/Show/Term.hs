--writes Relation to a String suitable for terminal output
module ProjectM36.Relation.Show.Term where
import ProjectM36.Base
import ProjectM36.Atom
import ProjectM36.AtomType
import ProjectM36.Tuple
import ProjectM36.Relation
import ProjectM36.Attribute hiding (null)
import qualified Data.List as L
import qualified Data.Text as T
import Control.Arrow hiding (left)
import Data.Monoid
import Data.Char.WCWidth --guess the width that the character will appear as in the terminal

import Debug.Trace

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
    baseSize num = replicate num 0
    rowHeights = map snd cellSizeMatrix
    maxHeights = map (\l -> if null l then 0 else L.maximum l) rowHeights
    mergeMax = zipWith max

--the normal "lines" function returns an empty list for an empty string which is not what we want
breakLines :: StringType -> [StringType]
breakLines "" = [""]
breakLines x = T.lines x

cellSizes :: Table -> [([Int], [Int])]
cellSizes (header, body) = map (map maxRowWidth &&& map (length . breakLines)) allRows
  where
    maxRowWidth row = if null (lengths row) then
                         0
                      else
                        L.maximum (lengths row)
    lengths row = map stringDisplayLength (breakLines row)
    allRows = header : body
    
relationAsTable :: Relation -> Table
relationAsTable rel@(Relation _ tupleSet) = (header, body)
  where
    oAttrs = orderedAttributes (attributes rel)
    oAttrNames = orderedAttributeNames (attributes rel)
    header = map prettyAttribute oAttrs
    body :: [[Cell]]
    body = L.foldl' tupleFolder [] (asList tupleSet)
    tupleFolder acc tuple = acc ++ [map (\attrName -> case atomForAttributeName attrName tuple of
                                            Left _ -> "?"
                                            Right atom -> showAtom 0 atom
                                            ) oAttrNames]

showParens :: Bool -> StringType -> StringType
showParens predicate f = if predicate then
                      "(" `T.append` f `T.append` ")"
                    else
                      f

showAtom :: Int -> Atom -> StringType
showAtom _ (RelationAtom rel) = renderTable $ relationAsTable rel
showAtom level (ConstructedAtom dConsName _ atoms) = showParens (level >= 1 && not (null atoms)) $ T.concat (L.intersperse " " (dConsName : map (showAtom 1) atoms))
showAtom _ (TextAtom t) = "\"" <> t <> "\""
showAtom _ atom = atomToText atom

renderTable :: Table -> StringType
renderTable table = renderHeader table (fst cellLocs) `T.append` renderBody (snd table) cellLocs
  where
    cellLocs = cellLocations table

renderHeader :: Table -> [Int] -> StringType
renderHeader (header, body) columnLocations = renderTopBar `T.append` renderHeaderNames `T.append` renderBottomBar
  where
    renderTopBar = boxTL `T.append` T.concat (L.intersperse boxTB (map (`repeatString` boxH) columnLocations)) `T.append` boxTR `T.append` "\n"
    renderHeaderNames = renderRow header columnLocations 1 boxV
    renderBottomBar = if null body then ""
                      else renderHBar boxLB boxC boxRB columnLocations `T.append` "\n"

renderHBar :: StringType -> StringType -> StringType -> [Int] -> StringType
renderHBar left middle end columnLocations = left `T.append` T.concat (L.intersperse middle (map (`repeatString` boxH) columnLocations)) `T.append` end

--pad a block of potentially multi-lined text
leftPaddedString :: Int -> Int -> StringType -> StringType
leftPaddedString lineNum size str = if lineNum > length paddedLines -1 then
                                      repeatString size " "
                                    else
                                      paddedLines !! lineNum
  where
    paddedLines = map (\line -> line `T.append` repeatString (size - stringDisplayLength line) " ") (breakLines str)

renderRow :: [Cell] -> [Int] -> Int -> StringType -> StringType
renderRow cells columnLocations rowHeight interspersed = T.unlines $ map renderOneLine [0..rowHeight-1]
  where
    renderOneLine lineNum = boxV `T.append` T.concat (L.intersperse interspersed (zipWith (leftPaddedString lineNum) columnLocations cells)) `T.append` boxV

renderBody :: [[Cell]] -> ([Int],[Int]) -> StringType
renderBody cellMatrix cellLocs = renderRows `T.append` renderBottomBar
  where
    columnLocations = fst cellLocs
    rowLocations = snd cellLocs
    renderRows = T.concat (map (\(row, rowHeight)-> renderRow row columnLocations rowHeight boxV) rowHeightMatrix)
    rowHeightMatrix = zip cellMatrix (tail rowLocations)
    renderBottomBar = renderHBar boxBL boxBB boxBR columnLocations

repeatString :: Int -> StringType -> StringType
repeatString c s = T.concat (replicate c s)

showRelation :: Relation -> StringType
showRelation rel = renderTable (relationAsTable rel)

--use wcwidth to guess the string width in the terminal- many CJK characters can take multiple columns in a fixed width font
stringDisplayLength :: StringType -> Int
stringDisplayLength = T.foldr charSize 0 
  where
    charSize char accum = let w = wcwidth char in
      accum + if w < 0 then
        1 
      else
        w 
                                             

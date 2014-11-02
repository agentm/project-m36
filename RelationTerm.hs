--writes Relation to a String suitable for terminal output
module RelationTerm where
import RelationType
import Relation
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.HashSet as HS
import qualified Data.List as L

boxV = "│"
boxH = "─"

boxTL = "┌"
boxTR = "┐"
boxBL = "└"
boxBR = "┘"

boxLB = "├"
boxRB = "┤"
boxTB = "┬"
boxBB = "┴"

boxC = "┼"

dboxH = "═"
dboxL = "╞"
dboxR = "╡"

class TermSize a where
  termLength :: a -> Int
  
--represent a relation as a table similar to those drawn by Date  
type Cell = String
type Table = ([Cell], [[Cell]]) --header, body

addRow :: [Cell] -> Table -> Table
addRow cells (header,body) = (header, body ++ [cells])

columnLocations :: Table -> [Int]
columnLocations (header, body) = foldl sizeFolder (head cellSizes) cellSizes
  where
    cellSizes = map (\row -> map length row) (body ++ [header]) --bar padding
    sizeFolder acc row = mergeMax row acc
    mergeMax a b = map (\(x,y) -> if x > y then x else y) (zip a b)

relationAsTable :: Relation -> Table
relationAsTable rel@(Relation attributes tupleSet) = (header, body)
  where
    oAttrs = orderedAttributeNames rel
    header = oAttrs
    body :: [[Cell]]
    body = HS.foldl' tupleFolder [] tupleSet
    tupleFolder acc (RelationTuple tupMap) = acc ++ [map (\n -> showAtom (tupMap M.! n)) oAttrs]
    
showAtom :: Atom -> String    
showAtom (StringAtom s) = s
showAtom (IntAtom i) = show i
showAtom (RelationAtom rel) = renderTable $ relationAsTable rel

renderTable :: Table -> String
renderTable table = renderHeader table colLocations ++ renderBody (snd table) colLocations
  where
    colLocations = columnLocations table  
    
renderHeader :: Table -> [Int] -> String
renderHeader (header, body) columnLocations = renderTopBar ++ renderHeaderNames ++ renderBottomBar ++ "\n"
  where
    renderTopBar = boxTL ++ concat (L.intersperse boxTB (map (\x -> repeatString x boxH) columnLocations)) ++ boxTR ++ "\n"
    renderHeaderNames = renderRow header columnLocations boxV
    renderBottomBar = if length body == 0 then ""
                      else renderHBar boxLB boxC boxRB columnLocations
                        
renderHBar :: String -> String -> String -> [Int] -> String
renderHBar left middle end columnLocations = left ++ concat (L.intersperse middle (map (\x -> repeatString x boxH) columnLocations)) ++ end

leftPaddedString :: Int -> String -> String
leftPaddedString size str = str ++ repeatString (size - length str) " "
  
renderRow :: [Cell] -> [Int] -> String -> String
renderRow cells columnLocations interspersed = boxV ++ concat (L.intersperse interspersed (map (\(size, value) -> leftPaddedString size value) (zip columnLocations cells))) ++ boxV ++ "\n"

renderBody :: [[Cell]] -> [Int] -> String
renderBody cellMatrix columnLocations = renderRows ++ renderBottomBar
  where
    renderRows = concat (map (\r-> renderRow r columnLocations boxV) cellMatrix)
    renderBottomBar = renderHBar boxBL boxBB boxBR columnLocations

orderedAttributeNames :: Relation -> [AttributeName]
orderedAttributeNames = S.toAscList . attributeNames
                    
repeatString :: Int -> String -> String
repeatString c s = concat (take c (repeat s))

showRelation :: Relation -> String
showRelation = renderTable . relationAsTable
                                  
  
    

  

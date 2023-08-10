{-# LANGUAGE DeriveGeneric, DerivingVia #-}
{- A dataframe is a strongly-typed, ordered list of named tuples. A dataframe differs from a relation in that its tuples are ordered.-}
module ProjectM36.DataFrame where
import ProjectM36.Base
import qualified ProjectM36.Attribute as A hiding (drop)
import ProjectM36.Error
import qualified ProjectM36.Relation as R
import ProjectM36.Relation.Show.Term
import qualified ProjectM36.Relation.Show.HTML as RelHTML
import ProjectM36.DataTypes.Sorting
import ProjectM36.AtomType
import ProjectM36.Atom
import qualified Data.Vector as V
import GHC.Generics
import qualified Data.List as L
import qualified Data.Set as S
import Data.Maybe
import qualified Data.Text as T
import Control.Arrow
import Control.Monad (unless)
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif

data AttributeOrderExpr = AttributeOrderExpr AttributeName Order
  deriving (Show, Generic, Eq)

data AttributeOrder = AttributeOrder AttributeName Order
  deriving (Show, Generic, Eq)

data Order = AscendingOrder | DescendingOrder
  deriving (Eq, Show, Generic)

ascending :: T.Text
ascending = "⬆"

descending :: T.Text
descending = "⬇"

arbitrary :: T.Text
arbitrary = "↕"

data DataFrame = DataFrame {
  orders :: [AttributeOrder],
  attributes :: Attributes,
  tuples :: [DataFrameTuple]
  }
  deriving (Show, Generic)

data DataFrameTuple = DataFrameTuple Attributes (V.Vector Atom)
  deriving (Eq, Show, Generic)

sortDataFrameBy :: [AttributeOrder] -> DataFrame -> Either RelationalError DataFrame
sortDataFrameBy attrOrders frame = do
  attrs <- mapM (\(AttributeOrder nam _) -> A.attributeForName nam (attributes frame)) attrOrders 
  mapM_ (\attr -> unless (isSortableAtomType (A.atomType attr)) $ Left (AttributeNotSortableError attr)) attrs
  pure $ DataFrame attrOrders (attributes frame) (sortTuplesBy (compareTupleByAttributeOrders attrOrders) (tuples frame))

sortTuplesBy :: (DataFrameTuple -> DataFrameTuple -> Ordering) -> [DataFrameTuple] -> [DataFrameTuple]
sortTuplesBy = L.sortBy

compareTupleByAttributeOrders :: [AttributeOrder] -> DataFrameTuple -> DataFrameTuple -> Ordering
compareTupleByAttributeOrders attributeOrders tup1 tup2 = 
  let compare' (AttributeOrder attr order) = if order == DescendingOrder 
        then compareTupleByOneAttributeName attr tup2 tup1
        else compareTupleByOneAttributeName attr tup1 tup2
      res = map compare' attributeOrders in
  fromMaybe EQ (L.find (/= EQ) res)

compareTupleByOneAttributeName :: AttributeName -> DataFrameTuple -> DataFrameTuple -> Ordering
compareTupleByOneAttributeName attr tuple1 tuple2 =
  let eAtom1 = atomForAttributeName attr tuple1
      eAtom2 = atomForAttributeName attr tuple2 in
  case eAtom1 of
    Left err -> error (show err)
    Right atom1 ->
      case eAtom2 of
        Left err -> error (show err)
        Right atom2 -> compareAtoms atom1 atom2

atomForAttributeName :: AttributeName -> DataFrameTuple -> Either RelationalError Atom
atomForAttributeName attrName (DataFrameTuple tupAttrs tupVec) = case V.findIndex (\attr -> A.attributeName attr == attrName) (attributesVec tupAttrs) of
  Nothing -> Left (NoSuchAttributeNamesError (S.singleton attrName))
  Just index -> case tupVec V.!? index of
    Nothing -> Left (NoSuchAttributeNamesError (S.singleton attrName))
    Just atom -> Right atom

take' :: Integer -> DataFrame -> DataFrame
take' n df = df { tuples = take (fromInteger n) (tuples df) }

drop' :: Integer -> DataFrame -> DataFrame
drop' n df = df { tuples = drop (fromInteger n) (tuples df) }

toDataFrame :: Relation -> DataFrame
toDataFrame (Relation attrs (RelationTupleSet tuples')) = DataFrame [] attrs (map (\(RelationTuple tupAttrs tupVec) -> DataFrameTuple tupAttrs tupVec) tuples')

fromDataFrame :: DataFrame -> Either RelationalError Relation 
fromDataFrame df = R.mkRelation (attributes df) (RelationTupleSet tuples')
  where
    tuples' = map (\(DataFrameTuple attrs' tupVec) -> RelationTuple attrs' tupVec) (tuples df)

showDataFrame :: DataFrame -> T.Text
showDataFrame = renderTable . dataFrameAsTable
  
--terminal display
dataFrameAsTable :: DataFrame -> Table
dataFrameAsTable df = (header, body)
  where
    oAttrNames = A.orderedAttributeNames (attributes df)
    oAttrs = A.orderedAttributes (attributes df)
    header = "DF" : map dfPrettyAttribute oAttrs
    dfPrettyAttribute attr = prettyAttribute attr <> case L.find (\(AttributeOrder nam _) -> nam == A.attributeName attr) (orders df) of
      Nothing -> arbitrary
      Just (AttributeOrder _ AscendingOrder) -> ascending
      Just (AttributeOrder _ DescendingOrder) -> descending
    body = snd (L.foldl' tupleFolder (1 :: Int,[]) (tuples df))
    tupleFolder (count, acc) tuple = (count + 1,
                                      acc ++ [T.pack (show count) : map (\attrName -> case atomForAttributeName attrName tuple of
                                            Left _ -> "?"
                                            Right atom -> showAtom 0 atom
                                            ) oAttrNames])

-- | A Relation can be converted to a DataFrame for sorting, limits, and offsets.
data DataFrameExpr = DataFrameExpr {
  convertExpr :: RelationalExpr,
  orderExprs :: [AttributeOrderExpr],
  offset :: Maybe Integer,
  limit :: Maybe Integer
  }
  deriving (Show, Generic, Eq)

-- | True iff dataframe features are required to execute this expression, False if this expression could be evaluated as a relational expression (no sorting, limit, or offset).
usesDataFrameFeatures :: DataFrameExpr -> Bool
usesDataFrameFeatures df = not (null (orderExprs df)) || isJust (offset df) || isJust (limit df)

-- | Returns a data frame expression without any sorting or limits.
nakedDataFrameExpr :: RelationalExpr -> DataFrameExpr
nakedDataFrameExpr rexpr = DataFrameExpr { convertExpr = rexpr,
                                           orderExprs = [],
                                           offset = Nothing,
                                           limit = Nothing }

dataFrameAsHTML :: DataFrame -> T.Text
-- web browsers don't display tables with empty cells or empty headers, so we have to insert some placeholders- it's not technically the same, but looks as expected in the browser
dataFrameAsHTML df 
  | length (tuples df) == 1 && A.null (attributes df) = style <>
                          tablestart <>
                          "<tr><th></th></tr>" <>
                          "<tr><td></td></tr>" <> 
                          tablefooter <> "</table>"
  | L.null (tuples df) && A.null (attributes df) = style <>
                           tablestart <>
                           "<tr><th></th></tr>" <>
                           tablefooter <> 
                           "</table>"
  | otherwise = style <>
                tablestart <> 
                attributesAsHTML (attributes df) (orders df) <> 
                tuplesAsHTML (tuples df) <> 
                tablefooter <> 
                "</table>"
  where
    cardinality = T.pack (show (length (tuples df)))
    style = "<style>.pm36dataframe {empty-cells: show;} .pm36dataframe tbody td, .pm36relation th { border: 1px solid black;}</style>"
    tablefooter = "<tfoot><tr><td colspan=\"100%\">" <> cardinality <> " tuples</td></tr></tfoot>"
    tablestart = "<table class=\"pm36dataframe\"\">"

tuplesAsHTML :: [DataFrameTuple] -> T.Text
tuplesAsHTML = foldr folder ""
  where
    folder tuple acc = acc <> tupleAsHTML tuple

tupleAssocs :: DataFrameTuple -> [(AttributeName, Atom)]
tupleAssocs (DataFrameTuple attrs tupVec) = V.toList $ V.map (first A.attributeName) (V.zip (attributesVec attrs) tupVec)
    

tupleAsHTML :: DataFrameTuple -> T.Text
tupleAsHTML tuple = "<tr>" <> T.concat (L.map tupleFrag (tupleAssocs tuple)) <> "</tr>"
  where
    tupleFrag tup = "<td>" <> atomAsHTML (snd tup) <> "</td>"
    atomAsHTML (RelationAtom rel) = RelHTML.relationAsHTML rel
    atomAsHTML (TextAtom t) = "&quot;" <> t <> "&quot;"
    atomAsHTML atom = atomToText atom

attributesAsHTML :: Attributes -> [AttributeOrder] -> T.Text
attributesAsHTML attrs orders' = "<tr>" <> T.concat (map oneAttrHTML (A.toList attrs)) <> "</tr>"
  where
    oneAttrHTML attr = "<th>" <> prettyAttribute attr <> ordering (A.attributeName attr) <> "</th>"
    ordering attrName = " " <> case L.find (\(AttributeOrder nam _) -> nam == attrName) orders' of
      Nothing -> "(arb)"
      Just (AttributeOrder _ AscendingOrder) -> "(asc)"
      Just (AttributeOrder _ DescendingOrder) -> "(desc)"

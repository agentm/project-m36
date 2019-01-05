{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{- A dataframe is a strongly-typed, ordered list of named tuples. A dataframe differs from a relation in that its tuples are ordered.-}
module ProjectM36.DataFrame where
import ProjectM36.Base
import ProjectM36.Attribute as A
import ProjectM36.Error
import qualified ProjectM36.Relation as R
import ProjectM36.Relation.Show.Term
import ProjectM36.DataTypes.Sorting
import ProjectM36.AtomType
import qualified Data.Vector as V
import GHC.Generics
import qualified Data.List as L
import qualified Data.Set as S
import Data.Maybe
import qualified Data.Text as T
import Data.Binary

data AttributeOrderExpr = AttributeOrderExpr AttributeName Order deriving (Show, Generic, Binary)
data AttributeOrder = AttributeOrder AttributeName Order deriving (Show, Generic, Binary)
data Order = AscendingOrder | DescendingOrder deriving (Eq, Show, Generic, Binary)

data DataFrame = DataFrame Attributes [DataFrameTuple] deriving (Show, Generic, Binary)

data DataFrameTuple = DataFrameTuple Attributes (V.Vector Atom) deriving (Eq, Show, Generic, Binary)

attributes :: DataFrame -> Attributes
attributes (DataFrame attrs _) = attrs

tuples :: DataFrame -> [DataFrameTuple]
tuples (DataFrame _ tups) = tups

sortDataFrameBy :: [AttributeOrder] -> DataFrame -> Either RelationalError DataFrame
sortDataFrameBy attrOrders frame = do
  attrs <- mapM (\(AttributeOrder nam _) -> A.attributeForName nam (attributes frame)) attrOrders 
  mapM_ (\attr -> if not (isSortableAtomType (atomType attr)) then
            Left (AttributeNotSortableError attr)
            else
            pure ()) attrs
  pure $ DataFrame (attributes frame) (sortTuplesBy (compareTupleByAttributeOrders attrOrders) (tuples frame))

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
atomForAttributeName attrName (DataFrameTuple tupAttrs tupVec) = case V.findIndex (\attr -> attributeName attr == attrName) tupAttrs of
  Nothing -> Left (NoSuchAttributeNamesError (S.singleton attrName))
  Just index -> case tupVec V.!? index of
    Nothing -> Left (NoSuchAttributeNamesError (S.singleton attrName))
    Just atom -> Right atom

take' :: Integer -> DataFrame -> DataFrame
take' n (DataFrame attrs tuples') = DataFrame attrs (take (fromInteger n) tuples')

drop' :: Integer -> DataFrame -> DataFrame
drop' n (DataFrame attrs tuples') = DataFrame attrs (drop (fromInteger n) tuples')

toDataFrame :: Relation -> DataFrame
toDataFrame (Relation attrs (RelationTupleSet tuples')) = DataFrame attrs (map (\(RelationTuple tupAttrs tupVec) -> DataFrameTuple tupAttrs tupVec) tuples')

fromDataFrame :: DataFrame -> Either RelationalError Relation 
fromDataFrame (DataFrame attrs dftuples) = R.mkRelation attrs (RelationTupleSet tuples')
  where
    tuples' = map (\(DataFrameTuple attrs' tupVec) -> RelationTuple attrs' tupVec) dftuples

--terminal display
dataFrameAsTable :: DataFrame -> Table
dataFrameAsTable (DataFrame attrs tups) = (header, body)
  where
    oAttrNames = orderedAttributeNames attrs
    oAttrs = orderedAttributes attrs
    header = "DF" : map prettyAttribute oAttrs
    body = snd (L.foldl' tupleFolder (1 :: Int,[]) tups)
    tupleFolder (count, acc) tuple = (count + 1,
                                      acc ++ [T.pack (show count) : map (\attrName -> case atomForAttributeName attrName tuple of
                                            Left _ -> "?"
                                            Right atom -> showAtom 0 atom
                                            ) oAttrNames])

showDataFrame :: DataFrame -> StringType
showDataFrame = renderTable . dataFrameAsTable

-- | A Relation can be converted to a DataFrame for sorting, limits, and offsets.
data DataFrameExpr = DataFrameExpr {
  convertExpr :: RelationalExpr,
  orderExprs :: [AttributeOrderExpr],
  offset :: Maybe Integer,
  limit :: Maybe Integer
  } deriving (Show, Binary, Generic)

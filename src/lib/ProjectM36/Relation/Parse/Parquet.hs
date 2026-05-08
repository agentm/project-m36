{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables #-}
module ProjectM36.Relation.Parse.Parquet where
import DataFrame.IO.Parquet
import DataFrame.Internal.DataFrame (getColumn, dataframeDimensions)
import DataFrame.Internal.Column (Column(..), columnTypeString)

import qualified Data.Map as M
import Control.Exception
import Data.Either (lefts, rights)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Type.Equality (testEquality)
import Type.Reflection

import ProjectM36.Base
import ProjectM36.Attribute (attributeNamesList, atomTypesList)

data ParquetImportError =
  ColumnTypeMismatchError AtomType Text | -- for columns which cannot be cast
  ValueTypeMismatchError AtomType Text | -- for specific values that cannot be cast
  DataFrameColumnNotFoundError AttributeName |
  DataFrameIOError IOException |
  MultipleParquetErrors [ParquetImportError]
  
-- use CoW to place the parquet file in the db directory, then we can read from it indefinitely
-- add an incrementing row id to keep each row unique in the relational sense
-- in the future, this should likely be reimplemented using streamly, but dataframe does not support incremental reading, so we use the parquet->dataframe->relation path for now
parquetAsRelation :: Attributes -> TypeConstructorMapping -> [FilePath] -> IO (Either ParquetImportError Relation)
parquetAsRelation attrs tConsMap parquetPaths = do
  let parquetReadOptions = defaultParquetReadOptions { selectedColumns = Just attrsProjection }
      attrsProjection = attributeNamesList attrs
      attrsTypes = atomTypesList attrs
      parquetHandler (err :: IOException) = pure (Left (DataFrameIOError err))
        
  eFileDataFrames <- mapM (\path ->
                              (readParquetWithOpts parquetReadOptions path >>= (pure . Right)) `catch` parquetHandler) parquetPaths

  case lefts eFileDataFrames of
    (e:_) -> pure (Left e)
    [] -> do
      let fileDataFrames = rights eFileDataFrames
          oneDataFrame = mconcat fileDataFrames
  --typecheck dataframe
      let eProjectedDataFrameColumns = mapM (\attrName -> case getColumn attrName oneDataFrame of
                                                Nothing -> Left (DataFrameColumnNotFoundError attrName)
                                                Just col -> pure col
                                            ) attrsProjection
      case eProjectedDataFrameColumns of
        Left err -> pure (Left err)
        Right projectedDataFrameColumns -> do
          let eTypes = map (uncurry typeCheckColumn) (zip projectedDataFrameColumns attrsTypes)
          if not (null (lefts eTypes)) then
            pure (Left (MultipleParquetErrors (lefts eTypes)))
            else do -- type match, but watch out for nulls!
              --convert dataframe to relation
              let rowAsTuple :: Int -> V.Vector Atom
                  rowAsTuple rowIndex =
                    foldr (columnFolder rowIndex) V.empty projectedDataFrameColumns
                  (rowCount, _) = dataframeDimensions oneDataFrame  
                  columnFolder :: Int -> Column -> V.Vector Atom -> V.Vector Atom
                  columnFolder rowIndex (BoxedColumn (col :: V.Vector a)) acc =
                    case col V.!? rowIndex of
                      Just e ->
                        case testEquality (typeRep @a) (typeRep @Text) of
                          Just Refl -> acc `V.snoc` TextAtom e
                          

                  tuples = map rowAsTuple [0 .. (rowCount - 1)]
              error "gonk"

typeCheckColumn :: Column -> AtomType -> Either ParquetImportError AtomType
typeCheckColumn col expectedAtomType = do
  let simpleConversionMap = M.fromList [("Integer", IntegerAtomType)]
    --nullableConversion =
      parquetColumnType = columnTypeString col
  case M.lookup parquetColumnType simpleConversionMap of
    Nothing -> Left $ ColumnTypeMismatchError expectedAtomType (T.pack parquetColumnType)
    Just typ -> pure typ

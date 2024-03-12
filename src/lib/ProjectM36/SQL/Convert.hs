--convert SQL into relational or database context expressions
{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables, TypeApplications, GeneralizedNewtypeDeriving #-}
module ProjectM36.SQL.Convert where
import ProjectM36.Base as B
import ProjectM36.Error
import ProjectM36.DataTypes.SQL.Null
import ProjectM36.SQL.Select
import ProjectM36.SQL.Insert as Insert
import ProjectM36.SQL.DBUpdate
import ProjectM36.SQL.Update as Update
import ProjectM36.SQL.Delete as Delete
import ProjectM36.SQL.CreateTable as CreateTable
import ProjectM36.RelationalExpression
import ProjectM36.DataFrame (DataFrameExpr(..), AttributeOrderExpr(..), Order(..), usesDataFrameFeatures)
import ProjectM36.AttributeNames as A
import ProjectM36.Relation (attributes)
import qualified ProjectM36.Attribute as A
import qualified Data.Text as T
import qualified ProjectM36.WithNameExpr as With
import Control.Monad (foldM)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (intercalate, find)
import qualified Data.Functor.Foldable as Fold
import qualified Data.List.NonEmpty as NE
import Control.Monad (when)
import Data.Maybe (isJust, fromMaybe)
--import Control.Monad (void)
import Control.Monad.Trans.State (StateT, get, put, runStateT, evalStateT)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans.Class (lift)

import Debug.Trace

{-
TODO
* remove commented out code
* remove unused functions from failed experiments
* remove traceShow*
* enable duplicate rows by adding uuid column
-}

--over the course of conversion of a table expression, we collect all the table aliases we encounter, including non-aliased table references, including the type of the table, projections have their own name resolution system
newtype TableContext = TableContext (M.Map TableAlias (RelationalExpr, Attributes, ColumnAliasRemapper))
  deriving (Semigroup, Monoid, Show, Eq)

type TypeForRelExprF = RelationalExpr -> Either RelationalError Relation

type ConvertM = StateT TableContext (ExceptT SQLError Identity)

runConvertM :: TableContext -> ConvertM a -> Either SQLError (a, TableContext)
runConvertM tcontext m = runIdentity (runExceptT (runStateT m tcontext))

evalConvertM :: TableContext -> ConvertM a -> Either SQLError a
evalConvertM tcontext m = runIdentity (runExceptT (evalStateT m tcontext))

data SelectItemsConvertTask = SelectItemsConvertTask { taskProjections :: S.Set ColumnProjectionName,
                                                       taskRenames :: [(ColumnProjectionName, ColumnAlias)],
                                                       taskExtenders :: [ExtendTupleExpr]
                                                     } deriving (Show, Eq)
                                          
-- (real attribute name in table- immutable, (renamed "preferred" attribute name needed to disambiguate names on conflict, set of names which are used to reference the "preferred" name)
type AttributeAlias = AttributeName
-- the AttributeAlias is necessary when then is otherwise a naming conflict such as with join conditions which would otherwise cause duplicate column names which SQL supports but the relational algebra does not
type ColumnAliasRemapper = M.Map AttributeName (AttributeAlias, S.Set ColumnName)

insertIntoColumnAliasRemap' :: AttributeName -> AttributeAlias -> ColumnName -> ColumnAliasRemapper -> Either SQLError ColumnAliasRemapper
insertIntoColumnAliasRemap' attrName attrAlias colName remap =
  case attrName `M.lookup` remap of
    Nothing -> pure $ M.insert attrName (attrAlias, S.singleton colName) remap
    Just (attrAlias', colNames) | attrAlias' == attrAlias ->
                                  pure $ M.insert attrName (attrAlias, S.insert colName colNames) remap
                                  | otherwise ->
                                    Left (ColumnAliasResolutionError (ColumnAlias attrName))

-- | Used to note if columns are remapped to different attributes in order to mitigate attribute naming conflicts.
insertColumnAlias ::
  TableAlias -> -- table reference
  AttributeName -> -- real attribute name
  ColumnAlias -> -- column alias
  ColumnName -> -- original reference name
  ConvertM ()
insertColumnAlias tAlias attrName (ColumnAlias colAlias) colName = do
  TableContext tmap <- get
  case tAlias `M.lookup` tmap of
    Nothing -> throwSQLE (MissingTableReferenceError tAlias)
    Just (rve,attrs,remap) -> do
      case insertIntoColumnAliasRemap' attrName colAlias colName remap of
        Left err -> throwSQLE err
        Right remap' -> do
          let tmap' = M.insert tAlias (rve, attrs, remap') tmap
          put (TableContext tmap')

-- debugging utility function
prettyTableContext :: TableContext -> String
prettyTableContext (TableContext tMap) = "TableContext {\n" <> concatMap prettyKV (M.toList tMap) <> "}"
  where
    prettyKV (TableAlias k, (_rvexpr, _attrs, aliasMap)) =
      " " <> T.unpack k <> "::  " <> 
      prettyColumnAliasRemapper aliasMap <> "\n"

prettyColumnAliasRemapper :: ColumnAliasRemapper -> String
prettyColumnAliasRemapper cAMap = intercalate ", " $ map (\(realAttr, (attrAlias, colNameSet)) -> "real->" <> T.unpack realAttr <> ":alias->" <> T.unpack attrAlias <> ":alts->{" <> show colNameSet <> "}") (M.toList cAMap)


  
traceStateM :: ConvertM ()
traceStateM = do
  s <- get
  traceM (prettyTableContext s)
  
-- key: alias value: real column attribute name
type ColumnAliasMap = M.Map ColumnAlias AttributeName

tableAliasesAsWithNameAssocs :: ConvertM WithNamesAssocs
tableAliasesAsWithNameAssocs = do
  (TableContext tmap) <- get
  filter notSelfRef <$> mapM mapper (M.toList tmap)
  where
    notSelfRef (WithNameExpr nam (), RelationVariable nam' ()) | nam == nam' = False
                                                            | otherwise = True
    notSelfRef _ = True
--    mapper :: (TableAlias, (RelationalExpr, Attributes)) -> ConvertM (WithNameExpr, RelationalExpr)
    mapper (TableAlias nam, (rvExpr, _, _)) = pure (WithNameExpr nam (), rvExpr)

throwSQLE :: SQLError -> ConvertM a
throwSQLE = lift . throwE

type ColumnAliasRenameMap = M.Map (TableAlias, AttributeName) ColumnAlias

-- | Pass state down to subselect, but discard any state changes from the subselect processing.
withSubSelect :: ConvertM a -> ConvertM (a, ColumnAliasRenameMap)
withSubSelect m = do
  state@(TableContext orig) <- get
  ret <- m
  (TableContext postSub) <- get
  put state
  -- diff the state to get just the items that were added
--  traceShowM ("keys orig"::String, M.keys orig)
--  traceShowM ("keys postSub"::String, M.keys postSub)
  let tableDiffFolder acc (tAlias, (RelationVariable _rv (), _ , colAliasRemapper)) = do
        let convertColAliases :: ColumnAliasRemapper -> (AttributeName, (AttributeName, S.Set ColumnName)) -> ColumnAliasRenameMap -> ColumnAliasRenameMap
            convertColAliases origColAlRemapper (attrName, (attrAlias,_)) acc' =
              if M.member attrName origColAlRemapper then
                acc'
                else
                M.insert (tAlias, attrName) (ColumnAlias attrAlias) acc'
        case M.lookup tAlias orig of
          -- new table has been added to column alias map, add all columns aliased
          Nothing -> do
            pure (acc <> foldr (convertColAliases mempty) mempty (M.toList colAliasRemapper))
          -- we are aware of the table, but there may have been some new columns added
          Just (_,_,colAliasRemapper') ->
            pure (acc <> foldr (convertColAliases colAliasRemapper') mempty (M.toList colAliasRemapper'))
      tableDiffFolder _ (_, (rvexpr, _, _)) = throwSQLE (UnexpectedRelationalExprError rvexpr)
            
  diff <- foldM tableDiffFolder mempty (M.toList postSub)

{-  let diff = M.differenceWith tctxDiff postSub orig
      tctxDiff (rexprA, attrsA, colAliasMapA) (_, _, colAliasMapB) =
        Just (rexprA, attrsA, M.difference colAliasMapB colAliasMapA)-}
--  traceShowM ("subselect diff"::String, diff)
  pure (ret, diff)

-- if we find a column naming conflict, generate a non-conflicting name for insertion into the column alias map
generateColumnAlias :: TableAlias -> AttributeName -> ConvertM ColumnAlias
generateColumnAlias (TableAlias tAlias) attrName = do
  tctx <- get
  let potentialNames = map ColumnName ([[attrName],
                                        [tAlias <> "." <> attrName]] <>
                                        map (\x -> [tAlias <> "." <> attrName <> T.pack (show x)]) [1::Int ..])
      nameIsAvailable nam = 
        case findOneColumn' nam tctx of
          Left ColumnResolutionError{} -> --no match, so we can use this name
            True
          _ -> False --some conflict, so loop
      firstAvailableName = find nameIsAvailable potentialNames
--  traceShowM ("generateColumnAlias scan"::String, tAlias, attrName, firstAvailableName)     
  case firstAvailableName of
    Just (ColumnName [nam]) -> pure (ColumnAlias nam)
    _ -> throwSQLE $ ColumnResolutionError (ColumnName [attrName])

-- | Insert another table into the TableContext. Returns an alias map of any columns which could conflict with column names already present in the TableContext so that they can be optionally renamed.
insertTable :: TableAlias -> RelationalExpr -> Attributes -> ConvertM ColumnAliasMap
insertTable tAlias expr rtype = do
  (TableContext map') <- get
  case M.lookup tAlias map' of
    Nothing -> do
      put $ TableContext $ M.insert tAlias (expr, rtype, mempty) map'
--      traceShowM ("insertTable"::String, tAlias)
      pure mempty
    Just _ -> throwSQLE (DuplicateTableReferenceError tAlias)

-- | When a column is mentioned, it may need to be aliased. The table name must already be in the table context so that we can identify that the attribute exists. Without a table name, we must look for a uniquely named column amongst all tables. Thus, we pre-emptively eliminate duplicate column names.
noteColumnMention :: Maybe TableAlias -> ColumnName -> Maybe ColumnAlias -> ConvertM ColumnAlias
noteColumnMention mTblAlias colName mColAlias = do
  -- find the relevant table for the key to the right table
--  traceShowM ("noteColumnMention"::String, mTblAlias, colName)
--  traceStateM
  tc@(TableContext tcontext) <- get
{-  tblAlias' <- case mTblAlias of
                        Just tblAlias -> do
                          void $ lookupTable tblAlias
                          pure tblAlias
                        Nothing ->do
                          -- scan column names for match- if there are multiple matches, return a column ambiguity error
                          ret <- findOneColumn colName
--                          traceShowM ("insertColumn2", colName)
                          pure ret-}
  -- insert into the column alias map
{-  let colAttr = case colName of
                  ColumnName [c] -> c
                  ColumnName [t,c] -> 
      origAttrName = case colName of
                       ColumnName [c] -> c
                       ColumnName [_,c] -> c-}
  -- check if we already have a mention mapping
  let lookupWithTableAlias (TableAlias tAlias) colAttr = do
        when (isJust mTblAlias && Just (TableAlias tAlias) /= mTblAlias) (throwSQLE (TableAliasMismatchError (TableAlias tAlias)))
      -- we have a specific table alias, so ensure it's valid
        let tPrefixColAttr = tAlias <> "." <> colAttr
            insertColAlias newAlias = do
              insertColumnAlias (TableAlias tAlias) colAttr (ColumnAlias newAlias) colName
              pure (ColumnAlias newAlias)
        case M.lookup (TableAlias tAlias) tcontext of
          Nothing -> do -- add a new colaliasremapper
            insertColAlias (fromMaybe tPrefixColAttr (unColumnAlias <$> mColAlias))
          Just (_, _, colAlRemapper) -> do
            -- table alias already known, check for column alias
--            traceShowM ("noteColumnMention before attr"::String, colAlRemapper)
            case attributeNameForAttributeAlias colAttr colAlRemapper of
              Left _ -> do
                -- col alias missing, so add it- figure out if it needs a table prefix
                --traceShowM ("findNotedColumn' in noteColumnMention"::String, colAlias)
                --traceStateM
                let sqlColAlias = fromMaybe colAttr (unColumnAlias <$> mColAlias)
                colAlias' <- case findNotedColumn' (ColumnName [colAttr]) tc of
                               Left _ -> -- no match, so table prefix not required
                                 insertColAlias sqlColAlias
                               Right [] -> -- no match, so table prefix not required
                                 insertColAlias sqlColAlias
                               Right [_] -> -- we have a match, so we need the table prefix
                                 insertColAlias (fromMaybe tPrefixColAttr (unColumnAlias <$> mColAlias))
                               Right (_:_) -> throwSQLE (AmbiguousColumnResolutionError colName)
                --traceShowM ("findNotedColumn' in noteColumnMentionB"::String, colAlias')
                pure colAlias'
              Right attrName ->
                -- we know the alias already, so return it
                pure (ColumnAlias attrName)

  case colName of
    ColumnName [tAlias,colAlias] -> lookupWithTableAlias (TableAlias tAlias) colAlias
    ColumnName [colAlias] ->
      case mTblAlias of
        Just tAlias -> lookupWithTableAlias tAlias colAlias
        Nothing -> do
          -- lookup without table alias
          -- unqualified column alias- search for unambiguous table reference
          let folder (ta, (_, _, colAliasRemapper)) acc =
                case attributeNameForAttributeAlias colAlias colAliasRemapper of
                  Left _ -> acc
                  Right attrName -> (ta,attrName) : acc
              sqlColAlias = fromMaybe colAlias (unColumnAlias <$> mColAlias)                
      
          case foldr folder mempty (M.toList tcontext) of
            [] -> do -- no matches, search raw attributes
              case findColumn' colName tc of
                [] -> -- no match in attributes, either, error
                  throwSQLE (UnexpectedColumnNameError colName)
                [tAlias] -> do -- one match, insert it
                  insertColumnAlias tAlias sqlColAlias (ColumnAlias colAlias) colName
                  pure (ColumnAlias colAlias)
                (_:_) -> -- too many matches, error
                  throwSQLE (AmbiguousColumnResolutionError colName)
            [(tAlias, attrName)] -> do -- valid attribute match, so add colaliasremapper
              insertColumnAlias tAlias attrName (ColumnAlias colAlias) colName
              pure (ColumnAlias colAlias)
            (_:_) -> -- two many matches, error
              throwSQLE (AmbiguousColumnResolutionError colName)
    other@ColumnName{} -> throwSQLE (UnexpectedColumnNameError other)   


------      
{-  case findNotedColumn' colName tc of
    Right [] -> do
      -- no match found, so we can insert this as a new column alias
      let colAlias = case mColAlias of
                       Just al -> al
                       Nothing -> --ColumnAlias (unTableAlias tblAlias' <> "." <> origAttrName)
                         ColumnAlias origAttrName 
      insertColumnAlias tblAlias' origAttrName colAlias colName
      pure colAlias
    Right [match] ->
      -- one match found- error
      throwSQLE (AmbiguousColumnResolutionError colName)
    Right (match:_) ->
      -- multiple matches found- error
      throwSQLE (AmbiguousColumnResolutionError colName)
    Left (ColumnResolutionError{}) ->
      throwSQLE err-}
{-  case M.lookup tblAlias' tcontext of
    Nothing -> throwSQLE (MissingTableReferenceError tblAlias')
    Just (_,_,colAliasRemapper) -> do
      case attributeNameForAttributeAlias colAttr colAliasRemapper of
        Right _ -> pure (ColumnAlias colAttr)
        Left _ -> do -- no match previously recorded, so add it-}
{-  when (newAlias `elem` allColumnAliases tcontext) $ do
    traceShowM ("gonk error",
                "colName", colName,
                "mTblAlias", mTblAlias,
                "mColAlias", mColAlias,
p                tmap)
    throwSQLE (DuplicateColumnAliasError newAlias)-} --duplicate column aliases are OK
  --verify that the alias is not duplicated
{-          let colAlias = case mColAlias of
                           Just al -> al
                           Nothing -> --ColumnAlias (unTableAlias tblAlias' <> "." <> origAttrName)
                             ColumnAlias origAttrName 
          insertColumnAlias tblAlias' origAttrName colAlias colName
          pure colAlias
-}
{-
-- | Add a column alias for a column which has already been inserted into the TableContext.
addColumnAlias' :: TableContext -> TableAlias -> ColumnAlias -> AttributeName -> Either SQLError TableContext
addColumnAlias' (TableContext tctx) tAlias colAlias@(ColumnAlias colText) attr = do
  case M.lookup tAlias tctx of
    Nothing -> Left (ColumnAliasResolutionError colAlias)
    Just (rvexpr, attrs, colMap) ->
      --check that the attribute is present in attributes, then plop it into the colMap and return the updated TableContext
      if attr `A.isAttributeNameContained` attrs then do
        insertColumnAlias 
        let newColMap = M.insert colAlias attr colMap
            newTContext = M.insert tAlias (rvexpr, attrs, newColMap) tctx
        pure (TableContext newTContext)
      else do
        traceShow "addColAlias'" $ Left (ColumnResolutionError (ColumnName [attr]))

addColumnAlias :: TableAlias -> ColumnAlias -> AttributeName -> ConvertM ()
addColumnAlias tAlias colAlias attrName = do
  tctx <- get
  case addColumnAlias' tctx tAlias colAlias attrName of
    Left err -> throwSQLE err
    Right tctx' -> put tctx'

allColumnAliases :: TableContext -> [ColumnAlias]
allColumnAliases (TableContext tmap) =
  foldl' folder [] tmap
  where
    folder acc (_,_,colmap) = M.keys colmap <> acc
-}  
lookupTable :: TableAlias -> ConvertM (RelationalExpr, Attributes, ColumnAliasRemapper)
lookupTable ta = do
  (TableContext map') <- get
  case M.lookup ta map' of
    Nothing -> throwSQLE (MissingTableReferenceError ta)
    Just res -> pure res

{-
-- | Merge table contexts (used in subselects)
mergeContext :: TableContext -> ConvertM ColumnAliasMap
mergeContext (TableContext ctxB) = do
  (TableContext tMapA) <- get
  foldM folder mempty (M.toList tMapA)
   where
    folder acc (tAlias, (re,attrs, _)) = do
      colMap <- insertTable tAlias re attrs
      pure (M.union acc colMap)
-}
-- | Find a column name or column alias in the underlying table context. Returns key into table context.
findColumn :: ColumnName -> ConvertM [TableAlias]
findColumn targetCol = do
  tcontext <- get
  pure (findColumn' targetCol tcontext)
  
-- | non ConvertM version of findColumn
findColumn' :: ColumnName -> TableContext -> [TableAlias]
findColumn' targetCol (TableContext tMap) = do
--  traceShowM ("findColumn'", targetCol, tMap)
  M.foldrWithKey folder [] tMap
   where
    folder tAlias@(TableAlias tat) (_rvExpr, rtype, _) acc =
      case targetCol of
        ColumnName [colName'] ->
          if S.member colName' (A.attributeNameSet rtype) then
            tAlias : acc
            else
            acc
        ColumnName [tPrefix, colName'] ->
          if tat == tPrefix && S.member colName' (A.attributeNameSet rtype) then
            tAlias : acc
            else
            acc
        _ -> acc

-- search ColumnAliasRemapper for columns which have already been noted- can be used for probing for new aliases
findNotedColumn' :: ColumnName -> TableContext -> Either SQLError [(TableAlias, AttributeName)]
findNotedColumn' (ColumnName [attr]) (TableContext tcontext) =
  -- search all column alias remappers for attribute- if there is a conflict because the alias is ambiguous, error out
  pure $ foldr folder mempty (M.toList tcontext)
  where
    folder (ta, (_, _, colAliasRemapper)) acc =
      case attributeNameForAttributeAlias attr colAliasRemapper of
        Left _ -> acc
        Right attrName -> (ta,attrName) : acc

findNotedColumn' (ColumnName [tPrefix, attr]) (TableContext tcontext) =
  --find referenced table alias
  --search for noted column in column alias remapper
  case M.lookup (TableAlias tPrefix) tcontext of
    Nothing -> Left (MissingTableReferenceError (TableAlias tPrefix))
    Just (_, _, colAlRemapper) -> do
      attrName <- attributeNameForAttributeAlias attr colAlRemapper
      pure [(TableAlias tPrefix, attrName)]
findNotedColumn' colName _ = Left $ UnexpectedColumnNameError colName      


attributeNameForAttributeAlias :: AttributeAlias -> ColumnAliasRemapper -> Either SQLError AttributeName
attributeNameForAttributeAlias al remapper = do
--  traceShowM ("attributeNameForAttributeAlias"::String, al, remapper)
  foldr folder (Left (ColumnAliasResolutionError (ColumnAlias al))) (M.toList remapper)
  where
    folder (_attrName, (attrAlias, _)) acc =
               if attrAlias == al then
                 pure attrAlias
               else
                 acc
  

findOneColumn :: ColumnName -> ConvertM TableAlias
findOneColumn targetCol = do
  tcontext <- get
  case findOneColumn' targetCol tcontext of
    Left err -> throwSQLE err
    Right match -> pure match

findOneColumn' :: ColumnName -> TableContext -> Either SQLError TableAlias
findOneColumn' targetCol tcontext = do
  case findColumn' targetCol tcontext of
    [] -> do
      Left (ColumnResolutionError targetCol)
    [match] -> pure match
    _matches -> Left (AmbiguousColumnResolutionError targetCol)

-- | Search the TableContext for a column alias remapping for the given column name. This function can change the state context if column names conflict.
attributeNameForColumnName :: ColumnName -> ConvertM AttributeName
attributeNameForColumnName colName = do
  tKey@(TableAlias tAlias) <- findOneColumn colName
  tcontext@(TableContext tmap) <- get
  let (_, rvattrs, colAliases) = tmap M.! tKey
  --strip table prefix, if necessary
  (ColumnAlias colAttr) <- case colName of
                ColumnName [attr] -> pure $ ColumnAlias attr
                ColumnName [_tname,attr] -> pure $ ColumnAlias attr
                ColumnName{} -> throwSQLE $ ColumnResolutionError colName
--  traceShowM ("attributeNameForColumnName' colAlias"::String, colAttr, colAliases, colAlias)
  case M.lookup colAttr colAliases of
    Just (alias,_) -> pure alias -- we found it, so it's valid
    Nothing ->
      -- look in rvattrs, so we don't need the table alias prefix. The lack of an entry in the column alias map indicates that the column was not renamed in the join condition.
      if colAttr `A.isAttributeNameContained` rvattrs then
        -- we have a matching attribute, but it could conflict with another attribute, so check for that
        case findOneColumn' (ColumnName [colAttr]) tcontext of
          Right _ -> pure colAttr
          Left (AmbiguousColumnResolutionError{}) -> do
            --we have a conflict, so insert a new column alias and return it
--            traceShowM ("attributeNameForColumnName"::String, colName)
            (ColumnAlias al) <- noteColumnMention (Just tKey) (ColumnName [tAlias,colAttr]) Nothing
            --traceShowM ("attributeNameForColumnName' noteColumnMention"::String, colAttr, al)
            pure al
          Left err -> throwSQLE err
        --pure (T.concat [tAlias, ".", colAttr])
      else
        case colName of
          ColumnName [_, col] | col `A.isAttributeNameContained` rvattrs ->
                                -- the column has not been aliased, so we presume it can be use the column name directly
                                pure col
          _ -> throwSQLE $ traceShow ("attrnameforcolname"::String, rvattrs, colName) $ ColumnResolutionError colName
{-
attributeNameForColumnName :: ColumnName -> ConvertM AttributeName
attributeNameForColumnName colName = do
  s <- get
  case attributeNameForColumnName' colName s of
    Left err -> throwSQLE err
    Right al -> do
      traceStateM
      traceShowM ("attributeNameForColumnName"::String, colName, "->"::String, al)
      pure al
  -}

wrapTypeF :: TypeForRelExprF -> RelationalExpr -> ConvertM Relation
wrapTypeF typeF relExpr =
  case typeF relExpr of
    Left relError -> throwSQLE (SQLRelationalError relError)
    Right v -> pure v    


-- | Return the table alias for the column name iff the attribute is unique. Used for attribute resolution.
{-
tableAliasForColumnName :: TypeForRelExprF -> ColumnName -> TableContext -> Either SQLError TableAlias
-- the table alias is included
tableAliasForColumnName typeF cn@(ColumnName [tAlias, _]) (TableContext tMap) = do
  if M.member (TableAlias tAlias) tMap then
    pure (TableAlias tAlias)
    else
    Left (ColumnResolutionError cn)
tableAliasForColumnName typeF qn@(ColumnName [colName]) (TableContext tMap) = do
  --look up the column name in all possible tables
  res <- foldM folder Nothing (M.toList tMap)
  case res of
    Just res -> pure res
    Nothing -> Left (ColumnResolutionError qn)
  where
    folder :: Maybe ColumnName -> (TableAlias, RelationalExpr) -> _
    folder Just{} _ = Left (AmbiguousColumnResolutionError qn)
    folder Nothing (TableAlias tableAlias, (rvExpr,_)) = do
      tRel <- wrapTypeF typeF rvExpr -- we could cache this in the table alias map ADT
      --traceShowM ("findColName", rvExpr, tRel)
      if colName `S.member` attributeNameSet (attributes tRel) then
        pure (Just (ColumnName [tableAlias, colName]))
        else pure Nothing
-}
baseDFExpr :: DataFrameExpr
baseDFExpr = DataFrameExpr { convertExpr = MakeRelationFromExprs (Just []) (TupleExprs () [TupleExpr mempty]), --relationTrue if the table expression is empty "SELECT 1"
                             orderExprs = [],
                             offset = Nothing,
                             limit = Nothing }

falseDFExpr :: DataFrameExpr
falseDFExpr = DataFrameExpr { convertExpr = MakeRelationFromExprs (Just []) (TupleExprs () []), --relationFalse 
                             orderExprs = [],
                             offset = Nothing,
                             limit = Nothing }


convertQuery :: TypeForRelExprF -> Query -> ConvertM DataFrameExpr
convertQuery typeF (QuerySelect sel) = convertSelect typeF sel
convertQuery typeF (QueryValues vals) = do
  let convertTupleExprs tupVals = do
        TupleExpr . M.fromList <$> mapM (\(c, sexpr) -> do
                                            atomExpr <- convertScalarExpr typeF sexpr
                                            pure ("attr_" <> T.pack (show c), atomExpr)
                                        ) (zip [1::Int ..] tupVals)
  tupleExprs <- mapM convertTupleExprs vals
  pure (baseDFExpr { convertExpr = MakeRelationFromExprs Nothing (TupleExprs () tupleExprs) })
convertQuery _typeF (QueryTable tname) = do
  rvName <- convertTableName tname
  pure $ baseDFExpr { convertExpr = RelationVariable rvName () }
             
convertSelect :: TypeForRelExprF -> Select -> ConvertM DataFrameExpr
convertSelect typeF sel = do
  wExprs <- case withClause sel of
              Nothing -> pure mempty
              Just wClause -> do
                convertWithClause typeF wClause
  -- extract all mentioned tables into the table alias map for
  let typeF' = appendWithsToTypeF typeF wExprs
  (dfExpr, _colRemap) <- case tableExpr sel of
              Nothing -> pure (baseDFExpr, mempty)
              Just tExpr -> convertTableExpr typeF' tExpr
--  traceShowM ("table aliases", tAliasMap)              
  let explicitWithF = if null wExprs then id else With wExprs
              
  -- convert projection using table alias map to resolve column names
  projF <- convertProjection typeF' (projectionClause sel)
  -- add with clauses
  withAssocs <- tableAliasesAsWithNameAssocs
  let withF = case withAssocs of
                [] -> id
                _ -> With withAssocs
      finalRelExpr = explicitWithF (withF (projF (convertExpr dfExpr)))
  -- if we have only one table alias or the columns are all unambiguous, remove table aliasing of attributes
  -- apply rename reduction- this could be applied by the static query optimizer, but we do it here to simplify the tests so that they aren't polluted with redundant renames
  pure (dfExpr { convertExpr = finalRelExpr })


-- returns a new typeF function which adds type checking for "with" clause expressions
appendWithsToTypeF :: TypeForRelExprF -> WithNamesAssocs -> TypeForRelExprF
appendWithsToTypeF typeF withAssocs relExpr =
  case relExpr of
    expr@(RelationVariable x ()) -> case With.lookup x withAssocs of
                               Nothing -> typeF expr
                               Just matchExpr -> typeF matchExpr
    other -> typeF other
      

-- | Slightly different processing for subselects.
convertSubSelect :: TypeForRelExprF -> Select -> ConvertM RelationalExpr
convertSubSelect typeF sel = do
  ((applyF, tExpr), colRenames) <- withSubSelect $ do
    wExprs <- case withClause sel of
                Nothing -> pure mempty
                Just wClause -> do
                  convertWithClause typeF wClause
    let typeF' = appendWithsToTypeF typeF wExprs    
    (dfExpr, _colMap) <- case tableExpr sel of
                            Nothing -> pure (baseDFExpr, mempty)
                            Just tExpr -> convertTableExpr typeF' tExpr  
    when (usesDataFrameFeatures dfExpr) $ throwSQLE (NotSupportedError "ORDER BY/LIMIT/OFFSET in subquery")
--    traceShowM ("convertSubSelect"::String, colMap)
    let explicitWithF = if null wExprs then id else With wExprs    
    -- convert projection using table alias map to resolve column names
    projF <- convertProjection typeF' (projectionClause sel) -- the projection can only project on attributes from the subselect table expression
    -- add with clauses
    withAssocs <- tableAliasesAsWithNameAssocs
    let withF = case withAssocs of
                  [] -> id
                  _ -> With withAssocs
    -- add disambiguation renaming
--    traceShowM ("subselect tExpr"::String, convertExpr dfExpr)
    pure (explicitWithF . withF . projF, convertExpr dfExpr)
    
{-  let renamesF = Rename (S.fromList (map renamer (M.toList colRenames)))
      renamer ((TableAlias tAlias, realAttr), ColumnAlias newAttr) =
        (realAttr, newAttr)-}
  let renamedExpr = foldr renamerFolder tExpr (M.toList colRenames)
      renamerFolder ((TableAlias tAlias, oldAttrName), ColumnAlias newAttrName) acc =
        pushDownAttributeRename (S.singleton (oldAttrName, newAttrName)) (RelationVariable tAlias ()) acc
        
  pure (applyF renamedExpr)

convertSelectItem :: TypeForRelExprF -> SelectItemsConvertTask -> (Int,SelectItem) -> ConvertM SelectItemsConvertTask
convertSelectItem typeF acc (c,selItem) =
  case selItem of
    -- select * from x
    (Identifier (ColumnProjectionName [Asterisk]), Nothing) ->
      pure acc
    -- select sup.* from s as sup
    (Identifier qpn@(ColumnProjectionName [ProjectionName _, Asterisk]), Nothing) ->
      pure $ acc { taskProjections = S.insert qpn (taskProjections acc) }
    -- select a from x
    (Identifier qpn@(ColumnProjectionName [ProjectionName _col]), Nothing) -> do
      --look up unaliased column name
      _ <- colinfo qpn
      pure $ acc { taskProjections = S.insert qpn (taskProjections acc)
                 }
    -- select city as x from s        
    (Identifier qpn@(ColumnProjectionName [ProjectionName _]), Just newName@(ColumnAlias newNameTxt)) -> do
          pure $ acc { taskProjections = S.insert (ColumnProjectionName [ProjectionName newNameTxt]) (taskProjections acc),
                       taskRenames = taskRenames acc <> [(qpn, newName)] }
    -- select s.city from s
    (Identifier qpn@(ColumnProjectionName [ProjectionName tname, ProjectionName colname]), Nothing) -> do
      --lookup column renaming, if applicable
          pure $ acc { taskProjections = S.insert qpn (taskProjections acc),
                       taskRenames = taskRenames acc <> [(ColumnProjectionName [ProjectionName colname], ColumnAlias (T.intercalate "." [tname,colname]))] }
    -- other exprs
    (scalarExpr, mAlias) -> do
      let attrName' (Just (ColumnAlias nam)) _ = nam
          attrName' Nothing c' = "attr_" <> T.pack (show c')
      atomExpr <- convertProjectionScalarExpr typeF scalarExpr
      let newAttrName = attrName' mAlias c
      -- we need to apply the projections after the extension!
      pure $ acc { taskExtenders = AttributeExtendTupleExpr newAttrName atomExpr : taskExtenders acc,
                       taskProjections = S.insert (ColumnProjectionName [ProjectionName newAttrName]) (taskProjections acc)
                     }
  where
   colinfo (ColumnProjectionName [ProjectionName name]) = do
     findOneColumn (ColumnName [name])
   colinfo colProjName = throwSQLE $ UnexpectedColumnProjectionName colProjName

convertProjection :: TypeForRelExprF -> [SelectItem] -> ConvertM (RelationalExpr -> RelationalExpr)
convertProjection typeF selItems = do
--    traceShowM ("convertProjection", selItems)
    let emptyTask = SelectItemsConvertTask { taskProjections = S.empty,
                                             taskRenames = mempty,
                                             taskExtenders = mempty }
--        attrName' (Just (ColumnAlias nam)) _ = nam
--        attrName' Nothing c = "attr_" <> T.pack (show c)
    task <- foldM (convertSelectItem typeF) emptyTask (zip [1::Int ..] selItems)
    --apply projections
    fProjection <- if S.null (taskProjections task) then
                     pure id
                   else do
                     let projFolder (attrNames, b) (ColumnProjectionName [ProjectionName nam]) =
                           pure (S.insert nam attrNames, b)
                         projFolder (attrNames, b) (ColumnProjectionName [ProjectionName nameA, ProjectionName nameB]) =
                           pure $ (S.insert (T.concat [nameA, ".", nameB]) attrNames, b)
                         projFolder (attrNames, relExprAttributes) (ColumnProjectionName [ProjectionName tname, Asterisk]) =
                           pure $ (attrNames, relExprAttributes <> [tname])
                         projFolder _ colProjName = throwSQLE $ UnexpectedColumnProjectionName colProjName
                     (attrNames, relExprRvs) <- foldM projFolder mempty (S.toList (taskProjections task))
                     let attrsProj = A.some (map (\rv -> RelationalExprAttributeNames (RelationVariable rv ())) relExprRvs <> [AttributeNames attrNames])
                     pure $ Project attrsProj
    -- apply extensions
    let fExtended = foldr (\ext acc -> (Extend ext) . acc) id (taskExtenders task)
    -- apply rename
    renamesSet <- foldM (\acc (qProjName, (ColumnAlias newName)) -> do
                          oldName <- convertColumnProjectionName qProjName
                          pure $ S.insert (oldName, newName) acc) S.empty (taskRenames task)
    let fRenames = if S.null renamesSet then id else Rename renamesSet
    pure (fProjection . fExtended . fRenames)

convertUnqualifiedColumnName :: UnqualifiedColumnName -> AttributeName
convertUnqualifiedColumnName (UnqualifiedColumnName nam) = nam

convertColumnName :: ColumnName -> ConvertM AttributeName
convertColumnName colName = do
  attributeNameForColumnName colName

convertColumnProjectionName :: ColumnProjectionName -> ConvertM AttributeName
convertColumnProjectionName qpn@(ColumnProjectionName names) = do
  let namer (ProjectionName t) = pure t
      namer Asterisk = throwSQLE $ UnexpectedAsteriskError qpn
  names' <- mapM namer names
  convertColumnName (ColumnName names')
                      
        
convertTableExpr :: TypeForRelExprF -> TableExpr -> ConvertM (DataFrameExpr, ColumnAliasMap)
convertTableExpr typeF tExpr = do
    (fromExpr, columnMap) <- convertFromClause typeF (fromClause tExpr)
    whereF <- case whereClause tExpr of
      Just whereExpr -> do
        restrictPredExpr <- convertWhereClause typeF whereExpr
        pure $ Restrict restrictPredExpr
      Nothing -> pure id
    orderExprs' <- convertOrderByClause typeF (orderByClause tExpr)
    -- add disambiguation renaming
    let disambiguationRenamerF = if S.null renames then id else Rename renames
        renames = S.fromList $ foldr folder mempty (M.toList columnMap)
        whereAttrNames = S.map (\(ColumnName cs) -> T.intercalate "." cs) whereColNames
        whereColNames = maybe mempty columnNamesInRestrictionExpr (whereClause tExpr)
        folder (ColumnAlias alias, attrName) acc = -- include renamer only if the column is referenced and the renaming is not redundant
          if alias /= attrName && S.member alias whereAttrNames then
             (attrName, alias):acc
             else
            acc
    
    let dfExpr = DataFrameExpr { convertExpr = whereF (disambiguationRenamerF fromExpr),
                                 orderExprs = orderExprs',
                                 offset = offsetClause tExpr,
                                 limit = limitClause tExpr }
    pure (dfExpr, columnMap)

convertWhereClause :: TypeForRelExprF -> RestrictionExpr -> ConvertM RestrictionPredicateExpr
convertWhereClause typeF (RestrictionExpr rexpr) = do
    let wrongType t = throwSQLE $ TypeMismatchError t BoolAtomType --must be boolean expression
        coalesceBoolF expr = FunctionAtomExpr "sql_coalesce_bool" [expr] ()
    case rexpr of
      IntegerLiteral{} -> wrongType IntegerAtomType
      DoubleLiteral{} -> wrongType DoubleAtomType
      NullLiteral{} -> wrongType IntegerAtomType
      StringLiteral{} -> wrongType TextAtomType
      Identifier _i -> wrongType TextAtomType -- could be a better error here
      BooleanLiteral True -> 
        pure TruePredicate
      BooleanLiteral False ->
        pure (NotPredicate TruePredicate)
      BinaryOperator (Identifier colName) (OperatorName ["="]) exprMatch -> do --we don't know here if this results in a boolean expression, so we pass it down
        attrName <- attributeNameForColumnName colName
--        traceShowM ("convertWhereClause eq"::String, colName, attrName)
--        traceStateM
        expr' <- convertScalarExpr typeF exprMatch
        pure (AtomExprPredicate (coalesceBoolF (FunctionAtomExpr "sql_equals" [AttributeAtomExpr attrName, expr'] ())))
      BinaryOperator exprA op exprB -> do
        a <- convertScalarExpr typeF exprA
        b <- convertScalarExpr typeF exprB
        f <- lookupOperator op
        pure (AtomExprPredicate (coalesceBoolF (f [a,b])))
      PostfixOperator expr (OperatorName ops) -> do
        expr' <- convertScalarExpr typeF expr
--        traceShowM ("convertWhereClause"::String, expr')
        case ops of
          ["is", "null"] -> do
            pure $ AtomExprPredicate (coalesceBoolF (FunctionAtomExpr "sql_isnull" [expr'] ()))
          other -> throwSQLE $ NotSupportedError ("postfix operator: " <> T.pack (show other))
      InExpr inOrNotIn sexpr (InList matches') -> do
        eqExpr <- convertScalarExpr typeF sexpr
        case reverse matches' of
         (match:matches) -> do
           firstItem <- convertScalarExpr typeF match
           let inFunc a b = AtomExprPredicate (FunctionAtomExpr "sql_equals" [a,b] ())
               predExpr' = inFunc eqExpr firstItem
               folder predExpr'' sexprItem = do
                 item <- convertScalarExpr typeF sexprItem
                 pure $ OrPredicate (inFunc eqExpr item) predExpr''
           res <- foldM folder predExpr' matches --be careful here once we introduce NULLs
           case inOrNotIn of
             In -> pure res
             NotIn -> pure (NotPredicate res)
         [] -> throwSQLE $ NotSupportedError "empty IN() clause"
      ExistsExpr subQ -> do
        relExpr  <- convertSubSelect typeF subQ
        --pretty sure I have to rename attributes in both the top-level query and in this one to prevent attribute conflicts- we can't rename all the attributes in the subquery, because the renamer won't know which attributes actually refer to the top-level attributes- should we just prefix all attributes unconditionally or send a signal upstream to rename attributes? FIXME
        let rexpr' = Project A.empty relExpr
        pure (RelationalExprPredicate rexpr')
      other -> throwSQLE $ NotSupportedError ("where clause: " <> T.pack (show other))


convertScalarExpr :: TypeForRelExprF -> ScalarExpr -> ConvertM AtomExpr
convertScalarExpr typeF expr = do
    let naked = pure . NakedAtomExpr
    case expr of
      IntegerLiteral i -> naked (IntegerAtom i)
      DoubleLiteral d -> naked (DoubleAtom d)
      StringLiteral s -> naked (TextAtom s)
      BooleanLiteral True -> pure $ ConstructedAtomExpr "True" [] ()
      BooleanLiteral False -> pure $ ConstructedAtomExpr "False" [] ()
      -- we don't have enough type context with a cast, so we default to text
      NullLiteral -> pure $ ConstructedAtomExpr "SQLNull" [] ()
      Identifier i -> do
        AttributeAtomExpr <$> convertColumnName i
      BinaryOperator exprA op exprB -> do
        a <- convertScalarExpr typeF exprA
        b <- convertScalarExpr typeF exprB
        f <- lookupOperator op
        pure $ f [a,b]
      other -> throwSQLE $ NotSupportedError ("scalar expr: " <> T.pack (show other))

convertProjectionScalarExpr :: TypeForRelExprF -> ProjectionScalarExpr -> ConvertM AtomExpr
convertProjectionScalarExpr typeF expr = do
    let naked = pure . NakedAtomExpr
    case expr of
      IntegerLiteral i -> naked (IntegerAtom i)
      DoubleLiteral d -> naked (DoubleAtom d)
      StringLiteral s -> naked (TextAtom s)
      BooleanLiteral True -> pure $ ConstructedAtomExpr "True" [] ()
      BooleanLiteral False -> pure $ ConstructedAtomExpr "False" [] ()
      NullLiteral -> pure $ ConstructedAtomExpr "SQLNull" [] ()
      Identifier i ->
        AttributeAtomExpr <$> convertColumnProjectionName i
      BinaryOperator exprA op exprB -> do
        a <- convertProjectionScalarExpr typeF exprA
        b <- convertProjectionScalarExpr typeF exprB
        f <- lookupOperator op
        pure $ f [a,b]
      other -> throwSQLE $ NotSupportedError ("projection scalar expr: " <> T.pack (show other))

convertOrderByClause :: TypeForRelExprF -> [SortExpr] -> ConvertM [AttributeOrderExpr]
convertOrderByClause typeF exprs =
  mapM converter exprs
    where
      converter (SortExpr sexpr mDirection mNullsOrder) = do
        atomExpr <- convertScalarExpr typeF sexpr
        attrn <- case atomExpr of
                   AttributeAtomExpr aname -> pure aname
                   x -> throwSQLE (NotSupportedError (T.pack (show x)))
        let ordering = case mDirection of
                         Nothing -> AscendingOrder
                         Just Ascending -> AscendingOrder
                         Just Descending -> DescendingOrder
        case mNullsOrder of
          Nothing -> pure ()
          Just x -> throwSQLE (NotSupportedError (T.pack (show x)))
        pure (AttributeOrderExpr attrn ordering)
  

convertWithClause :: TypeForRelExprF -> WithClause -> ConvertM WithNamesAssocs
convertWithClause typeF wClause =
  mapM convertOneWith (NE.toList (withExprs wClause))
  where
    convertOneWith (WithExpr (WithExprAlias alias) sel) = do
      relExpr <- convertSubSelect typeF sel
      pure (WithNameExpr alias (), relExpr)

type ColumnRemap = M.Map ColumnName ColumnName

convertFromClause :: TypeForRelExprF -> [TableRef] -> ConvertM (RelationalExpr, ColumnAliasMap)
convertFromClause typeF (firstRef:trefs) = do
    --the first table ref must be a straight RelationVariable
  let convertFirstTableRef (SimpleTableRef (TableName [nam])) = do
        let rv = RelationVariable nam ()
        typeR <- wrapTypeF typeF rv
        colMap <- insertTable (TableAlias nam) rv (attributes typeR)
        pure (rv, colMap)
      convertFirstTableRef (AliasedTableRef (SimpleTableRef (TableName [nam])) al@(TableAlias alias)) = do
        let rv = RelationVariable nam ()
        typeR <- wrapTypeF typeF rv
        colMap <- insertTable al rv (attributes typeR)
        pure (RelationVariable alias (), colMap)
      convertFirstTableRef tref =
        throwSQLE $ NotSupportedError ("first table ref: " <> T.pack (show tref))
  (firstRel, colMap) <- convertFirstTableRef firstRef
  expr' <- foldM (joinTableRef typeF) firstRel (zip [1..] trefs)
  pure (expr', colMap)
convertFromClause _ [] = throwSQLE $ NotSupportedError "empty table refs"  

-- | Convert TableRefs after the first one (assumes all additional TableRefs are for joins). Returns the qualified name key that was added to the map, the underlying relexpr (not aliased so that it can used for extracting type information), and the new table context map
convertTableRef :: TypeForRelExprF -> TableRef -> ConvertM (TableAlias, RelationalExpr)
convertTableRef typeF tref =
  case tref of
    SimpleTableRef (TableName [nam]) -> do
      let rv = RelationVariable nam ()
          ta = TableAlias nam
      typeRel <- wrapTypeF typeF rv 
      _ <- insertTable ta rv (attributes typeRel)
      pure (ta, rv) -- include with clause even for simple cases because we use this mapping to 
    AliasedTableRef (SimpleTableRef (TableName [nam])) tAlias -> do
      typeRel <- wrapTypeF typeF (RelationVariable nam ())
      let rv = RelationVariable nam ()
      _ <- insertTable tAlias rv (attributes typeRel)
      pure $ (tAlias, RelationVariable nam ())
    x -> throwSQLE $ NotSupportedError ("table ref: " <> T.pack (show x))

  
joinTableRef :: TypeForRelExprF -> RelationalExpr -> (Int, TableRef) -> ConvertM RelationalExpr
joinTableRef typeF rvA (_c,tref) = do
      -- optionally prefix attributes unelss the expr is a RelationVariable
  let attrRenamer x expr attrs = do
        renamed <- mapM (renameOneAttr x expr) attrs
        pure (Rename (S.fromList renamed) expr)
      -- prefix all attributes
      prefixRenamer tAlias expr attrs = do
        renamed <- mapM (prefixOneAttr tAlias) attrs
        pure (Rename (S.fromList renamed) expr)
      prefixOneAttr tAlias@(TableAlias tPrefix) old_name = do
        -- insert into columnAliasMap
        let new_name = T.concat [tPrefix, ".", old_name]
--        traceShowM ("prefixOneAttr", tAlias, old_name, new_name)
        (ColumnAlias alias) <- noteColumnMention (Just tAlias) (ColumnName [old_name]) (Just (ColumnAlias new_name))
--        traceShowM ("joinTableRef prefixOneAttr", alias)
--        traceStateM
--        insertColumnAlias tAlias old_name (ColumnAlias new_name) (ColumnName [new_name])
--        addColumnAlias tAlias (ColumnAlias new_name) old_name
        pure (old_name, alias)
      renameOneAttr x expr old_name = do
--        traceShowM ("renameOneAttr", old_name, new_name)
        insertColumnAlias (TableAlias prefix) old_name (ColumnAlias new_name) (ColumnName [new_name])
--        addColumnAlias (TableAlias prefix) (ColumnAlias new_name) old_name
        pure (old_name, new_name)
        where
          new_name = T.concat [prefix, ".", old_name]
          prefix = case expr of
            RelationVariable rvName () -> rvName
            _ -> x -- probably need to return errors for some expressions
      crossJoin jtref = do
            --rename all columns to prefix them with a generated alias to prevent any natural join occurring, then perform normal join
            -- we need the type to get all the attribute names for both relexprs
            (_tKey, rvB) <- convertTableRef typeF jtref
            case typeF rvA of
              Left err -> throwSQLE (SQLRelationalError err)
              Right typeA ->
                case typeF rvB of
                  Left err -> throwSQLE (SQLRelationalError err)
                  Right typeB -> do
                    let attrsA = A.attributeNameSet (attributes typeA)
                        attrsB = A.attributeNameSet (attributes typeB)
                        attrsIntersection = S.intersection attrsA attrsB
                        --find intersection of attributes and rename all of them with prefix 'expr'+c+'.'
                    exprA <- attrRenamer "a" rvA (S.toList attrsIntersection)
                    pure (Join exprA rvB)
  case tref of
          SimpleTableRef tname -> -- a simple table ref in this position implies a cross join (no join condition unless it appears in the where clause)
            crossJoin (SimpleTableRef tname)
          NaturalJoinTableRef jtref -> do
            -- then natural join is the only type of join which the relational algebra supports natively
            (_, rvB) <- convertTableRef typeF jtref
            pure $ Join rvA rvB
          CrossJoinTableRef jtref -> crossJoin jtref
          InnerJoinTableRef jtref (JoinUsing qnames) -> do
            (tKey, rvB) <- convertTableRef typeF jtref
            let jCondAttrs = S.fromList $ map convertUnqualifiedColumnName qnames
            (attrsIntersection, _attrsA, _attrsB) <- commonAttributeNames typeF rvA rvB
            --rename attributes used in the join condition
            let attrsToRename = S.difference attrsIntersection jCondAttrs
--            traceShowM ("inner", attrsToRename, attrsIntersection, jCondAttrs)
                rvNameB = case tKey of
                            TableAlias ta -> ta
            exprA <- attrRenamer "a" rvA (S.toList attrsToRename)
            exprB <- prefixRenamer tKey (RelationVariable rvNameB ()) (S.toList attrsToRename)
            pure (Join exprA exprB)
            
          InnerJoinTableRef jtref (JoinOn (JoinOnCondition joinExpr)) -> do
            --create a cross join but extend with the boolean sexpr
            --extend the table with the join conditions, then join on those
            --exception: for simple attribute equality, use regular join renames using JoinOn logic
            
            (tKey, rvB) <- convertTableRef typeF jtref
            --rvA and rvB now reference potentially aliased relation variables (needs with clause to execute), but this is useful for making attributes rv-prefixed
--            traceShowM ("converted", rvA, rvB, tAliases)
            --extract all table aliases to create a remapping for SQL names discovered in the sexpr
            
            withExpr <- With <$> tableAliasesAsWithNameAssocs
            (_commonAttrs, attrsA, attrsB) <- commonAttributeNames typeF (withExpr rvA) (withExpr rvB)
            -- first, execute the rename, renaming all attributes according to their table aliases
            let rvPrefix rvExpr =
                  case rvExpr of
                    RelationVariable nam () -> pure nam
                    x -> throwSQLE $ NotSupportedError ("cannot derived name for relational expression " <> T.pack (show x))
                   
                rvNameB = case tKey of
                             TableAlias ta -> ta
            rvNameA <- rvPrefix rvA
--            rvPrefixB <- rvPrefix rvB
            exprA <- prefixRenamer (TableAlias rvNameA) rvA (S.toList attrsA)
            exprB <- prefixRenamer tKey (RelationVariable rvNameB ()) (S.toList attrsB)
--            traceShowM ("exprA", exprA)
--            traceShowM ("exprB", exprB)
            -- for the join condition, we can potentially extend to include all the join criteria columns, then project them away after constructing the join condition
            joinRe <- convertScalarExpr typeF joinExpr --' why are we renaming here- can't we call attributenameforcolumnname in the scalarexpr conversion???
            --let joinCommonAttrRenamer (RelationVariable rvName ()) old_name =
            --rename all common attrs and use the new names in the join condition
            let allAttrs = S.union attrsA attrsB
                firstAvailableName c allAttrs' =
                  let new_name = T.pack ("join_" <> show c) in
                  if S.member new_name allAttrs' then
                    firstAvailableName (c + 1) allAttrs'
                    else
                    new_name
                joinName = firstAvailableName (1::Int) allAttrs
                extender = AttributeExtendTupleExpr joinName (FunctionAtomExpr "sql_coalesce_bool" [joinRe] ())
                joinMatchRestriction = Restrict (AttributeEqualityPredicate joinName (ConstructedAtomExpr "True" [] ()))
                projectAwayJoinMatch = Project (InvertedAttributeNames (S.fromList [joinName]))
            pure (projectAwayJoinMatch (joinMatchRestriction (Extend extender (Join exprB exprA))))
          other -> throwSQLE $ NotSupportedError ("join: " <> T.pack (show other))

lookupOperator :: OperatorName -> ConvertM ([AtomExpr] -> AtomExpr)
lookupOperator (OperatorName nam) = lookupFunc (FuncName nam)

-- this could be amended to support more complex expressions such as coalesce by returning an [AtomExpr] -> AtomExpr function
lookupFunc :: FuncName -> ConvertM ([AtomExpr] -> AtomExpr)
lookupFunc qname =
  case qname of
    FuncName [nam] ->
      case lookup nam sqlFuncs of
        Nothing -> throwSQLE $ NoSuchSQLFunctionError qname
        Just match -> pure match
    other -> throwSQLE $ NotSupportedError ("function name: " <> T.pack (show other))
  where
    f n args = FunctionAtomExpr n args ()
    sqlFuncs = [(">",f "sql_gt"),
                 ("<",f "sql_lt"),
                 (">=",f "sql_gte"),
                 ("<=",f "sql_lte"),
                 ("=",f "sql_equals"),
                 ("!=",f "sql_not_equals"), -- function missing
                 ("<>",f "sql_not_equals"), -- function missing
                 ("+", f "sql_add"),
                 ("and", f "sql_and"),
                 ("or", f "sql_or")
               ]

-- | Used in join condition detection necessary for renames to enable natural joins.
commonAttributeNames :: TypeForRelExprF -> RelationalExpr -> RelationalExpr -> ConvertM (S.Set AttributeName, S.Set AttributeName, S.Set AttributeName)
commonAttributeNames typeF rvA rvB =
  case typeF rvA of
    Left err -> throwSQLE (SQLRelationalError err)
    Right typeA ->
      case typeF rvB of
        Left err -> throwSQLE (SQLRelationalError err)
        Right typeB -> do
          let attrsA = A.attributeNameSet (attributes typeA)
              attrsB = A.attributeNameSet (attributes typeB)
          pure $ (S.intersection attrsA attrsB, attrsA, attrsB)

-- | Used to remap SQL qualified names to new names to prevent conflicts in join conditions.
renameIdentifier :: (ColumnName -> ColumnName) -> ScalarExpr -> ScalarExpr
renameIdentifier renamer sexpr = Fold.cata renamer' sexpr
  where
    renamer' :: ScalarExprBaseF ColumnName ScalarExpr -> ScalarExpr
    renamer' (IdentifierF n) = Identifier (renamer n)
    renamer' x = Fold.embed x

-- find all column aliases in a scalar expression- useful for determining if a renamer needs to be applied
columnNamesInScalarExpr :: ScalarExpr -> S.Set ColumnName
columnNamesInScalarExpr expr = Fold.cata finder expr
  where
    finder :: ScalarExprBaseF ColumnName (S.Set ColumnName) -> S.Set ColumnName
    finder (IdentifierF n) = S.singleton n
    finder exprs = foldr S.union mempty exprs

columnNamesInRestrictionExpr :: RestrictionExpr -> S.Set ColumnName
columnNamesInRestrictionExpr (RestrictionExpr sexpr) = columnNamesInScalarExpr sexpr

-- | If the restriction includes a EXISTS expression, we must rename all attributes at the top-level to prevent conflicts.
needsToRenameAllAttributes :: RestrictionExpr -> Bool
needsToRenameAllAttributes (RestrictionExpr sexpr) =
  rec' sexpr
  where
  rec' sexpr' =
    case sexpr' of
      DoubleLiteral{} -> False
      StringLiteral{} -> False
      IntegerLiteral{} -> False
      NullLiteral{} -> False
      BooleanLiteral{} -> False
      Identifier{} -> False
      BinaryOperator e1 _ e2 -> rec' e1 || rec' e2
      PrefixOperator _ e1 -> rec' e1
      PostfixOperator e1 _ -> rec' e1
      BetweenOperator e1 _ e2 -> rec' e1 || rec' e2
      FunctionApplication _ e1 -> rec' e1
      CaseExpr cases else' -> or (map (\(whens, then') ->
                                          or (map rec' whens) || rec' then' || maybe False rec' else') cases)
      QuantifiedComparison{} -> True
      InExpr _ sexpr'' _ -> rec' sexpr''
      BooleanOperatorExpr e1 _ e2 -> rec' e1 || rec' e2
      ExistsExpr{} -> True

{-
":showexpr relation{tuple{val 4, children relation{tuple{val 6,children relation{tuple{}}}}},
                   tuple{val 10, children relation{tuple{val 1, children relation{tuple{}}},
                                                   tuple{val 2, children relation{tuple{}}}}}}
-}

-- rename an attribute within a relational expression
-- this really should be generalized to a standard fold or via recursion schemes
pushDownAttributeRename :: S.Set (AttributeName, AttributeName) -> RelationalExpr -> RelationalExpr -> RelationalExpr
pushDownAttributeRename renameSet matchExpr targetExpr =
  case targetExpr of
    _ | targetExpr == matchExpr ->
        Rename renameSet targetExpr
    x@MakeRelationFromExprs{} -> x
    x@MakeStaticRelation{} -> x
    x@ExistingRelation{} -> x
    x@RelationVariable{} -> x
    Project attrs expr -> Project attrs (push expr)
    Union exprA exprB -> Union (push exprA) (push exprB)
    Join exprA exprB -> Join (push exprA) (push exprB)
    Rename rset expr -> Rename (S.union rset renameSet) (push expr)
    Difference exprA exprB -> Difference (push exprA) (push exprB)
    B.Group gAttrs newAttr expr -> B.Group gAttrs newAttr (push expr)
    Ungroup attrName expr -> Ungroup attrName (push expr)
    Restrict rExpr expr -> Restrict (pushRestrict rExpr) (push expr)
    Equals exprA exprB -> Equals (push exprA) (push exprB)
    NotEquals exprA exprB -> NotEquals (push exprA) (push exprB)
    Extend eExpr expr -> Extend (pushExtend eExpr) (push expr)
    With wAssocs expr -> With wAssocs (push expr)
  where
    push expr = pushDownAttributeRename renameSet matchExpr expr
    pushRestrict expr =
      case expr of
        x@TruePredicate -> x
        AndPredicate eA eB -> AndPredicate (pushRestrict eA) (pushRestrict eB)
        OrPredicate eA eB -> OrPredicate (pushRestrict eA) (pushRestrict eB)
        NotPredicate e -> NotPredicate (pushRestrict e)
        RelationalExprPredicate rexpr -> RelationalExprPredicate (push rexpr)
        AtomExprPredicate aexpr -> AtomExprPredicate (pushAtom aexpr)
        AttributeEqualityPredicate attr aexpr -> AttributeEqualityPredicate attr (pushAtom aexpr)
    pushExtend (AttributeExtendTupleExpr attrName aexpr) =
      --should this rename the attrName, too?
      AttributeExtendTupleExpr attrName (pushAtom aexpr)
    pushAtom expr =
      case expr of
        x@AttributeAtomExpr{} -> x --potential rename
        x@NakedAtomExpr{} -> x
        FunctionAtomExpr fname args () -> FunctionAtomExpr fname (pushAtom <$> args) ()
        RelationAtomExpr e -> RelationAtomExpr (push e)
        ConstructedAtomExpr dConsName args () -> ConstructedAtomExpr dConsName (pushAtom <$> args) ()
        
mkTableContextFromDatabaseContext :: DatabaseContext -> TransactionGraph -> Either RelationalError TableContext
mkTableContextFromDatabaseContext dbc tgraph = do
  TableContext . M.fromList <$> mapM rvMapper (M.toList (relationVariables dbc))
  where
    rvMapper (nam, rvexpr) = do
      let gfEnv = freshGraphRefRelationalExprEnv (Just dbc) tgraph
      typeRel <- runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr rvexpr)
      pure (TableAlias nam,
            (RelationVariable nam (), attributes typeRel, mempty))

convertUpdate :: TypeForRelExprF -> Update -> ConvertM DatabaseContextExpr
convertUpdate typeF up = do
  let convertSetColumns (UnqualifiedColumnName colName, sexpr) = do
        (,) <$> pure colName <*> convertScalarExpr typeF sexpr
  atomMap <- M.fromList <$> mapM convertSetColumns (setColumns up)
  restrictionExpr <- case mRestriction up of
                       Nothing -> pure TruePredicate
                       Just restriction' -> convertWhereClause typeF restriction'
  rvname <- convertTableName (Update.target up)
  pure (B.Update rvname atomMap restrictionExpr)

convertTableName :: TableName -> ConvertM RelVarName
convertTableName (TableName [tname]) = pure tname
convertTableName t@TableName{} = throwSQLE (UnexpectedTableNameError t)

convertDBUpdates :: TypeForRelExprF -> [DBUpdate] -> ConvertM DatabaseContextExpr
convertDBUpdates typeF dbUpdates = MultipleExpr <$> mapM (convertDBUpdate typeF) dbUpdates

convertDBUpdate :: TypeForRelExprF -> DBUpdate -> ConvertM DatabaseContextExpr
convertDBUpdate typeF (UpdateUpdate up) = convertUpdate typeF up
convertDBUpdate typeF (UpdateInsert ins) = convertInsert typeF ins
convertDBUpdate typeF (UpdateDelete del) = convertDelete typeF del
convertDBUpdate typeF (UpdateCreateTable ct) = convertCreateTable typeF ct

convertInsert :: TypeForRelExprF -> Insert -> ConvertM DatabaseContextExpr
convertInsert typeF ins = do
  dfExpr <- convertQuery typeF (source ins)
  when (usesDataFrameFeatures dfExpr) $ throwSQLE (NotSupportedError "ORDER BY/LIMIT/OFFSET in subquery")
  -- check that all columns are mentioned because Project:M36 does not support default columns
  case typeF (convertExpr dfExpr) of
    Left err -> throwSQLE (SQLRelationalError err)
    Right rvExprType -> do
      let rvExprAttrNames = A.attributeNamesList (attributes rvExprType)
          insAttrNames = map convertUnqualifiedColumnName (Insert.targetColumns ins)
          rvExprColNameSet = S.map UnqualifiedColumnName (S.fromList rvExprAttrNames)
          insAttrColSet = S.fromList (Insert.targetColumns ins)
      when (length rvExprAttrNames /= length insAttrNames) $ throwSQLE (ColumnNamesMismatch rvExprColNameSet insAttrColSet)
      rvTarget <- convertTableName (Insert.target ins)
      -- insert into s(s#,sname,city,status) select * from s; -- we need to reorder attributes to align?
      -- rename attributes rexpr via query/values to map to targetCol attrs
      let insExpr = if rvExprColNameSet == insAttrColSet then -- if the attributes already align, don't perform any renaming
                      convertExpr dfExpr
                    else
                      Rename (S.fromList (filter rendundantRename (zip rvExprAttrNames insAttrNames))) (convertExpr dfExpr)
          rendundantRename (a,b) = a /= b
      traceShowM ("ins"::String, insExpr)
      pure $ B.Insert rvTarget insExpr

convertDelete :: TypeForRelExprF -> Delete.Delete -> ConvertM DatabaseContextExpr
convertDelete typeF del = do
  rvname <- convertTableName (Delete.target del)
  let rv = RelationVariable rvname ()
  case typeF rv of
    Left err -> throwSQLE (SQLRelationalError err)
    Right typeRel -> do
      _ <- insertTable (TableAlias rvname) rv (attributes typeRel)
      res <- convertWhereClause typeF (restriction del)
      pure (B.Delete rvname res)

convertCreateTable :: TypeForRelExprF -> CreateTable -> ConvertM DatabaseContextExpr
convertCreateTable _typeF ct = do
  rvTarget <- convertTableName (CreateTable.target ct)
  attrs <- convertColumnNamesAndTypes (CreateTable.targetColumns ct)
  pure (Define rvTarget attrs)

convertColumnNamesAndTypes :: [(UnqualifiedColumnName, ColumnType, PerColumnConstraints)] -> ConvertM [AttributeExpr]
convertColumnNamesAndTypes colAssocs =
  mapM mkAttributeExpr colAssocs
  where
    mkAttributeExpr (UnqualifiedColumnName colName, colType, constraints) = do
      aType <- convertColumnType colType constraints
      pure $ NakedAttributeExpr (Attribute colName aType)

convertColumnType :: ColumnType -> PerColumnConstraints -> ConvertM AtomType
convertColumnType colType constraints =
  mkAtomType $
    case colType of
      IntegerColumnType -> IntegerAtomType
      TextColumnType -> TextAtomType
      BoolColumnType -> BoolAtomType
      DoubleColumnType -> DoubleAtomType
      DateTimeColumnType -> DateTimeAtomType
  where
    mkAtomType aType =
      pure $ if notNullConstraint constraints then
               aType
             else
               nullAtomType aType

--convert SQL into relational or database context expressions
{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module ProjectM36.SQL.Convert where
import ProjectM36.Base as B
import ProjectM36.Error
import ProjectM36.DataTypes.SQL.Null
import ProjectM36.SQL.Select
import ProjectM36.DatabaseContext (someDatabaseContextExprs)
import ProjectM36.SQL.Insert as Insert
import ProjectM36.Key (databaseContextExprForUniqueKey, inclusionDependencyForKey)
import ProjectM36.SQL.DBUpdate
import ProjectM36.SQL.Update as Update
import ProjectM36.SQL.Delete as Delete
import ProjectM36.SQL.CreateTable as CreateTable
import ProjectM36.SQL.DropTable as DropTable
import ProjectM36.RelationalExpression
import ProjectM36.DataFrame (DataFrameExpr(..), AttributeOrderExpr(..), Order(..), usesDataFrameFeatures)
import ProjectM36.AttributeNames as A
import ProjectM36.Relation (attributes, atomTypeForName)
import qualified ProjectM36.Attribute as A
import qualified Data.Text as T
import qualified ProjectM36.WithNameExpr as With
import Control.Monad (foldM, when)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (intercalate, find)
import qualified Data.Functor.Foldable as Fold
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
--import Control.Monad (void)
import Control.Monad.Trans.State (StateT, get, put, runStateT, evalStateT)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (foldl')
import Data.Bifunctor (bimap)
--import qualified Data.HashSet as HS

--import Debug.Trace

{-
TODO
* enable duplicate rows by adding uuid column
-}

--over the course of conversion of a table expression, we collect all the table aliases we encounter, including non-aliased table references, including the type of the table, projections have their own name resolution system
newtype TableContext = TableContext (M.Map TableAlias (RelationalExpr, Attributes, ColumnAliasRemapper))
  deriving (Semigroup, Monoid, Show, Eq)

type TypeForRelExprF = RelationalExpr -> Either RelationalError Relation

type ConvertM = StateT TableContext (ExceptT SQLError Identity)

runConvertM :: TableContext -> ConvertM a -> Either SQLError (a, TableContext)
runConvertM tcontext m = runIdentity (runExceptT (runStateT m tcontext))

runLocalConvertM :: ConvertM a -> ConvertM a
runLocalConvertM m = do
  saveState <- get
  ret <- m
  put saveState
  pure ret

evalConvertM :: TableContext -> ConvertM a -> Either SQLError a
evalConvertM tcontext m = runIdentity (runExceptT (evalStateT m tcontext))

data SelectItemsConvertTask = SelectItemsConvertTask { taskProjections :: S.Set ColumnProjectionName,
                                                       taskRenames :: [(ColumnProjectionName, ColumnAlias)],
                                                       taskExtenders :: [ExtendTupleExpr],
                                                       taskGroups :: [S.Set ColumnProjectionName]
                                                     } deriving (Show, Eq)

emptyTask :: SelectItemsConvertTask
emptyTask = SelectItemsConvertTask { taskProjections = S.empty,
                                     taskRenames = mempty,
                                     taskGroups = mempty,
                                     taskExtenders = mempty }

                                          
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


{-  
traceStateM :: ConvertM ()
traceStateM = do
  s <- get
  traceM (prettyTableContext s)
-}

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
  tc@(TableContext tcontext) <- get
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
            insertColAlias (maybe tPrefixColAttr unColumnAlias mColAlias)
          Just (_, _, colAlRemapper) -> do
            -- table alias already known, check for column alias
            case attributeNameForAttributeAlias colAttr colAlRemapper of
              Left _ -> do
                -- col alias missing, so add it- figure out if it needs a table prefix
                let sqlColAlias = maybe colAttr unColumnAlias mColAlias
                case findNotedColumn' (ColumnName [colAttr]) tc of
                               Left _ -> -- no match, so table prefix not required
                                 insertColAlias sqlColAlias
                               Right [] -> -- no match, so table prefix not required
                                 insertColAlias sqlColAlias
                               Right [_] -> -- we have a match, so we need the table prefix
                                 insertColAlias (maybe tPrefixColAttr unColumnAlias mColAlias)
                               Right (_:_) -> throwSQLE (AmbiguousColumnResolutionError colName)
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
              sqlColAlias = maybe colAlias unColumnAlias mColAlias
      
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

lookupTable :: TableAlias -> ConvertM (RelationalExpr, Attributes, ColumnAliasRemapper)
lookupTable ta = do
  (TableContext map') <- get
  case M.lookup ta map' of
    Nothing -> throwSQLE (MissingTableReferenceError ta)
    Just res -> pure res

-- | Find a column name or column alias in the underlying table context. Returns key into table context.
findColumn :: ColumnName -> ConvertM [TableAlias]
findColumn targetCol = 
  findColumn' targetCol <$> get
  
-- | non ConvertM version of findColumn
findColumn' :: ColumnName -> TableContext -> [TableAlias]
findColumn' targetCol (TableContext tMap) = do
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
            (ColumnAlias al) <- noteColumnMention (Just tKey) (ColumnName [tAlias,colAttr]) Nothing
            pure al
          Left err -> throwSQLE err
      else
        case colName of
          ColumnName [_, col] | col `A.isAttributeNameContained` rvattrs ->
                                -- the column has not been aliased, so we presume it can be use the column name directly
                                pure col
          _ -> throwSQLE $ ColumnResolutionError colName

wrapTypeF :: TypeForRelExprF -> RelationalExpr -> ConvertM Relation
wrapTypeF typeF relExpr =
  case typeF relExpr of
    Left relError -> throwSQLE (SQLRelationalError relError)
    Right v -> pure v    

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
convertQuery typeF (QueryOp op q1 q2) = do
  let dfErr = NotSupportedError ("ORDER BY/LIMIT/OFFSET in " <> T.pack (show op))
  dfExpr1 <- runLocalConvertM (convertQuery typeF q1)
  when (usesDataFrameFeatures dfExpr1) $ throwSQLE dfErr  
  dfType1 <- case typeF (convertExpr dfExpr1) of
              Left err -> throwSQLE (SQLRelationalError err)
              Right t -> pure t

  dfExpr2 <- runLocalConvertM (convertQuery typeF q2)
  when (usesDataFrameFeatures dfExpr2) $ throwSQLE dfErr   
  dfType2 <- case typeF (convertExpr dfExpr2) of
               Left err -> throwSQLE (SQLRelationalError err)
               Right t -> pure t

  when (dfType1 /= dfType2) $ throwSQLE (QueryOperatorTypeMismatchError op (attributes dfType1) (attributes dfType2))

  let relOp = case op of
        UnionQueryOperator -> Union
        ExceptQueryOperator -> Difference
        IntersectQueryOperator -> Join

  pure $ baseDFExpr { convertExpr = relOp (convertExpr dfExpr1) (convertExpr dfExpr2) }
             
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
  let explicitWithF = if null wExprs then id else With wExprs
      (groupByExprs, havingExpr) = case tableExpr sel of
                                     Nothing -> ([],Nothing)
                                     Just texpr -> (groupByClause texpr, havingClause texpr)
  -- convert projection using table alias map to resolve column names
  projF <- convertProjection typeF' (projectionClause sel) groupByExprs havingExpr
  -- add with clauses
  withAssocs <- tableAliasesAsWithNameAssocs
  let withF = case withAssocs of
                [] -> id
                _ -> With withAssocs
      finalRelExpr = explicitWithF (withF (projF (convertExpr dfExpr)))
  -- if we have only one table alias or the columns are all unambiguous, remove table aliasing of attributes
  -- apply rename reduction- this could be applied by the static query optimizer, but we do it here to simplify the tests so that they aren't polluted with redundant renames
--  traceShowM ("finalExpr"::String, finalRelExpr)
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
    let explicitWithF = if null wExprs then id else With wExprs    
    -- convert projection using table alias map to resolve column names
    projF <- convertProjection typeF' (projectionClause sel) [] Nothing -- the projection can only project on attributes from the subselect table expression
    -- add with clauses
    withAssocs <- tableAliasesAsWithNameAssocs
    let withF = case withAssocs of
                  [] -> id
                  _ -> With withAssocs
    -- add disambiguation renaming
    pure (explicitWithF . withF . projF, convertExpr dfExpr)
    
  let renamedExpr = foldr renamerFolder tExpr (M.toList colRenames)
      renamerFolder ((TableAlias tAlias, oldAttrName), ColumnAlias newAttrName)=
        pushDownAttributeRename (S.singleton (oldAttrName, newAttrName)) (RelationVariable tAlias ())
        
  pure (applyF renamedExpr)

convertSelectItem :: TypeForRelExprF -> SelectItemsConvertTask -> (Int, SelectItem) -> ConvertM SelectItemsConvertTask
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
          newAttrName = attrName' mAlias c
      atomExpr <- processSQLAggregateFunctions <$> convertProjectionScalarExpr typeF scalarExpr
      -- we need to apply the projections after the extension!
      pure $ acc { taskExtenders = AttributeExtendTupleExpr newAttrName atomExpr : taskExtenders acc,
                   taskProjections = S.insert (ColumnProjectionName [ProjectionName newAttrName]) (taskProjections acc)
                     }
  where
   colinfo (ColumnProjectionName [ProjectionName name]) = do
     findOneColumn (ColumnName [name])
   colinfo colProjName = throwSQLE $ UnexpectedColumnProjectionName colProjName

convertProjection :: TypeForRelExprF -> [SelectItem] -> [GroupByExpr] -> Maybe HavingExpr -> ConvertM (RelationalExpr -> RelationalExpr)
convertProjection typeF selItems groupBys havingExpr = do
    groupInfo <- convertGroupBy typeF groupBys havingExpr selItems
    task <- foldM (convertSelectItem typeF) emptyTask (zip [1::Int ..] selItems)
    -- SQL supports only one grouping at a time, but multiple aggregations, so we create the group as attribute "_sql_aggregate" and the aggregations as fold projections on it
    fGroup <- if not (null (nonAggregates groupInfo)) ||
                 (null (nonAggregates groupInfo) && not (null (aggregates groupInfo)))
                 -- special case: SELECT max(status) FROM city- handle aggregations without GROUP BY                 
              then
                pure $ Group (InvertedAttributeNames
                              (S.fromList (map fst (nonAggregates groupInfo)))) "_sql_aggregate"
              else
                pure id
    let coalesceBoolF expr = FunctionAtomExpr "sql_coalesce_bool" [expr] ()                
    fGroupHavingExtend <- 
      case havingRestriction groupInfo of
        Nothing -> pure id
        Just sexpr -> do
          convertedAtomExpr <- convertProjectionScalarExpr typeF sexpr
          let atomExpr = processSQLAggregateFunctions convertedAtomExpr
          pure $ Extend (AttributeExtendTupleExpr "_sql_having" (coalesceBoolF atomExpr))
    let fGroupRestriction = case havingRestriction groupInfo of
                              Nothing -> id
                              Just _ ->
                                  Restrict (AttributeEqualityPredicate "_sql_having" (NakedAtomExpr (BoolAtom True)))
    --apply projections
    fProjection <- if S.null (taskProjections task) then
                     pure id
                   else do
                     let projFolder (attrNames, b) (ColumnProjectionName [ProjectionName nam]) =
                           pure (S.insert nam attrNames, b)
                         projFolder (attrNames, b) (ColumnProjectionName [ProjectionName nameA, ProjectionName nameB]) =
                           pure (S.insert (T.concat [nameA, ".", nameB]) attrNames, b)
                         projFolder (attrNames, relExprAttributes) (ColumnProjectionName [ProjectionName tname, Asterisk]) =
                           pure (attrNames, relExprAttributes <> [tname])
                         projFolder _ colProjName = throwSQLE $ UnexpectedColumnProjectionName colProjName
                     (attrNames, relExprRvs) <- foldM projFolder mempty (S.toList (taskProjections task))
                     let attrsProj = A.some (map (\rv -> RelationalExprAttributeNames (RelationVariable rv ())) relExprRvs <> [AttributeNames attrNames])
                     pure $ Project attrsProj
    -- apply extensions
    let fExtended = foldr (\ext acc -> Extend ext . acc) id (taskExtenders task)
    -- process SQL aggregates by replacing projections
    -- apply rename
    renamesSet <- foldM (\acc (qProjName, ColumnAlias newName) -> do
                          oldName <- convertColumnProjectionName qProjName
                          pure $ S.insert (oldName, newName) acc) S.empty (taskRenames task)
    let fRenames = if S.null renamesSet then id else Rename renamesSet
    pure (fGroupRestriction . fProjection . fGroupHavingExtend . fExtended . fRenames . fGroup)

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
        sqlEq l = FunctionAtomExpr "sql_equals" l ()
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
        expr' <- convertScalarExpr typeF exprMatch
        pure (AtomExprPredicate (coalesceBoolF (FunctionAtomExpr "sql_equals" [AttributeAtomExpr attrName, expr'] ())))
      BinaryOperator exprA op exprB -> do
        a <- convertScalarExpr typeF exprA
        b <- convertScalarExpr typeF exprB
        f <- lookupOperator False op
        pure (AtomExprPredicate (coalesceBoolF (f [a,b])))
      PostfixOperator expr (OperatorName ops) -> do
        expr' <- convertScalarExpr typeF expr
        let isnull = AtomExprPredicate (coalesceBoolF (FunctionAtomExpr "sql_isnull" [expr'] ()))
        case ops of
          ["is", "null"] -> 
            pure isnull
          ["is", "not", "null"] -> 
            pure (NotPredicate isnull)
          other -> throwSQLE $ NotSupportedError ("postfix operator: " <> T.pack (show other))
      InExpr inOrNotIn sexpr (InList matches') -> do
        eqExpr <- convertScalarExpr typeF sexpr
        case reverse matches' of
         (match:matches) -> do
           firstItem <- convertScalarExpr typeF match
           let predExpr' = sqlEq [eqExpr, firstItem]
               folder predExpr'' sexprItem = do
                 item <- convertScalarExpr typeF sexprItem
                 pure $ FunctionAtomExpr "sql_or" [sqlEq [eqExpr,item], predExpr''] ()
           res <- AtomExprPredicate . coalesceBoolF <$> foldM folder predExpr' matches 
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
      BooleanLiteral True -> naked (BoolAtom True)
      -- pure $ ConstructedAtomExpr "True" [] ()
      BooleanLiteral False -> naked (BoolAtom False)
      --pure $ ConstructedAtomExpr "False" [] ()
      -- we don't have enough type context with a cast, so we default to text
      NullLiteral -> pure $ ConstructedAtomExpr "SQLNullOfUnknownType" [] ()
      Identifier i -> do
        AttributeAtomExpr <$> convertColumnName i
      BinaryOperator exprA op exprB -> do
        a <- convertScalarExpr typeF exprA
        b <- convertScalarExpr typeF exprB
        f <- lookupOperator False op
        pure $ f [a,b]
      FunctionApplication funcName' fargs -> do
        func <- lookupFunc funcName'
        fargs' <- mapM (convertScalarExpr typeF) fargs
        pure (func fargs')
      other -> throwSQLE $ NotSupportedError ("scalar expr: " <> T.pack (show other))

-- SQL conflates projection and extension so we use the SQL context name here
convertProjectionScalarExpr :: TypeForRelExprF -> ProjectionScalarExpr -> ConvertM AtomExpr
convertProjectionScalarExpr typeF expr = do
    let naked = pure . NakedAtomExpr
    case expr of
      IntegerLiteral i -> naked (IntegerAtom i)
      DoubleLiteral d -> naked (DoubleAtom d)
      StringLiteral s -> naked (TextAtom s)
      BooleanLiteral True ->
        naked (BoolAtom True)
        --pure $ ConstructedAtomExpr "True" [] ()
      BooleanLiteral False ->
        naked (BoolAtom False)
        --pure $ ConstructedAtomExpr "False" [] ()
      NullLiteral -> pure $ ConstructedAtomExpr "SQLNullOfUnknownType" [] ()
      Identifier i -> do
        AttributeAtomExpr <$> convertColumnProjectionName i
      BinaryOperator exprA op exprB -> do
        a <- convertProjectionScalarExpr typeF exprA
        b <- convertProjectionScalarExpr typeF exprB
        f <- lookupOperator False op
        pure $ f [a,b]
      FunctionApplication fname fargs -> do
        func <- lookupFunc fname
        -- as a special case, count(*) is valid, if non-sensical SQL, so handle it here
        fargs' <- if fname == FuncName ["count"] && fargs == [Identifier (ColumnProjectionName [Asterisk])] then
                   pure [AttributeAtomExpr "_sql_aggregate"]
                 else 
                   mapM (convertProjectionScalarExpr typeF) fargs
        pure (func fargs')
      PrefixOperator op sexpr -> do
        func <- lookupOperator True op
        arg <- convertProjectionScalarExpr typeF sexpr
        pure (func [arg])
      CaseExpr conditionals mElse -> do
        let coalesceBoolF expr' = FunctionAtomExpr "sql_coalesce_bool" [expr'] ()
        conditionals' <- mapM (\(ifExpr, thenExpr) -> do
                                  ifE <- coalesceBoolF <$> convertProjectionScalarExpr typeF ifExpr
                                  thenE <- convertProjectionScalarExpr typeF thenExpr
                                  
                                  pure (ifE, thenE)
                              ) conditionals

        elseExpr <- case mElse of
                      Nothing -> pure $ NakedAtomExpr $ nullAtom (TypeVariableType "a") Nothing --will the engine resolve this type variable?
                      Just expr' -> convertProjectionScalarExpr typeF expr'
        let ifThenFolder acc (ifE, thenE) = IfThenAtomExpr ifE thenE acc
        pure $ foldl' ifThenFolder elseExpr conditionals'
      other -> throwSQLE $ NotSupportedError ("projection scalar expr: " <> T.pack (show other))

convertOrderByClause :: TypeForRelExprF -> [SortExpr] -> ConvertM [AttributeOrderExpr]
convertOrderByClause typeF =
  mapM converter
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
      pure (tAlias, RelationVariable nam ())
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
                --joinMatchRestriction = Restrict (AttributeEqualityPredicate joinName (ConstructedAtomExpr "True" [] ()))
                joinMatchRestriction = Restrict (AttributeEqualityPredicate joinName (NakedAtomExpr (BoolAtom True)))
                projectAwayJoinMatch = Project (InvertedAttributeNames (S.fromList [joinName]))
            pure (projectAwayJoinMatch (joinMatchRestriction (Extend extender (Join exprB exprA))))
          other -> throwSQLE $ NotSupportedError ("join: " <> T.pack (show other))

lookupOperator :: Bool -> OperatorName -> ConvertM ([AtomExpr] -> AtomExpr)
lookupOperator isPrefix op@(OperatorName nam)
  | isPrefix = do
      let f n args = FunctionAtomExpr n args ()
      case nam of
        ["-"] -> pure $ f "sql_negate"
        _ -> throwSQLE $ NoSuchSQLOperatorError op
  | otherwise =
      lookupFunc (FuncName nam)


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
    aggMapper (FuncName [nam], nam') = (nam, f nam')
    aggMapper (FuncName other,_) = error ("unexpected multi-component SQL aggregate function: " <> show other)
    sqlFuncs = [(">",f "sql_gt"),
                 ("<",f "sql_lt"),
                 (">=",f "sql_gte"),
                 ("<=",f "sql_lte"),
                 ("=",f "sql_equals"),
                 ("!=",f "sql_not_equals"), -- function missing
                 ("<>",f "sql_not_equals"), -- function missing
                 ("+", f "sql_add"),
                 ("and", f "sql_and"),
                 ("or", f "sql_or"),
                 ("abs", f "sql_abs")
               ] <> map aggMapper aggregateFunctions


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
          pure (S.intersection attrsA attrsB, attrsA, attrsB)

-- | Used to remap SQL qualified names to new names to prevent conflicts in join conditions.
renameIdentifier :: (ColumnName -> ColumnName) -> ScalarExpr -> ScalarExpr
renameIdentifier renamer = Fold.cata renamer'
  where
    renamer' :: ScalarExprBaseF ColumnName ScalarExpr -> ScalarExpr
    renamer' (IdentifierF n) = Identifier (renamer n)
    renamer' x = Fold.embed x

-- find all column aliases in a scalar expression- useful for determining if a renamer needs to be applied
columnNamesInScalarExpr :: ScalarExpr -> S.Set ColumnName
columnNamesInScalarExpr = Fold.cata finder
  where
    finder :: ScalarExprBaseF ColumnName (S.Set ColumnName) -> S.Set ColumnName
    finder (IdentifierF n) = S.singleton n
    finder sexpr = foldr S.union mempty sexpr

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
      FunctionApplication _ e1 -> or (rec' <$> e1)
      CaseExpr cases else' -> any (\(when', then') ->
                                          rec' when' || rec' then' || maybe False rec' else') cases
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
    x@RelationValuedAttribute{} -> x
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
    push = pushDownAttributeRename renameSet matchExpr
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
        IfThenAtomExpr ifE thenE elseE -> IfThenAtomExpr (pushAtom ifE) (pushAtom thenE) (pushAtom elseE)
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
        (,) colName <$> convertScalarExpr typeF sexpr
  atomMap <- M.fromList <$> mapM convertSetColumns (setColumns up)
  rvname <- convertTableName (Update.target up)
  let rv = RelationVariable rvname ()  
  case typeF rv of
    Left err -> throwSQLE (SQLRelationalError err)
    Right typeRel -> do
      _ <- insertTable (TableAlias rvname) rv (attributes typeRel)
      restrictionExpr <- case mRestriction up of
                           Nothing -> pure TruePredicate
                           Just restriction' -> convertWhereClause typeF restriction'
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
convertDBUpdate typeF (UpdateDropTable dt) = convertDropTable typeF dt

convertInsert :: TypeForRelExprF -> Insert -> ConvertM DatabaseContextExpr
convertInsert typeF ins = do
  -- check that all columns are mentioned because Project:M36 does not support default columns
  rvTarget <- convertTableName (Insert.target ins)
  let eRvTargetType = typeF (RelationVariable rvTarget ())
  case eRvTargetType of
    Left err -> throwSQLE (SQLRelationalError err)
    Right rvTargetType -> do
      -- if types do not align due to nullability, then add SQLJust
      dfExpr <- convertQuery typeF (source ins)
      when (usesDataFrameFeatures dfExpr) $ throwSQLE (NotSupportedError "ORDER BY/LIMIT/OFFSET in subquery")
      case typeF (convertExpr dfExpr) of
        Left err -> throwSQLE (SQLRelationalError err)
        Right rvExprType -> do
          let rvExprAttrNames = A.attributeNamesList (attributes rvExprType)
              insAttrNames = map convertUnqualifiedColumnName (Insert.targetColumns ins)
              rvExprColNameSet = S.map UnqualifiedColumnName (S.fromList rvExprAttrNames)
              insAttrColSet = S.fromList (Insert.targetColumns ins)
          when (length rvExprAttrNames /= length insAttrNames) $ throwSQLE (ColumnNamesMismatch rvExprColNameSet insAttrColSet)
    

          -- insert into s(s#,sname,city,status) select * from s; -- we need to reorder attributes to align?
          -- rename attributes rexpr via query/values to map to targetCol attrs
          let atomTypeForName' attrName type' =
                case atomTypeForName attrName type' of
                  Left err -> throwSQLE (SQLRelationalError err)
                  Right targetType -> pure targetType
              ren a b (Rename names expr) = Rename (S.insert (a,b) names) expr
              ren a b e = Rename (S.singleton (a, b)) e
              sqlPrefix s = "_sql_" <> s
              projHide n = Project (InvertedAttributeNames (S.singleton n))
              -- if one of the types is a nullable version of the other
--              isSQLNullableCombo t1 t2 = isSQLNullableSpecificType t1 t2 || isSQLNullableSpecificType t2 t1
              sqlNullMorpher interName targetName targetType t2 expr
                | isSQLNullableSpecificType targetType t2 = -- targetType is nullable version of t2
                  Extend (AttributeExtendTupleExpr targetName (ConstructedAtomExpr "SQLJust" [AttributeAtomExpr interName] ())) expr
                | otherwise = expr
               
          let typeMatchRenamer acc (targetAttrName, sourceAttrName) = do
                targetType <- atomTypeForName' targetAttrName rvTargetType
                insType <- atomTypeForName' sourceAttrName rvExprType
                if targetType == insType && targetAttrName == sourceAttrName then --nothing to do
                  pure acc
                  else if targetAttrName /= sourceAttrName &&
                          targetType == insType then do
                          --simple rename
                         pure $ ren sourceAttrName targetAttrName acc
                  else if targetAttrName == sourceAttrName &&
                          targetType /= insType &&
                          isSQLNullableSpecificType targetType insType
                  then do -- we need to extend the expr, but we want to use the targetName, so we have to rename it twice
                         let intermediateName = sqlPrefix targetAttrName
                         pure $ ren intermediateName targetAttrName (sqlNullMorpher intermediateName targetAttrName targetType insType (ren sourceAttrName intermediateName acc))
                  else if targetAttrName /= sourceAttrName &&
                          targetType /= insType &&
                          isSQLNullableSpecificType targetType insType then do
                         -- we extend the expr, but don't need an intermediate rename
                         pure $ projHide sourceAttrName (Extend (AttributeExtendTupleExpr targetAttrName (ConstructedAtomExpr "SQLJust" [AttributeAtomExpr sourceAttrName] ())) acc)
                  else if targetAttrName == sourceAttrName &&
                          isSQLNullUnknownType insType &&
                          isNullAtomType targetType then do
                         case atomTypeFromSQLNull targetType of
                           Nothing -> do
                             pure acc
                           -- replace null of unknown type with typed null
                           Just atype -> do
                               pure $ Extend (AttributeExtendTupleExpr targetAttrName (NakedAtomExpr (nullAtom atype Nothing))) (projHide sourceAttrName acc)
                  else if targetAttrName /= sourceAttrName &&
                          isSQLNullUnknownType insType &&
                          isNullAtomType targetType then do
                         case atomTypeFromSQLNull targetType of
                           Nothing -> do
                             pure acc
                           -- replace null of unknown type with typed null
                           Just _atype -> do
                               pure $ projHide sourceAttrName $ Extend (AttributeExtendTupleExpr targetAttrName (ConstructedAtomExpr "SQLNull" [] ())) acc
                  else
                         pure acc
                         
          insExpr <- foldM typeMatchRenamer (convertExpr dfExpr) (zip insAttrNames rvExprAttrNames)
{-          let insExpr = if rvExprColNameSet == insAttrColSet then -- if the attributes already align, don't perform any renaming
                          convertExpr dfExpr
                        else
                          Rename (S.fromList (filter rendundantRename (zip rvExprAttrNames insAttrNames))) (convertExpr dfExpr)
              rendundantRename (a,b) = a /= b-}
{-          traceShowM ("source ins"::String, source ins)
          traceShowM ("source ins converted"::String, convertExpr dfExpr)
          traceShowM ("ins converted"::String, insExpr)
          traceShowM ("rvTargetType"::String, rvTargetType)-}

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
  (attrs, constraintExprs) <- convertColumnNamesAndTypes rvTarget (CreateTable.targetColumns ct)
  pure (someDatabaseContextExprs (Define rvTarget attrs : constraintExprs))

convertDropTable :: TypeForRelExprF -> DropTable -> ConvertM DatabaseContextExpr
convertDropTable _typeF dt = do
  rvTarget <- convertTableName (DropTable.target dt)
  pure (Undefine rvTarget)

convertColumnNamesAndTypes :: RelVarName -> [(UnqualifiedColumnName, ColumnType, PerColumnConstraints)] -> ConvertM ([AttributeExpr], [DatabaseContextExpr])
convertColumnNamesAndTypes rvName =
  foldM processColumn mempty
  where
    processColumn acc (ucn@(UnqualifiedColumnName colName), colType, constraints) = do
      aTypeCons <- convertColumnType colType constraints
      constraintExprs <- convertPerColumnConstraints rvName ucn constraints
      pure ( fst acc <> [AttributeAndTypeNameExpr colName aTypeCons ()],
             constraintExprs <> snd acc)

convertColumnType :: ColumnType -> PerColumnConstraints -> ConvertM TypeConstructor
convertColumnType colType constraints = do
  let mkTypeCons aType =
        let typeName = T.dropEnd (length ("AtomType"::String)) (T.pack (show aType))
            tCons = ADTypeConstructor typeName []
        in
          if notNullConstraint constraints then
            tCons
          else
            ADTypeConstructor "SQLNullable" [tCons]
      colTCons = mkTypeCons $
             case colType of
               IntegerColumnType -> IntegerAtomType
               TextColumnType -> TextAtomType
               BoolColumnType -> BoolAtomType
               DoubleColumnType -> DoubleAtomType
               DateTimeColumnType -> DateTimeAtomType
               DateColumnType -> DayAtomType
               ByteaColumnType -> ByteStringAtomType
  pure colTCons

convertPerColumnConstraints :: RelVarName -> UnqualifiedColumnName -> PerColumnConstraints -> ConvertM [DatabaseContextExpr]
convertPerColumnConstraints rvname (UnqualifiedColumnName colName) constraints = do
  -- NOT NULL constraints are already enforced by the column type
  fkExprs <- case references constraints of
               Nothing -> pure []        
               Just (TableName [fkTableName], UnqualifiedColumnName fkColName) -> do
                 let fkIncDepName = rvname <> "_" <> colName <> "__" <> fkTableName <> "_" <> fkColName <> "_fk"
                     mkFK = InclusionDependency (Project (AttributeNames (S.singleton colName)) (RelationVariable rvname ())) (Project (AttributeNames (S.singleton fkColName)) (RelationVariable fkTableName ()))

                 pure [AddInclusionDependency fkIncDepName mkFK]
               Just (TableName fkTableNames, UnqualifiedColumnName fkColName) ->
                 throwSQLE (NotSupportedError ("schema-qualified table name in fk constraint: " <> T.pack (show fkTableNames) <> " " <> fkColName))
  -- the uniqueness constraint in SQL does not consider NULLs to be equal by default
  let uniqueExprs = if uniquenessConstraint constraints then
                      if notNullConstraint constraints then
                        [databaseContextExprForUniqueKey rvname [colName]]
                      else
                        [databaseContextExprForUniqueKeyWithNull rvname colName]
                    else
                      []
  pure $ uniqueExprs <> fkExprs

databaseContextExprForUniqueKeyWithNull :: RelVarName -> AttributeName -> DatabaseContextExpr
databaseContextExprForUniqueKeyWithNull rvname attrName =
  AddInclusionDependency incDepName incDep
  where
    incDep = inclusionDependencyForKey (AttributeNames (S.singleton attrName)) (Restrict notNull (RelationVariable rvname ()))
    incDepName = rvname <> "_" <> attrName <> "_unique"
    notNull = NotPredicate (AtomExprPredicate (FunctionAtomExpr "sql_isnull" [AttributeAtomExpr attrName] ()))
                                  

{-
select city,max(status) from s group by city;
(((s{city,status}) group ({status} as sub)) : {status2:=max(@sub)}){city,status2} rename {status2 as status}

before: Project (AttributeNames (fromList ["attr_2","city"])) (Extend (AttributeExtendTupleExpr "attr_2" (FunctionAtomExpr "sql_max" [AttributeAtomExpr "status"] ())) (RelationVariable "s" ()))

after: Rename (fromList [("status2","status")]) (Project (AttributeNames (fromList ["city","status2"])) (Extend (AttributeExtendTupleExpr "status2" (FunctionAtomExpr "max" [AttributeAtomExpr "sub"] ())) (Group (AttributeNames (fromList ["status"])) "sub" (Project (AttributeNames (fromList ["city","status"])) (RelationVariable "s" ())))))
-}

-- (s group ({all but city} as sub): {maxstatus:=max(@sub{status})}){city,maxstatus}
-- select city,max(status) from s group by city;

convertGroupBy :: TypeForRelExprF -> [GroupByExpr] -> Maybe HavingExpr -> [SelectItem] -> ConvertM GroupByInfo
convertGroupBy _typeF groupBys mHavingExpr sqlProjection = do
  --first, check that projection includes an aggregate, otherwise, there's no point
  --find aggregate functions at the top-level (including within other functions such as 1+max(x)), and refocus them on the group attribute projected on the aggregate target
  -- do we need an operator to apply a relexpr to a subrelation? For example, it would be useful to apply a projection across all the subrelations, and types are maintained
--  foldM convertGroupByExpr emptyGroupByInfo sqlProjection
  -- each scalar expr must appear at the top-level SelectItem list
--    convertGroupByExpr acc
  -- search group by exprs to find the matching sexpr- if more than one matches, error
  --todo: handle asterisk
  let findMatchingProjection expr@(GroupByExpr gbexpr) =
        let exprMatcher (projExpr, _alias) acc =
              if containsProjScalarExpr gbexpr projExpr then
                projExpr : acc
              else
                acc
        in
          case foldr exprMatcher mempty sqlProjection of
            [] -> throwSQLE (AggregateGroupByMismatchError gbexpr)
            [match] -> if containsAggregate match then
                         pure (AggGroupByItem match expr)
                       else
                         pure (NonAggGroupByItem match expr)
            _matches -> throwSQLE (AggregateGroupByMismatchError gbexpr)
      collectGroupByInfo info gbsexpr = do
        -- validate that there is a corresponding group by
        matchExpr <- findMatchingProjection gbsexpr
        case matchExpr of
          AggGroupByItem pe _gb -> 
            pure $ info { aggregates = pe : aggregates info }
          NonAggGroupByItem (Identifier colName) gb -> do
            aname <- convertColumnProjectionName colName
            pure $ info { nonAggregates = (aname, gb) : nonAggregates info }
          NonAggGroupByItem pe _ -> do
            throwSQLE (UnsupportedGroupByProjectionError pe)
      -- find select items which are not mentioned in the group by expression and make sure that are in the aggregates info
--      collectNonGroupByInfo :: [ProjectionScalarExpr] -> GroupByInfo -> SelectItem ->  ConvertM GroupByInfo
      collectNonGroupByInfo info (projExpr, _alias) = 
       if containsAggregate projExpr then
         pure (info { aggregates = projExpr : aggregates info })
       else
         pure info
         
  groups1 <- foldM collectGroupByInfo emptyGroupByInfo groupBys
  groups2 <- foldM collectNonGroupByInfo groups1 sqlProjection
  let groups3 = case mHavingExpr of
                  Just (HavingExpr sexpr) -> groups2 { havingRestriction = Just sexpr }
                  Nothing -> groups2
  -- perform some validation
{-  let sqlProj = HS.fromList (map fst sqlProjection)
      groupByProj = HS.fromList (aggregates groups2 <> map fst (nonAggregates groups2))
      diff = HS.difference sqlProj groupByProj
  if HS.null diff then-}
  pure groups3
{-    else
    throwSQLE (GroupByColumnNotReferencedInGroupByError (HS.toList diff))-}
    


data GroupByItem = AggGroupByItem ProjectionScalarExpr GroupByExpr |
                   NonAggGroupByItem ProjectionScalarExpr GroupByExpr
 deriving (Show, Eq)

-- | Validated "group by" and "having" data
data GroupByInfo =
  GroupByInfo { aggregates :: [ProjectionScalarExpr], -- ^ mentioned in group by clause and uses aggregation
                nonAggregates :: [(AttributeName, GroupByExpr)], -- ^ mentioned in group by clause by not aggregations
                havingRestriction :: Maybe ProjectionScalarExpr                
              }
  deriving (Show, Eq)

emptyGroupByInfo :: GroupByInfo
emptyGroupByInfo = GroupByInfo { aggregates = [], nonAggregates = [], havingRestriction = Nothing }

aggregateFunctions :: [(FuncName, FunctionName)]
aggregateFunctions = [(FuncName ["max"], "sql_max"),
                       (FuncName ["min"], "sql_min"),
                       (FuncName ["sum"], "sql_sum"),
                       (FuncName ["count"], "sql_count")]

isAggregateFunction :: FuncName -> Bool
isAggregateFunction fname = fname `elem` map fst aggregateFunctions

containsAggregate :: ProjectionScalarExpr -> Bool
containsAggregate expr =
  case expr of
    IntegerLiteral{} -> False
    DoubleLiteral{} -> False
    StringLiteral{} -> False
    BooleanLiteral{} -> False
    NullLiteral -> False
    Identifier{} -> False
    BinaryOperator e1 op e2 -> containsAggregate e1 || containsAggregate e2 || opAgg op
    PrefixOperator op e1 -> containsAggregate e1 || opAgg op
    PostfixOperator e1 op -> containsAggregate e1 || opAgg op
    BetweenOperator e1 e2 e3 -> containsAggregate e1 || containsAggregate e2 || containsAggregate e3
    FunctionApplication fname args -> isAggregateFunction fname || any containsAggregate args
    c@CaseExpr{} -> or (cElse : concatMap (\(when', res) -> [containsAggregate res, containsAggregate when']) (caseWhens c))
      where
        cElse = maybe False containsAggregate (caseElse c)
    q@QuantifiedComparison{} -> containsAggregate (qcExpr q)
    InExpr _ e1 _ -> containsAggregate e1
    BooleanOperatorExpr e1 opName e2 -> opAgg opName || containsAggregate e1 || containsAggregate e2
    ExistsExpr{} -> False
  where
    opAgg _opName = False

-- | Returns True iff a projection scalar expr within a larger expression. Used for group by aggregation validation.
containsProjScalarExpr :: ProjectionScalarExpr -> ProjectionScalarExpr -> Bool
containsProjScalarExpr needle haystack =
  (needle == haystack) ||
    case haystack of
      IntegerLiteral{} -> False
      DoubleLiteral{} -> False
      StringLiteral{} -> False
      BooleanLiteral{} -> False
      NullLiteral -> False
      Identifier{} -> False
      BinaryOperator e1 _op e2 -> con e1 || con e2
      PrefixOperator _op e1 -> con e1
      PostfixOperator e1 _op -> con e1
      BetweenOperator e1 e2 e3 -> con e1 || con e2 || con e3
      FunctionApplication _fname args -> any con args
      c@CaseExpr{} -> or (cElse : concatMap (\(when', res) -> [con res, con when']) (caseWhens c))
        where
          cElse = maybe False con (caseElse c)
      q@QuantifiedComparison{} -> con (qcExpr q)
      InExpr _ e1 _ -> containsAggregate e1
      BooleanOperatorExpr e1 _opName e2 -> con e1 || con e2
      ExistsExpr{} -> False
   where
     con = containsProjScalarExpr needle

-- depth first replacement for scalar expr modification
replaceProjScalarExpr :: (ProjectionScalarExpr -> ProjectionScalarExpr) -> ProjectionScalarExpr -> ProjectionScalarExpr
replaceProjScalarExpr r orig =
  case orig of
    IntegerLiteral{} -> r orig
    DoubleLiteral{} -> r orig
    StringLiteral{} -> r orig
    BooleanLiteral{} -> r orig
    NullLiteral{} -> r orig
    Identifier{} -> r orig
    BinaryOperator e1 op e2 -> r (BinaryOperator (recr e1) op (recr e2))
    PrefixOperator op e1 -> r (PrefixOperator op (recr e1))
    PostfixOperator e1 op -> r (PostfixOperator (recr e1) op)
    BetweenOperator e1 e2 e3 -> r (BetweenOperator (recr e1) (recr e2) (recr e3))
    FunctionApplication fname args -> r (FunctionApplication fname (map recr args))
    c@CaseExpr{} -> r (CaseExpr { caseWhens = map (bimap recr recr) (caseWhens c),
                                  caseElse = recr <$> caseElse c
                                })
    c@QuantifiedComparison{} -> r (c{ qcExpr = recr (qcExpr c) })
    InExpr flag e1 predval -> r (InExpr flag (recr e1) predval)
    BooleanOperatorExpr e1 op e2 -> r (BooleanOperatorExpr (recr e1) op (recr e2))
    e@ExistsExpr{} -> e
  where
    recr = replaceProjScalarExpr r

-- find SQL aggregate functions and replace then with folds on attribute "_sql_aggregate"
processSQLAggregateFunctions :: AtomExpr -> AtomExpr
processSQLAggregateFunctions expr = 
  case expr of
    AttributeAtomExpr{} -> expr
    NakedAtomExpr{} -> expr
    FunctionAtomExpr fname [AttributeAtomExpr attrName] ()
      | fname == "sql_count" && -- count(*) counts the number of rows
        attrName == "_sql_aggregate" -> expr
      | fname == "sql_count" -> -- count(city) counts the number city elements that are not null
        callF fname [RelationAtomExpr
                     (Restrict
                       (NotPredicate
                        (AtomExprPredicate
                         (callF "sql_isnull" [AttributeAtomExpr attrName]))) (RelationValuedAttribute "_sql_aggregate"))]
      | fname `elem` map snd aggregateFunctions ->
          FunctionAtomExpr fname
            [RelationAtomExpr (Project (AttributeNames (S.singleton attrName)) (RelationValuedAttribute "_sql_aggregate"))] ()
    FunctionAtomExpr fname args () -> FunctionAtomExpr fname (map processSQLAggregateFunctions args) ()
    RelationAtomExpr{} -> expr --not supported in SQL
    IfThenAtomExpr ifE thenE elseE -> IfThenAtomExpr (processSQLAggregateFunctions ifE) (processSQLAggregateFunctions thenE) (processSQLAggregateFunctions elseE)
    ConstructedAtomExpr{} -> expr --not supported in SQL
  where
    callF fname args = FunctionAtomExpr fname args ()

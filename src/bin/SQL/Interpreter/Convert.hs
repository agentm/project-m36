--convert SQL into relational or database context expressions
{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables, TypeApplications, GeneralizedNewtypeDeriving #-}
module SQL.Interpreter.Convert where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.DataFrame (DataFrameExpr(..), AttributeOrderExpr(..), AttributeOrder(..),Order(..), usesDataFrameFeatures)
import ProjectM36.AttributeNames as A
import ProjectM36.Attribute as A
import qualified ProjectM36.WithNameExpr as W
import SQL.Interpreter.Select
import Data.Kind (Type)
import qualified Data.Text as T
import ProjectM36.Relation
import Control.Monad (foldM)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (foldl', intercalate)
import qualified Data.Functor.Foldable as Fold
import qualified Data.List.NonEmpty as NE
import Control.Monad (when)
import ProjectM36.DataTypes.Maybe
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.State (StateT, get, put, runStateT, evalStateT)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans.Class (lift)

import Debug.Trace

data SQLError = NotSupportedError T.Text |
                TypeMismatchError AtomType AtomType |
                NoSuchSQLFunctionError FuncName |
                DuplicateTableReferenceError TableAlias |
                MissingTableReferenceError TableAlias |
                UnexpectedTableNameError TableName |
                UnexpectedColumnNameError ColumnName |
                ColumnResolutionError ColumnName |
                ColumnAliasResolutionError ColumnAlias |
                UnexpectedRelationalExprError RelationalExpr |
                UnexpectedAsteriskError ColumnProjectionName |
                AmbiguousColumnResolutionError ColumnName |
                DuplicateColumnAliasError ColumnAlias |
                SQLRelationalError RelationalError
  deriving (Show, Eq)

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
                                          
--over the course of conversion, we collect all the table aliases we encounter, including non-aliased table references, including the type of the table
newtype TableContext = TableContext (M.Map TableAlias (RelationalExpr, Attributes, ColumnAliasMap))
  deriving (Semigroup, Monoid, Show, Eq)

-- debugging utility function
prettyTableContext :: TableContext -> String
prettyTableContext (TableContext tMap) = "TableContext {\n" <> concatMap prettyKV (M.toList tMap) <> "}"
  where
    prettyKV (TableAlias k,(rvexpr, attrs, colAliasMap)) =
      T.unpack k <> "::\n" <>
      --prettyRv <>
      --prettyAttrs <>
      prettyColAliasMap colAliasMap <> "\n"
    prettyColAliasMap cAMap = intercalate ", " $ map (\(ColumnAlias al, attrName') -> T.unpack al <> ":" <> T.unpack attrName') (M.toList cAMap)
  
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
    mapper :: (TableAlias, (RelationalExpr, Attributes, ColumnAliasMap)) -> ConvertM (WithNameExpr, RelationalExpr)
    mapper (TableAlias nam, (rvExpr, _, _)) = pure (WithNameExpr nam (), rvExpr)
    mapper (qn, _) = throwSQLE (NotSupportedError ("schema qualified table names: " <> T.pack (show qn)))

throwSQLE :: SQLError -> ConvertM a
throwSQLE = lift . throwE

-- | Pass state down to subselect, but discard any state changes from the subselect processing.
withSubSelect :: ConvertM a -> ConvertM (a, TableContext)
withSubSelect m = do
  state@(TableContext orig) <- get
  ret <- m
  state'@(TableContext postSub) <- get
  put state
  -- diff the state to get just the items that were added
  traceShowM ("diff orig", M.keys orig)
  traceShowM ("diff postSub", M.keys postSub)
  traceShowM ("diff1", M.difference postSub orig)
  let diff = M.differenceWith tctxDiff postSub orig
      tctxDiff (rexprA, attrsA, colAliasMapA) (_, _, colAliasMapB) =
        Just (rexprA, attrsA, M.difference colAliasMapB colAliasMapA)
  pure (ret, TableContext diff)

-- | Insert another table into the TableContext.
insertTable :: TableAlias -> RelationalExpr -> Attributes -> ConvertM ()
insertTable tAlias expr rtype = do
  (TableContext map') <- get
  case M.lookup tAlias map' of
    Nothing -> put $ TableContext $ M.insert tAlias (expr, rtype, mempty) map'
    Just _ -> throwSQLE (DuplicateTableReferenceError tAlias)

-- | When a column is mentioned, it may need to be aliased. The table name must already be in the table context so that we can identify that the attribute exists. Without a table name, we must look for a uniquely named column amongst all tables.
insertColumn :: Maybe TableAlias -> ColumnName -> Maybe ColumnAlias -> ConvertM ColumnAlias
insertColumn mTblAlias colName mColAlias = do
  tcontext@(TableContext tmap) <- get
  -- find the relevant table for the key to the right table
  traceShowM ("insertColumn", colName)
  tblAlias' <- case mTblAlias of
                        Just tblAlias -> do
                          void $ lookupTable tblAlias
                          pure tblAlias
                        Nothing ->do
                          -- scan column names for match- if there are multiple matches, return a column ambiguity error
                          traceShowM ("insertColumn", colName)
                          ret <- findOneColumn colName
--                          traceShowM ("insertColumn2", colName)
                          pure ret
  -- insert into the column alias map
  let newAlias = case mColAlias of
                   Nothing -> case colName of
                                ColumnName [c] -> ColumnAlias c
                                ColumnName [t,c] -> ColumnAlias (t <> "." <> c)
                   Just al -> al
      origColName = case colName of
                      ColumnName [c] -> c
                      ColumnName [_,c] -> c
                    
{-  when (newAlias `elem` allColumnAliases tcontext) $ do
    traceShowM ("gonk error",
                "colName", colName,
                "mTblAlias", mTblAlias,
                "mColAlias", mColAlias,
                tmap)
    throwSQLE (DuplicateColumnAliasError newAlias)-} --duplicate column aliases are OK
  --verify that the alias is not duplicated                  
  let tmap' = M.adjust insertCol tblAlias' tmap
      insertCol (rvexpr, attrs, colMap) =
        (rvexpr, attrs, M.insert newAlias origColName colMap)
  put (TableContext tmap')
  pure newAlias

-- | Add a column alias for a column which has already been inserted into the TableContext.
addColumnAlias' :: TableContext -> TableAlias -> ColumnAlias -> AttributeName -> Either SQLError TableContext
addColumnAlias' (TableContext tctx) tAlias colAlias@(ColumnAlias colText) attr = do
  case M.lookup tAlias tctx of
    Nothing -> Left (ColumnAliasResolutionError colAlias)
    Just (rvexpr, attrs, colMap) ->
      --check that the attribute is present in attributes, then plop it into the colMap and return the updated TableContext
      if attr `A.isAttributeNameContained` attrs then do
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
  
lookupTable :: TableAlias -> ConvertM (RelationalExpr, Attributes, ColumnAliasMap)
lookupTable ta = do
  (TableContext map') <- get
  case M.lookup ta map' of
    Nothing -> throwSQLE (MissingTableReferenceError ta)
    Just res -> pure res

-- | Merge table contexts (used in subselects)
mergeContext :: TableContext -> ConvertM ()
mergeContext (TableContext ctxB) = do
  (TableContext tMapA) <- get
  foldM folder () (M.toList tMapA)
   where
    folder acc (tAlias, (re,attrs, _)) = insertTable tAlias re attrs

-- | Find a column name or column alias in the underlying table context. Returns key into table context.
findColumn :: ColumnName -> ConvertM [TableAlias]
findColumn targetCol = do
  tcontext <- get
  pure (findColumn' targetCol tcontext)
  
-- | non ConvertM version of findColumn
findColumn' :: ColumnName -> TableContext -> [TableAlias]
findColumn' targetCol (TableContext tMap) = do
  traceShowM ("findColumn'", targetCol, tMap)
  M.foldrWithKey folder [] tMap
   where
    folder tAlias@(TableAlias tat) (rvExpr, rtype, _) acc =
      case targetCol of
        ColumnName [colName'] ->
          if S.member colName' (attributeNameSet rtype) then
            tAlias : acc
            else
            acc
        ColumnName [tPrefix, colName'] ->
          if tat == tPrefix && S.member colName' (attributeNameSet rtype) then
            tAlias : acc
            else
            acc
        _ -> acc

--findColumnAlias' :: ColumnAlias        


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
      traceShow ("findOneColumn'", targetCol) $ Left (ColumnResolutionError targetCol)
    [match] -> pure match
    _matches -> Left (AmbiguousColumnResolutionError targetCol)

-- | Search the TableContext for a column alias remapping for the given column name.
attributeNameForColumnName' :: ColumnName -> TableContext -> Either SQLError AttributeName
attributeNameForColumnName' colName tcontext@(TableContext tmap) = do
  traceShowM ("attributeNameForColumnName'", colName)
--  traceShowM ("attribtueNameForColumnName tmap", tmap)  
  tKey@(TableAlias tAlias) <- findOneColumn' colName tcontext
  let (_, rvattrs, colAliases) = tmap M.! tKey
  --strip table prefix, if necessary
  colAlias@(ColumnAlias colAttr) <- case colName of
                ColumnName [attr] -> pure $ ColumnAlias attr
                ColumnName [tname,attr] -> pure $ ColumnAlias (tname <> "." <> attr)
                ColumnName{} -> traceShow ("attrname", colName) $ Left $ ColumnResolutionError colName
  case M.lookup colAlias colAliases of
    Just _ -> pure (unColumnAlias colAlias) -- we found it, so it's valid
    Nothing ->
      -- look in rvattrs, so we don't need the table alias prefix. The lack of an entry in the column alias map indicates that the column was not renamed in the join condition.
      if colAttr `A.isAttributeNameContained` rvattrs then
        pure colAttr
        --pure (T.concat [tAlias, ".", colAttr])
      else
        case colName of
          ColumnName [_, col] | col `A.isAttributeNameContained` rvattrs ->
                                -- the column has not been aliased, so we presume it can be use the column name directly
                                pure col
          _ -> traceShow ("attrNameForColName") $ Left $ ColumnResolutionError colName

attributeNameForColumnName :: ColumnName -> ConvertM AttributeName
attributeNameForColumnName colName = do
  s <- get
  traceShowM ("attributeNameForColumnName", colName)
  traceStateM
  case attributeNameForColumnName' colName s of
    Left err -> throwSQLE err
    Right al -> pure al
  

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
baseDFExpr = DataFrameExpr { convertExpr = MakeRelationFromExprs (Just []) (TupleExprs () []),
                             orderExprs = [],
                             offset = Nothing,
                             limit = Nothing }
             
convertSelect :: TypeForRelExprF -> Select -> ConvertM DataFrameExpr
convertSelect typeF sel = do
  -- extract all mentioned tables into the table alias map for 
  (dfExpr, colRemap) <- case tableExpr sel of
              Nothing -> pure (baseDFExpr, mempty)
              Just tExpr -> convertTableExpr typeF tExpr
--  traceShowM ("table aliases", tAliasMap)              
  explicitWithF <- case withClause sel of
                     Nothing -> pure id
                     Just wClause -> do
                       wExprs <- convertWithClause typeF wClause
                       pure (With wExprs)
              
  -- convert projection using table alias map to resolve column names
  projF <- convertProjection typeF (projectionClause sel)
  -- add with clauses
  withAssocs <- tableAliasesAsWithNameAssocs
  let withF = case withAssocs of
                [] -> id
                _ -> With withAssocs
  -- if we have only one table alias or the columns are all unambiguous, remove table aliasing of attributes
  s <- get
  traceStateM
  pure (dfExpr { convertExpr = explicitWithF (withF (projF (convertExpr dfExpr))) })            

-- | Slightly different processing for subselects.
convertSubSelect :: TypeForRelExprF -> Select -> ConvertM RelationalExpr
convertSubSelect typeF sel = do
  (ret, TableContext aliasDiff) <- withSubSelect $ do
    (dfExpr, colRemap) <- case tableExpr sel of
                            Nothing -> pure (baseDFExpr, mempty)
                            Just tExpr -> convertTableExpr typeF tExpr  
    when (usesDataFrameFeatures dfExpr) $ throwSQLE (NotSupportedError "ORDER BY/LIMIT/OFFSET in subquery")
    traceShowM ("convertSubSelect", colRemap)
    explicitWithF <- case withClause sel of
                       Nothing -> pure id
                       Just wClause -> do
                         wExprs <- convertWithClause typeF wClause
                         pure (With wExprs)
    -- convert projection using table alias map to resolve column names
    projF <- convertProjection typeF (projectionClause sel) -- the projection can only project on attributes from the subselect table expression
    -- add with clauses
    withAssocs <- tableAliasesAsWithNameAssocs
    let withF = case withAssocs of
                  [] -> id
                  _ -> With withAssocs
    -- add disambiguation renaming
--       tableColumns = foldr ((\(tname,(_,_,colAliases)) acc -> acc <> map () (M.) [] (M.toList aliasDiff)
--       renamesSet <- foldM (\acc 
    pure (explicitWithF (withF (projF (convertExpr dfExpr))))
  traceShowM ("diff", aliasDiff) -- alias is not correct- the col alias map is empty for subquery
  pure ret

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
    (Identifier qpn@(ColumnProjectionName [ProjectionName col]), Nothing) -> do
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
          attrName' Nothing c = "attr_" <> T.pack (show c)
      atomExpr <- convertProjectionScalarExpr typeF scalarExpr
      let newAttrName = attrName' mAlias c
      -- we need to apply the projections after the extension!
      pure $ acc { taskExtenders = AttributeExtendTupleExpr newAttrName atomExpr : taskExtenders acc,
                       taskProjections = S.insert (ColumnProjectionName [ProjectionName newAttrName]) (taskProjections acc)
                     }
  where
   colinfo (ColumnProjectionName [ProjectionName name]) = do
     findOneColumn (traceShow ("colinfo", name) (ColumnName [name]))

convertProjection :: TypeForRelExprF -> [SelectItem] -> ConvertM (RelationalExpr -> RelationalExpr)
convertProjection typeF selItems = do
--    traceShowM ("convertProjection", selItems)
    let emptyTask = SelectItemsConvertTask { taskProjections = S.empty,
                                             taskRenames = mempty,
                                             taskExtenders = mempty }
        attrName' (Just (ColumnAlias nam)) _ = nam
        attrName' Nothing c = "attr_" <> T.pack (show c)
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

{-
convertColumnProjectionName :: ColumnProjectionName -> Either SQLError AttributeName
convertColumnProjectionName (ColumnProjectionName names) = do
    let namer (ProjectionName t) = pure t
        namer Asterisk = Left (NotSupportedError "asterisk in projection conversion")
    names' <- mapM namer names
    pure (T.concat names')
-}

convertUnqualifiedColumnName :: UnqualifiedColumnName -> AttributeName
convertUnqualifiedColumnName (UnqualifiedColumnName nam) = nam

convertColumnName :: ColumnName -> ConvertM AttributeName
convertColumnName colName = do
  traceShowM ("convertColumnName", colName)
  attributeNameForColumnName colName

convertColumnProjectionName :: ColumnProjectionName -> ConvertM AttributeName
convertColumnProjectionName qpn@(ColumnProjectionName names) = do
  let namer (ProjectionName t) = pure t
      namer Asterisk = throwSQLE $ UnexpectedAsteriskError qpn
  names' <- mapM namer names
  convertColumnName (ColumnName names')
                      
        
convertTableExpr :: TypeForRelExprF -> TableExpr -> ConvertM (DataFrameExpr, ColumnRemap)
convertTableExpr typeF tExpr = do
    (fromExpr, columnRemap) <- convertFromClause typeF (fromClause tExpr)
{-    let tableAliasMap' = M.filterWithKey filterRedundantAlias tableAliasMap
        filterRedundantAlias (QualifiedName [nam]) (RelationVariable nam' ())
          | nam == nam' = False
        filterRedundantAlias _ _ = True-}
{-    withExprs <- mapM (\(qnam, expr) -> do
                          nam <- convertQualifiedName qnam
                          pure (WithNameExpr nam (), expr)) (M.toList tableAliasMap')-}

      
    expr' <- case whereClause tExpr of
      Just whereExpr -> do
        restrictPredExpr <- convertWhereClause typeF whereExpr
        pure $ Restrict restrictPredExpr fromExpr        
      Nothing -> pure fromExpr
    orderExprs <- convertOrderByClause typeF (orderByClause tExpr)
    let dfExpr = DataFrameExpr { convertExpr = expr',
                                 orderExprs = orderExprs,
                                 offset = offsetClause tExpr,
                                 limit = limitClause tExpr }
    pure (dfExpr, columnRemap)

convertWhereClause :: TypeForRelExprF -> RestrictionExpr -> ConvertM RestrictionPredicateExpr
convertWhereClause typeF (RestrictionExpr rexpr) = do
    let wrongType t = throwSQLE $ TypeMismatchError t BoolAtomType --must be boolean expression
        attrName' (ColumnName ts) = T.intercalate "." ts
    case rexpr of
      IntegerLiteral{} -> wrongType IntegerAtomType
      DoubleLiteral{} -> wrongType DoubleAtomType
      StringLiteral{} -> wrongType TextAtomType
      Identifier i -> wrongType TextAtomType -- could be a better error here
      BinaryOperator i@(Identifier colName) (OperatorName ["="]) exprMatch -> do --we don't know here if this results in a boolean expression, so we pass it down
        traceShowM ("= bin", colName)        
        attrName <- attributeNameForColumnName colName
        AttributeEqualityPredicate attrName <$> convertScalarExpr typeF exprMatch
      BinaryOperator exprA op exprB -> do
        a <- convertScalarExpr typeF exprA
        b <- convertScalarExpr typeF exprB
        f <- lookupOperator op
        pure (AtomExprPredicate (f [a,b]))
      InExpr inOrNotIn sexpr (InList matches') -> do
        eqExpr <- convertScalarExpr typeF sexpr
        let (match:matches) = reverse matches'
        firstItem <- convertScalarExpr typeF match
        let inFunc a b = AtomExprPredicate (FunctionAtomExpr "eq" [a,b] ())
            predExpr' = inFunc eqExpr firstItem
            folder predExpr'' sexprItem = do
              item <- convertScalarExpr typeF sexprItem
              pure $ OrPredicate (inFunc eqExpr item) predExpr''
        res <- foldM folder predExpr' matches --be careful here once we introduce NULLs
        case inOrNotIn of
          In -> pure res
          NotIn -> pure (NotPredicate res)
      ExistsExpr subQ -> do
        relExpr  <- convertSubSelect typeF subQ
        --pretty sure I have to rename attributes in both the top-level query and in this one to prevent attribute conflicts- we can't rename all the attributes in the subquery, because the renamer won't know which attributes actually refer to the top-level attributes- should we just prefix all attributes unconditionally or send a signal upstream to rename attributes?
        let rexpr = Project A.empty relExpr
        pure (RelationalExprPredicate rexpr)


convertScalarExpr :: TypeForRelExprF -> ScalarExpr -> ConvertM AtomExpr
convertScalarExpr typeF expr = do
    let naked = pure . NakedAtomExpr
    case expr of
      IntegerLiteral i -> naked (IntegerAtom i)
      DoubleLiteral d -> naked (DoubleAtom d)
      StringLiteral s -> naked (TextAtom s)
      -- we don't have enough type context with a cast, so we default to text
      NullLiteral -> naked (ConstructedAtom "Nothing" (maybeAtomType TextAtomType) [])
      Identifier i -> do
        traceShowM ("convertScalarExpr", i)
        AttributeAtomExpr <$> convertColumnName i
      BinaryOperator exprA op exprB -> do
        a <- convertScalarExpr typeF exprA
        b <- convertScalarExpr typeF exprB
        f <- lookupOperator op
        pure $ f [a,b]

convertProjectionScalarExpr :: TypeForRelExprF -> ProjectionScalarExpr -> ConvertM AtomExpr
convertProjectionScalarExpr typeF expr = do
    let naked = pure . NakedAtomExpr
    case expr of
      IntegerLiteral i -> naked (IntegerAtom i)
      DoubleLiteral d -> naked (DoubleAtom d)
      StringLiteral s -> naked (TextAtom s)
      NullLiteral -> naked (ConstructedAtom "Nothing" (maybeAtomType TextAtomType) [])      
      Identifier i ->
        AttributeAtomExpr <$> convertColumnProjectionName i
      BinaryOperator exprA op exprB -> do
        a <- convertProjectionScalarExpr typeF exprA
        b <- convertProjectionScalarExpr typeF exprB
        f <- lookupOperator op
        pure $ f [a,b]

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
convertWithClause = undefined

type ColumnRemap = M.Map ColumnName ColumnName

convertFromClause :: TypeForRelExprF -> [TableRef] -> ConvertM (RelationalExpr, ColumnRemap)
convertFromClause typeF (firstRef:trefs) = do
    --the first table ref must be a straight RelationVariable
  let convertFirstTableRef (SimpleTableRef (TableName [nam])) = do
        let rv = RelationVariable nam ()
        typeR <- wrapTypeF typeF rv
        insertTable (TableAlias nam) rv (attributes typeR)
        pure rv
      convertFirstTableRef (AliasedTableRef (SimpleTableRef (TableName [nam])) al@(TableAlias alias)) = do
        let rv = RelationVariable nam ()
        typeR <- wrapTypeF typeF rv
        insertTable al rv (attributes typeR)
        pure (RelationVariable alias ())
  firstRel <- convertFirstTableRef firstRef
  expr' <- foldM (joinTableRef typeF) firstRel (zip [1..] trefs)
  pure (expr', mempty {- FIXME add column remapping-})

-- | Convert TableRefs after the first one (assumes all additional TableRefs are for joins). Returns the qualified name key that was added to the map, the underlying relexpr (not aliased so that it can used for extracting type information), and the new table context map
convertTableRef :: TypeForRelExprF -> TableRef -> ConvertM (TableAlias, RelationalExpr)
convertTableRef typeF tref =
  case tref of
    SimpleTableRef qn@(TableName [nam]) -> do
      let rv = RelationVariable nam ()
          ta = TableAlias nam
      typeRel <- wrapTypeF typeF rv 
      tContext' <- insertTable ta rv (attributes typeRel)
      pure (ta, rv) -- include with clause even for simple cases because we use this mapping to 
    AliasedTableRef (SimpleTableRef qn@(TableName [nam])) tAlias -> do
      typeRel <- wrapTypeF typeF (RelationVariable nam ())
      let rv = RelationVariable nam ()
      tContext' <- insertTable tAlias rv (attributes typeRel)
      pure $ (tAlias, RelationVariable nam ())
    x -> throwSQLE $ NotSupportedError (T.pack (show x))

  
joinTableRef :: TypeForRelExprF -> RelationalExpr -> (Int, TableRef) -> ConvertM RelationalExpr
joinTableRef typeF rvA (c,tref) = do
      -- optionally prefix attributes unelss the expr is a RelationVariable
  let attrRenamer x expr attrs = do
        renamed <- mapM (renameOneAttr x expr) attrs
        pure (Rename (S.fromList renamed) expr)
      -- prefix all attributes
      prefixRenamer tAlias@(TableAlias prefix) expr attrs = do
        renamed <- mapM (prefixOneAttr tAlias) attrs
        pure (Rename (S.fromList renamed) expr)
      prefixOneAttr tAlias@(TableAlias prefix) old_name = do
        -- insert into columnAliasMap
        let new_name = T.concat [prefix, ".", old_name]
        traceShowM ("prefixOneAttr", tAlias, old_name, new_name)
        addColumnAlias tAlias (ColumnAlias new_name) old_name
        pure (old_name, new_name)
      renameOneAttr x expr old_name = do
        traceShowM ("renameOneAttr", old_name, new_name)
        addColumnAlias (TableAlias prefix) (ColumnAlias new_name) old_name
        pure (old_name, new_name)
        where
          new_name = T.concat [prefix, ".", old_name]
          prefix = case expr of
            RelationVariable rvName () -> rvName
            _ -> x -- probably need to return errors for some expressions
      crossJoin jtref = do
            --rename all columns to prefix them with a generated alias to prevent any natural join occurring, then perform normal join
            -- we need the type to get all the attribute names for both relexprs
            (tKey, rvB) <- convertTableRef typeF jtref
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
            (attrsIntersection, attrsA, attrsB) <- commonAttributeNames typeF rvA rvB
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
            (commonAttrs, attrsA, attrsB) <- commonAttributeNames typeF (withExpr rvA) (withExpr rvB)
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
            traceShowM ("exprA", exprA)
            traceShowM ("exprB", exprB)
            -- for the join condition, we can potentially extend to include all the join criteria columns, then project them away after constructing the join condition
            tcontext <- get
{-            let joinExpr' = renameIdentifier renamer joinExpr
                renamer colName =
                  case attributeNameForColumnName' (traceShow ("inner join", colName) colName) tcontext of
                    Left err -> error (show err)
                    Right attrName -> (ColumnName [attrName])
            traceShowM ("joinExpr'", joinExpr')-}
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
                joinName = firstAvailableName 1 allAttrs
                extender = AttributeExtendTupleExpr joinName joinRe
                joinMatchRestriction = Restrict (AttributeEqualityPredicate joinName (ConstructedAtomExpr "True" [] ()))
                projectAwayJoinMatch = Project (InvertedAttributeNames (S.fromList [joinName]))
            pure (projectAwayJoinMatch (joinMatchRestriction (Extend extender (Join exprB exprA))))

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
  where
    f n args = FunctionAtomExpr n args ()
    sqlFuncs = [(">",f "gt"),
                 ("<",f "lt"),
                 (">=",f "gte"),
                 ("<=",f "lte"),
                 ("=",f "eq"),
                 ("!=",f "not_eq"), -- function missing
                 ("<>",f "not_eq"), -- function missing
                 ("+", f "add"),
                 ("and", f "and"),
                 ("or", f "or")
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

-- | If the restriction includes a EXISTS expression, we must rename all attributes at the top-level to prevent conflicts.
needsToRenameAllAttributes :: RestrictionExpr -> Bool
needsToRenameAllAttributes (RestrictionExpr sexpr) =
  rec' sexpr
  where
  rec' sexpr' =
    case sexpr' of
      DoubleLiteral{} -> False
      StringLiteral{} -> False
      NullLiteral{} -> False
      Identifier{} -> False
      BinaryOperator e1 _ e2 -> rec' e1 || rec' e2
      PrefixOperator _ e1 -> rec' e1
      PostfixOperator e1 _ -> rec' e1
      BetweenOperator e1 _ e2 -> rec' e1 || rec' e2
      FunctionApplication _ e1 -> rec' e1
      CaseExpr cases else' -> or (map (\(whens, then') ->
                                          or (map rec' whens) || rec' then') cases)
      qc@QuantifiedComparison{} -> True
      InExpr _ sexpr _ -> rec' sexpr
      BooleanOperatorExpr e1 _ e2 -> rec' e1 || rec' e2
      ExistsExpr{} -> True
  

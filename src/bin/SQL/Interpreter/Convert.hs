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
import Data.Text as T (pack,intercalate,Text,concat)
import ProjectM36.Relation
import Control.Monad (foldM)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (foldl')
import qualified Data.Functor.Foldable as Fold
import qualified Data.List.NonEmpty as NE
import Control.Monad (when)
import ProjectM36.DataTypes.Maybe
import Control.Monad (void)
import Data.Maybe (fromMaybe)

import Debug.Trace

data SQLError = NotSupportedError T.Text |
                TypeMismatchError AtomType AtomType |
                NoSuchSQLFunctionError FuncName |
                DuplicateTableReferenceError TableAlias |
                MissingTableReferenceError TableAlias |
                UnexpectedTableNameError TableName |
                UnexpectedColumnNameError ColumnName |
                ColumnResolutionError ColumnName |
                UnexpectedRelationalExprError RelationalExpr |
                UnexpectedAsteriskError ColumnProjectionName |
                AmbiguousColumnResolutionError ColumnName |
                DuplicateColumnAliasError ColumnAlias |
                SQLRelationalError RelationalError
  deriving (Show, Eq)

type TypeForRelExprF = RelationalExpr -> Either RelationalError Relation

--type ConvertM = State

data SelectItemsConvertTask = SelectItemsConvertTask { taskProjections :: S.Set ColumnProjectionName,
                                                       taskRenames :: [(ColumnProjectionName, ColumnAlias)],
                                                       taskExtenders :: [ExtendTupleExpr]
                                                     } deriving (Show, Eq)
                                          
--over the course of conversion, we collect all the table aliases we encounter, including non-aliased table references, including the type of the table
newtype TableContext = TableContext (M.Map TableAlias (RelationalExpr, Attributes, ColumnAliasMap))
  deriving (Semigroup, Monoid, Show, Eq)

-- key: alias value: real column attribute name
type ColumnAliasMap = M.Map ColumnAlias AttributeName

tableAliasesAsWithNameAssocs :: TableContext -> Either SQLError WithNamesAssocs
tableAliasesAsWithNameAssocs (TableContext tmap) =
  filter notSelfRef <$> mapM mapper (M.toList tmap)
  where
    notSelfRef (WithNameExpr nam (), RelationVariable nam' ()) | nam == nam' = False
                                                            | otherwise = True
    notSelfRef _ = True
--    mapper :: (QualifiedName, (RelationalExpr, Attributes, _)) -> Either SQLError (WithNameExpr, RelationalExpr)
    mapper (TableAlias nam, (rvExpr, _, _)) = pure (WithNameExpr nam (), rvExpr)
    mapper (qn, _) = Left (NotSupportedError ("schema qualified table names: " <> T.pack (show qn)))

-- | Insert another table into the TableContext.
insertTable :: TableAlias -> RelationalExpr -> Attributes -> TableContext -> Either SQLError TableContext
insertTable tAlias expr rtype (TableContext map') =
  case M.lookup tAlias map' of
    Nothing -> pure $ TableContext $ M.insert tAlias (expr, rtype, mempty) map'
    Just _ -> Left (DuplicateTableReferenceError tAlias)

-- | When a column is mentioned, it may need to be aliased. The table name must already be in the table context so that we can identify that the attribute exists. Without a table name, we must look for a uniquely named column amongst all tables.
insertColumn :: Maybe TableAlias -> ColumnName -> Maybe ColumnAlias -> TableContext -> Either SQLError (TableContext, ColumnAlias)
insertColumn mTblAlias colName mColAlias tcontext@(TableContext tmap) = do
  -- find the relevant table for the key to the right table
  tblAlias' <- case mTblAlias of
                        Just tblAlias -> do
                          void $ lookupTable tblAlias tcontext
                          pure tblAlias
                        Nothing ->
                          -- scan column names for match- if there are multiple matches, return a column ambiguity error
                          findOneColumn colName tcontext
  -- insert into the column alias map
  let newAlias = case mColAlias of
                   Nothing -> case colName of
                                ColumnName [c] -> ColumnAlias c
                                ColumnName [_,c] -> ColumnAlias c
                   Just al -> al
      origColName = case colName of
                      ColumnName [c] -> c
                      ColumnName [_,c] -> c
                   
  when (newAlias `elem` allColumnAliases tcontext) $ Left (DuplicateColumnAliasError newAlias)
  --verify that the alias is not duplicated                  
  let tcontext' = M.adjust insertCol tblAlias' tmap
      insertCol (rvexpr, attrs, colMap) =
        (rvexpr, attrs, M.insert newAlias origColName colMap)
  pure (TableContext tcontext', newAlias)


allColumnAliases :: TableContext -> [ColumnAlias]
allColumnAliases (TableContext tmap) = foldl' folder [] tmap
  where
    folder acc (_,_,colmap) = M.keys colmap <> acc
  
lookupTable :: TableAlias -> TableContext -> Either SQLError (RelationalExpr, Attributes, ColumnAliasMap)
lookupTable ta (TableContext map') =
  case M.lookup ta map' of
    Nothing -> Left (MissingTableReferenceError ta)
    Just res -> pure res

-- | Merge table contexts (used in subselects)
insertTables :: TableContext -> TableContext -> Either SQLError TableContext
insertTables (TableContext tMapA) ctxB =
  foldM folder ctxB (M.toList tMapA)
  where
    folder acc (qn, (re,attrs, _)) = insertTable qn re attrs acc
{-
replaceTableName :: QualifiedName -> QualifiedName -> TableContext -> Either SQLError TableContext
replaceTableName oldName newName (TableContext tctx) =
  case M.lookup oldName tctx of
    Nothing -> Left (MissingTableReferenceError oldName)
    Just match -> pure $ TableContext $ M.insert newName match (M.delete oldName tctx)
-}
-- | Find a column name or column alias in the underlying table context. Returns key into table context.
findColumn :: ColumnName -> TableContext -> [TableAlias]
findColumn targetCol (TableContext tMap) =
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

findOneColumn :: ColumnName -> TableContext -> Either SQLError TableAlias
findOneColumn colName tcontext = 
  case findColumn colName tcontext of
    [] -> Left (ColumnResolutionError colName)
    [match] -> pure match
    _matches -> Left (AmbiguousColumnResolutionError colName)

wrapTypeF :: TypeForRelExprF -> RelationalExpr -> Either SQLError Relation
wrapTypeF typeF relExpr =
  case typeF relExpr of
    Left relError -> Left (SQLRelationalError relError)
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
             
convertSelect :: TypeForRelExprF -> Select -> Either SQLError DataFrameExpr
convertSelect typeF sel = do
  -- extract all mentioned tables into the table alias map for 
  (dfExpr, tAliasMap, colRemap) <- case tableExpr sel of
              Nothing -> pure (baseDFExpr, mempty, mempty)
              Just tExpr -> convertTableExpr typeF tExpr
--  traceShowM ("table aliases", tAliasMap)              
  explicitWithF <- case withClause sel of
                     Nothing -> pure id
                     Just wClause -> do
                       wExprs <- convertWithClause typeF wClause
                       pure (With wExprs)
              
  -- convert projection using table alias map to resolve column names
  projF <- convertProjection typeF tAliasMap (projectionClause sel)
  -- add with clauses
  withAssocs <- tableAliasesAsWithNameAssocs tAliasMap
  let withF = case withAssocs of
                [] -> id
                _ -> With withAssocs
  -- if we have only one table alias or the columns are all unambiguous, remove table aliasing of attributes
  pure (dfExpr { convertExpr = explicitWithF (withF (projF (convertExpr dfExpr))) })            

-- | Slightly different processing for subselects.
convertSubSelect :: TypeForRelExprF -> TableContext -> Select -> Either SQLError (RelationalExpr, TableContext)
convertSubSelect typeF tctx sel = do
  (dfExpr, subTContext, colRemap) <- case tableExpr sel of
              Nothing -> pure (baseDFExpr, mempty, mempty)
              Just tExpr -> convertTableExpr typeF tExpr  
  when (usesDataFrameFeatures dfExpr) $ Left (NotSupportedError "ORDER BY/LIMIT/OFFSET in subquery")
  tableContext' <- insertTables tctx subTContext
  explicitWithF <- case withClause sel of
                     Nothing -> pure id
                     Just wClause -> do
                       wExprs <- convertWithClause typeF wClause
                       pure (With wExprs)
  -- convert projection using table alias map to resolve column names
  projF <- convertProjection typeF subTContext (projectionClause sel) -- the projection can only project on attributes from the subselect table expression
  -- add with clauses
  withAssocs <- tableAliasesAsWithNameAssocs subTContext
  let withF = case withAssocs of
                [] -> id
                _ -> With withAssocs
  -- if we have only one table alias or the columns are all unambiguous, remove table aliasing of attributes
  pure (explicitWithF (withF (projF (convertExpr dfExpr))), tableContext')
                       
  


convertSelectItem :: TypeForRelExprF -> TableContext -> SelectItemsConvertTask -> (Int,SelectItem) -> Either SQLError SelectItemsConvertTask
convertSelectItem typeF tableContext acc (c,selItem) =
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
      atomExpr <- convertProjectionScalarExpr typeF tableContext scalarExpr
      let newAttrName = attrName' mAlias c
      -- we need to apply the projections after the extension!
      pure $ acc { taskExtenders = AttributeExtendTupleExpr newAttrName atomExpr : taskExtenders acc,
                       taskProjections = S.insert (ColumnProjectionName [ProjectionName newAttrName]) (taskProjections acc)
                     }
  where
   colinfo (ColumnProjectionName [ProjectionName name]) =
     findOneColumn (ColumnName [name]) tableContext

convertProjection :: TypeForRelExprF -> TableContext -> [SelectItem] -> Either SQLError (RelationalExpr -> RelationalExpr)
convertProjection typeF tContext selItems = do
    let emptyTask = SelectItemsConvertTask { taskProjections = S.empty,
                                             taskRenames = mempty,
                                             taskExtenders = mempty }
        attrName' (Just (ColumnAlias nam)) _ = nam
        attrName' Nothing c = "attr_" <> T.pack (show c)
    task <- foldM (convertSelectItem typeF tContext) emptyTask (zip [1::Int ..] selItems)
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
                          oldName <- convertColumnProjectionName qProjName tContext
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

convertColumnName :: ColumnName -> TableContext -> Either SQLError AttributeName
convertColumnName colName tcontext = do
  --(_, <- insertColumn Nothing colName Nothing tcontext
  (TableAlias ts) <- findOneColumn colName tcontext -- wrong! why convert to a tablealias?
  pure ts

convertColumnProjectionName :: ColumnProjectionName -> TableContext -> Either SQLError AttributeName
convertColumnProjectionName qpn@(ColumnProjectionName names) tableContext = do
  let namer (ProjectionName t) = pure t
      namer Asterisk = Left $ UnexpectedAsteriskError qpn
  names' <- mapM namer names
  convertColumnName (ColumnName names') tableContext
                      
        
convertTableExpr :: TypeForRelExprF -> TableExpr -> Either SQLError (DataFrameExpr, TableContext, ColumnRemap)
convertTableExpr typeF tExpr = do
    (fromExpr, tContext, columnRemap) <- convertFromClause typeF (fromClause tExpr)
{-    let tableAliasMap' = M.filterWithKey filterRedundantAlias tableAliasMap
        filterRedundantAlias (QualifiedName [nam]) (RelationVariable nam' ())
          | nam == nam' = False
        filterRedundantAlias _ _ = True-}
{-    withExprs <- mapM (\(qnam, expr) -> do
                          nam <- convertQualifiedName qnam
                          pure (WithNameExpr nam (), expr)) (M.toList tableAliasMap')-}

      
    expr' <- case whereClause tExpr of
      Just whereExpr -> do
        restrictPredExpr <- convertWhereClause typeF tContext whereExpr
        pure $ Restrict restrictPredExpr fromExpr        
      Nothing -> pure fromExpr
    orderExprs <- convertOrderByClause typeF tContext (orderByClause tExpr)
    let dfExpr = DataFrameExpr { convertExpr = expr',
                                 orderExprs = orderExprs,
                                 offset = offsetClause tExpr,
                                 limit = limitClause tExpr }
    pure (dfExpr, tContext, columnRemap)

convertWhereClause :: TypeForRelExprF -> TableContext -> RestrictionExpr -> Either SQLError RestrictionPredicateExpr
convertWhereClause typeF tableContext (RestrictionExpr rexpr) = do
    let wrongType t = Left $ TypeMismatchError t BoolAtomType --must be boolean expression
        attrName' (ColumnName ts) = T.intercalate "." ts
    case rexpr of
      IntegerLiteral{} -> wrongType IntegerAtomType
      DoubleLiteral{} -> wrongType DoubleAtomType
      StringLiteral{} -> wrongType TextAtomType
      Identifier i -> wrongType TextAtomType -- could be a better error here
      BinaryOperator i@(Identifier colName) (OperatorName ["="]) exprMatch -> do --we don't know here if this results in a boolean expression, so we pass it down
        (tctx', colAlias) <- insertColumn Nothing colName Nothing tableContext
        AttributeEqualityPredicate (unColumnAlias colAlias) <$> convertScalarExpr typeF tableContext exprMatch
      BinaryOperator exprA op exprB -> do
        a <- convertScalarExpr typeF tableContext exprA
        b <- convertScalarExpr typeF tableContext exprB
        f <- lookupOperator op
        pure (AtomExprPredicate (f [a,b]))
      InExpr inOrNotIn sexpr (InList matches') -> do
        eqExpr <- convertScalarExpr typeF tableContext sexpr
        let (match:matches) = reverse matches'
        firstItem <- convertScalarExpr typeF tableContext match
        let inFunc a b = AtomExprPredicate (FunctionAtomExpr "eq" [a,b] ())
            predExpr' = inFunc eqExpr firstItem
            folder predExpr'' sexprItem = do
              item <- convertScalarExpr typeF tableContext sexprItem
              pure $ OrPredicate (inFunc eqExpr item) predExpr''
        res <- foldM folder predExpr' matches --be careful here once we introduce NULLs
        case inOrNotIn of
          In -> pure res
          NotIn -> pure (NotPredicate res)
      ExistsExpr subQ -> do
        (relExpr, tcontext') <- convertSubSelect typeF tableContext subQ
        --pretty sure I have to rename attributes in both the top-level query and in this one to prevent attribute conflicts- we can't rename all the attributes in the subquery, because the renamer won't know which attributes actually refer to the top-level attributes- should we just prefix all attributes unconditionally or send a signal upstream to rename attributes?
        let rexpr = Equals (Project A.empty relExpr) (RelationVariable "true" ())
        pure (RelationalExprPredicate rexpr)


convertScalarExpr :: TypeForRelExprF -> TableContext -> ScalarExpr -> Either SQLError AtomExpr
convertScalarExpr typeF tableContext expr = do
    let naked = pure . NakedAtomExpr
    case expr of
      IntegerLiteral i -> naked (IntegerAtom i)
      DoubleLiteral d -> naked (DoubleAtom d)
      StringLiteral s -> naked (TextAtom s)
      -- we don't have enough type context with a cast, so we default to text
      NullLiteral -> naked (ConstructedAtom "Nothing" (maybeAtomType TextAtomType) [])
      Identifier i ->
        AttributeAtomExpr <$> convertColumnName i tableContext
      BinaryOperator exprA op exprB -> do
        a <- convertScalarExpr typeF tableContext exprA
        b <- convertScalarExpr typeF tableContext exprB
        f <- lookupOperator op
        pure $ f [a,b]

convertProjectionScalarExpr :: TypeForRelExprF -> TableContext -> ProjectionScalarExpr -> Either SQLError AtomExpr
convertProjectionScalarExpr typeF tableContext expr = do
    let naked = pure . NakedAtomExpr
    case expr of
      IntegerLiteral i -> naked (IntegerAtom i)
      DoubleLiteral d -> naked (DoubleAtom d)
      StringLiteral s -> naked (TextAtom s)
      NullLiteral -> naked (ConstructedAtom "Nothing" (maybeAtomType TextAtomType) [])      
      Identifier i ->
        AttributeAtomExpr <$> convertColumnProjectionName i tableContext
      BinaryOperator exprA op exprB -> do
        a <- convertProjectionScalarExpr typeF tableContext exprA
        b <- convertProjectionScalarExpr typeF tableContext exprB
        f <- lookupOperator op
        pure $ f [a,b]

convertOrderByClause :: TypeForRelExprF -> TableContext -> [SortExpr] -> Either SQLError [AttributeOrderExpr]
convertOrderByClause typeF tableContext exprs =
  mapM converter exprs
    where
      converter (SortExpr sexpr mDirection mNullsOrder) = do
        atomExpr <- convertScalarExpr typeF tableContext sexpr
        attrn <- case atomExpr of
                   AttributeAtomExpr aname -> pure aname
                   x -> Left (NotSupportedError (T.pack (show x)))
        let ordering = case mDirection of
                         Nothing -> AscendingOrder
                         Just Ascending -> AscendingOrder
                         Just Descending -> DescendingOrder
        case mNullsOrder of
          Nothing -> pure ()
          Just x -> Left (NotSupportedError (T.pack (show x)))
        pure (AttributeOrderExpr attrn ordering)
  

convertWithClause :: TypeForRelExprF -> WithClause -> Either SQLError WithNamesAssocs
convertWithClause = undefined

type ColumnRemap = M.Map ColumnName ColumnName

convertFromClause :: TypeForRelExprF -> [TableRef] -> Either SQLError (RelationalExpr, TableContext, ColumnRemap)
convertFromClause typeF (firstRef:trefs) = do
    --the first table ref must be a straight RelationVariable
  let convertFirstTableRef (SimpleTableRef qn@(TableName [nam])) = do
        let rv = RelationVariable nam ()
        typeR <- wrapTypeF typeF rv
        let tContext = TableContext (M.singleton (TableAlias nam) (rv, attributes typeR, mempty))
        pure (rv, tContext) -- include with clause even for simple cases because we use this mapping to columns to tables
      convertFirstTableRef (AliasedTableRef tref al@(TableAlias alias)) = do
        (rvExpr, TableContext tContext) <- convertFirstTableRef tref
        (rvExpr', tContext') <- case rvExpr of
          RelationVariable oldName () ->
            let origQn = TableAlias oldName in
            case M.lookup origQn tContext of
              Just res -> pure $ (RelationVariable alias (),
                                  M.delete origQn (M.insert al res tContext))
              Nothing -> Left (MissingTableReferenceError origQn)
          other -> Left (UnexpectedRelationalExprError other)
        pure (rvExpr', TableContext tContext')
  (firstRel, tableAliases) <- convertFirstTableRef firstRef
  (expr', tContext'') <- foldM (joinTableRef typeF) (firstRel, tableAliases) (zip [1..] trefs)
  pure (expr', tContext'', mempty {- FIXME add column remapping-})

-- | Convert TableRefs after the first one (assumes all additional TableRefs are for joins). Returns the qualified name key that was added to the map, the underlying relexpr (not aliased so that it can used for extracting type information), and the new table context map
convertTableRef :: TypeForRelExprF -> TableContext -> TableRef -> Either SQLError (TableAlias, RelationalExpr, TableContext)
convertTableRef typeF tableContext tref =
  case tref of
    SimpleTableRef qn@(TableName [nam]) -> do
      let rv = RelationVariable nam ()
          ta = TableAlias nam
      typeRel <- wrapTypeF typeF rv 
      tContext' <- insertTable ta rv (attributes typeRel) tableContext
      pure (ta, rv, tContext') -- include with clause even for simple cases because we use this mapping to 
    AliasedTableRef (SimpleTableRef qn@(TableName [nam])) tAlias -> do
      typeRel <- wrapTypeF typeF (RelationVariable nam ())
      let rv = RelationVariable nam ()
      tContext' <- insertTable tAlias rv (attributes typeRel) tableContext
      pure $ (tAlias, RelationVariable nam (), tContext')
    x -> Left $ NotSupportedError (T.pack (show x))

  
joinTableRef :: TypeForRelExprF -> (RelationalExpr, TableContext) -> (Int, TableRef) -> Either SQLError (RelationalExpr, TableContext)
joinTableRef typeF (rvA, tcontext) (c,tref) = do
      -- optionally prefix attributes unelss the expr is a RelationVariable
  let attrRenamer x expr attrs = do
        renamed <- mapM (renameOneAttr x expr) attrs
        pure (Rename (S.fromList renamed) expr)
      -- prefix all attributes
      prefixRenamer prefix expr attrs = do
        renamed <- mapM (prefixOneAttr prefix) attrs
        pure (Rename (S.fromList renamed) expr)
      prefixOneAttr prefix old_name = pure (old_name, new_name)
        where
          new_name = T.concat [prefix, ".", old_name]
      renameOneAttr x expr old_name = pure (old_name, new_name)
        where
          new_name = T.concat [prefix, ".", old_name]
          prefix = case expr of
            RelationVariable rvName () -> rvName
            _ -> x -- probably need to return errors for some expressions
  case tref of
          NaturalJoinTableRef jtref -> do
            -- then natural join is the only type of join which the relational algebra supports natively
            (_, rvB, tcontext') <- convertTableRef typeF tcontext jtref
            pure $ (Join rvA rvB, tcontext')
          CrossJoinTableRef jtref -> do
            --rename all columns to prefix them with a generated alias to prevent any natural join occurring, then perform normal join
            -- we need the type to get all the attribute names for both relexprs
            (tKey, rvB, tcontext'@(TableContext tmap')) <- convertTableRef typeF tcontext jtref
            case typeF rvA of
              Left err -> Left (SQLRelationalError err)
              Right typeA ->
                case typeF rvB of
                  Left err -> Left (SQLRelationalError err)
                  Right typeB -> do
                    let attrsA = A.attributeNameSet (attributes typeA)
                        attrsB = A.attributeNameSet (attributes typeB)
                        attrsIntersection = S.intersection attrsA attrsB
                        --find intersection of attributes and rename all of them with prefix 'expr'+c+'.'
                    exprA <- attrRenamer "a" rvA (S.toList attrsIntersection)
                    pure (Join exprA rvB, tcontext')
          InnerJoinTableRef jtref (JoinUsing qnames) -> do
            (tKey, rvB, tcontext') <- convertTableRef typeF tcontext jtref
            let jCondAttrs = S.fromList $ map convertUnqualifiedColumnName qnames
            (attrsIntersection, attrsA, attrsB) <- commonAttributeNames typeF rvA rvB
            --rename attributes used in the join condition
            let attrsToRename = S.difference attrsIntersection jCondAttrs
--            traceShowM ("inner", attrsToRename, attrsIntersection, jCondAttrs)
            let rvNameB = case tKey of
                            TableAlias ta -> ta
            exprA <- attrRenamer "a" rvA (S.toList attrsToRename)
            exprB <- prefixRenamer rvNameB (RelationVariable rvNameB ()) (S.toList attrsToRename)
            pure (Join exprA exprB, tcontext')
            
          InnerJoinTableRef jtref (JoinOn (JoinOnCondition joinExpr)) -> do
            --create a cross join but extend with the boolean sexpr
            --extend the table with the join conditions, then join on those
            --exception: for simple attribute equality, use regular join renames using JoinOn logic
            
            (tKey, rvB, tContext'@(TableContext allAliases)) <- convertTableRef typeF tcontext jtref
            --rvA and rvB now reference potentially aliased relation variables (needs with clause to execute), but this is useful for making attributes rv-prefixed
--            traceShowM ("converted", rvA, rvB, tAliases)
            --extract all table aliases to create a remapping for SQL names discovered in the sexpr
            
            withExpr <- With <$> tableAliasesAsWithNameAssocs tContext'
            (commonAttrs, attrsA, attrsB) <- commonAttributeNames typeF (withExpr rvA) (withExpr rvB)
            -- first, execute the rename, renaming all attributes according to their table aliases
            let rvPrefix rvExpr =
                  case rvExpr of
                    RelationVariable nam () -> pure nam
                    x -> Left $ NotSupportedError ("cannot derived name for relational expression " <> T.pack (show x))
                   
                rvNameB = case tKey of
                             TableAlias ta -> ta
            rvNameA <- rvPrefix rvA
--            rvPrefixB <- rvPrefix rvB
            exprA <- prefixRenamer rvNameA rvA (S.toList attrsA)
            exprB <- prefixRenamer rvNameB (RelationVariable rvNameB ()) (S.toList attrsB)            
            -- for the join condition, we can potentially extend to include all the join criteria columns, then project them away after constructing the join condition
            let joinExpr' = renameIdentifier renamer joinExpr
                renamer n@(ColumnName [tAlias,attr]) = --lookup prefixed with table alias
                  case M.lookup (TableAlias tAlias) allAliases of
                    -- the table was not renamed, but the attribute may have been renamed
                    -- find the source of the attribute
                    Nothing -> n
                    Just found -> error (show (tAlias, found))
                renamer n@(ColumnName [attr]) = error (show n)
            joinRe <- convertScalarExpr typeF tContext' joinExpr'
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
            pure (projectAwayJoinMatch (joinMatchRestriction (Extend extender (Join exprB exprA))), tContext')

lookupOperator :: OperatorName -> Either SQLError ([AtomExpr] -> AtomExpr)
lookupOperator (OperatorName nam) = lookupFunc (FuncName nam)

-- this could be amended to support more complex expressions such as coalesce by returning an [AtomExpr] -> AtomExpr function
lookupFunc :: FuncName -> Either SQLError ([AtomExpr] -> AtomExpr)
lookupFunc qname =
  case qname of
    FuncName [nam] ->
      case lookup nam sqlFuncs of
        Nothing -> Left $ NoSuchSQLFunctionError qname
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
commonAttributeNames :: TypeForRelExprF -> RelationalExpr -> RelationalExpr -> Either SQLError (S.Set AttributeName, S.Set AttributeName, S.Set AttributeName)
commonAttributeNames typeF rvA rvB =
  case typeF rvA of
    Left err -> Left (SQLRelationalError err)
    Right typeA ->
      case typeF rvB of
        Left err -> Left (SQLRelationalError err)
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
  

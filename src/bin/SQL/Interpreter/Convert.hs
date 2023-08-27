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

import Debug.Trace

data SQLError = NotSupportedError T.Text |
                TypeMismatchError AtomType AtomType |
                NoSuchSQLFunctionError QualifiedName |
                DuplicateTableReferenceError QualifiedName |
                MissingTableReferenceError QualifiedName |
                ColumnResolutionError QualifiedName |
                UnexpectedRelationalExprError RelationalExpr |
                AmbiguousColumnResolutionError QualifiedName |
                SQLRelationalError RelationalError
  deriving (Show, Eq)

type TypeForRelExprF = RelationalExpr -> Either RelationalError Relation

data SelectItemsConvertTask = SelectItemsConvertTask { taskProjections :: S.Set QualifiedProjectionName,
                                                       taskRenames :: [(QualifiedProjectionName, AliasName)],
                                                       taskExtenders :: [ExtendTupleExpr]
                                                     } deriving (Show, Eq)
                                          
--over the course of conversion, we collect all the table aliases we encounter, including non-aliased table references, including the type of the table
newtype TableContext = TableContext (M.Map QualifiedName (RelationalExpr, Attributes))
  deriving (Semigroup, Monoid, Show, Eq)
  

tableAliasesAsWithNameAssocs :: TableContext -> Either SQLError WithNamesAssocs
tableAliasesAsWithNameAssocs (TableContext tmap) =
  filter notSelfRef <$> mapM mapper (M.toList tmap)
  where
    notSelfRef (WithNameExpr nam (), RelationVariable nam' ()) | nam == nam' = False
                                                            | otherwise = True
    notSelfRef _ = True
    mapper :: (QualifiedName, (RelationalExpr, Attributes)) -> Either SQLError (WithNameExpr, RelationalExpr)
    mapper (QualifiedName [nam], (rvExpr, _)) = pure (WithNameExpr nam (), rvExpr)
    mapper (qn, _) = Left (NotSupportedError ("schema qualified table names: " <> T.pack (show qn)))

-- | Insert another table into the TableContext.
insertTable :: QualifiedName -> RelationalExpr -> Attributes -> TableContext -> Either SQLError TableContext
insertTable qn expr rtype (TableContext map') =
  case M.lookup qn map' of
    Nothing -> pure $ TableContext $ M.insert qn (expr, rtype) map'
    Just _ -> Left (DuplicateTableReferenceError qn)

lookupTable :: QualifiedName -> TableContext -> Either SQLError (RelationalExpr, Attributes)
lookupTable qn (TableContext map') =
  case M.lookup qn map' of
    Nothing -> Left (MissingTableReferenceError qn)
    Just res -> pure res

replaceTableName :: QualifiedName -> QualifiedName -> TableContext -> Either SQLError TableContext
replaceTableName oldName newName (TableContext tctx) =
  case M.lookup oldName tctx of
    Nothing -> Left (MissingTableReferenceError oldName)
    Just match -> pure $ TableContext $ M.insert newName match (M.delete oldName tctx)

-- | Find a column name or column alias in the underlying table context.
findColumn :: QualifiedName -> TableContext -> [QualifiedName]
findColumn colName (TableContext tMap) =
  M.foldrWithKey folder [] tMap
   where
    folder (QualifiedName [tAlias]) (rvExpr, rtype) acc =
      case colName of
        QualifiedName [colName'] ->
          if S.member colName' (attributeNameSet rtype) then
            QualifiedName [tAlias] : acc
            else
            acc
        QualifiedName [tPrefix, colName'] ->
          if tAlias == tPrefix && colName' == colName' then
            QualifiedName [tAlias] : acc
            else
            acc
        _ -> acc

wrapTypeF :: TypeForRelExprF -> RelationalExpr -> Either SQLError Relation
wrapTypeF typeF relExpr =
  case typeF relExpr of
    Left relError -> Left (SQLRelationalError relError)
    Right v -> pure v


-- | Return the table alias for the column name iff the attribute is unique. Used for attribute resolution.
tableAliasForColumnName :: TypeForRelExprF -> QualifiedName -> TableContext -> Either SQLError QualifiedName
-- the table alias is included
tableAliasForColumnName typeF qn@(QualifiedName [tAlias, _]) (TableContext tMap) = do
  if M.member qn tMap then
    pure (QualifiedName [tAlias])
    else
    Left (ColumnResolutionError qn)
tableAliasForColumnName typeF qn@(QualifiedName [colName]) (TableContext tMap) = do
  --look up the column name in all possible tables
  res <- foldM folder Nothing (M.toList tMap)
  case res of
    Just res -> pure res
    Nothing -> Left (ColumnResolutionError qn)
  where
--    folder :: Maybe QualifiedName -> (QualifiedName, RelationalExpr) ->
    folder Just{} _ = Left (AmbiguousColumnResolutionError qn)
    folder Nothing (qn'@(QualifiedName [tableAlias]), (rvExpr,_)) = do
      tRel <- wrapTypeF typeF rvExpr -- we could cache this in the table alias map ADT
      --traceShowM ("findColName", rvExpr, tRel)
      if colName `S.member` attributeNameSet (attributes tRel) then
        pure (Just (QualifiedName [tableAlias, colName]))
        else pure Nothing

convertSelect :: TypeForRelExprF -> Select -> Either SQLError DataFrameExpr
convertSelect typeF sel = do
  let baseDFExpr = DataFrameExpr { convertExpr = MakeRelationFromExprs (Just []) (TupleExprs () []),
                                   orderExprs = [],
                                   offset = Nothing,
                                   limit = Nothing }
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

convertSelectItem :: TypeForRelExprF -> TableContext -> SelectItemsConvertTask -> (Int,SelectItem) -> Either SQLError SelectItemsConvertTask
convertSelectItem typeF tAliasMap acc (c,selItem) =
  case selItem of
    -- select * from x
    (Identifier (QualifiedProjectionName [Asterisk]), Nothing) ->
      pure acc
    -- select sup.* from s as sup
    (Identifier qpn@(QualifiedProjectionName [ProjectionName _, Asterisk]), Nothing) ->
      pure $ acc { taskProjections = S.insert qpn (taskProjections acc) }
    -- select a from x
    (Identifier qpn@(QualifiedProjectionName [ProjectionName col]), Nothing) -> do
      --look up unaliased column name
      _ <- colinfo qpn
      pure $ acc { taskProjections = S.insert qpn (taskProjections acc)
                 }
    -- select city as x from s        
    (Identifier qpn@(QualifiedProjectionName [ProjectionName _]), Just newName@(AliasName newNameTxt)) -> do
          pure $ acc { taskProjections = S.insert (QualifiedProjectionName [ProjectionName newNameTxt]) (taskProjections acc),
                       taskRenames = taskRenames acc <> [(qpn, newName)] }
    -- select s.city from s
    (Identifier qpn@(QualifiedProjectionName [ProjectionName tname, ProjectionName colname]), Nothing) -> do
      --lookup column renaming, if applicable
          pure $ acc { taskProjections = S.insert qpn (taskProjections acc),
                       taskRenames = taskRenames acc <> [(QualifiedProjectionName [ProjectionName colname], AliasName (T.intercalate "." [tname,colname]))] }
    -- other exprs
    (scalarExpr, mAlias) -> do
      let attrName' (Just (AliasName nam)) _ = nam
          attrName' Nothing c = "attr_" <> T.pack (show c)
      atomExpr <- convertProjectionScalarExpr typeF scalarExpr
      let newAttrName = attrName' mAlias c
      -- we need to apply the projections after the extension!
      pure $ acc { taskExtenders = AttributeExtendTupleExpr newAttrName atomExpr : taskExtenders acc,
                       taskProjections = S.insert (QualifiedProjectionName [ProjectionName newAttrName]) (taskProjections acc)
                     }
  where
   colinfo (QualifiedProjectionName [ProjectionName name]) =
     case tableAliasForColumnName typeF (QualifiedName [name]) tAliasMap of
       Left err -> Left err
       Right (QualifiedName names') -> pure $ AliasName (T.intercalate "." names')


convertProjection :: TypeForRelExprF -> TableContext -> [SelectItem] -> Either SQLError (RelationalExpr -> RelationalExpr)
convertProjection typeF tAliasMap selItems = do
    let emptyTask = SelectItemsConvertTask { taskProjections = S.empty,
                                             taskRenames = mempty,
                                             taskExtenders = mempty }
        attrName' (Just (AliasName nam)) _ = nam
        attrName' Nothing c = "attr_" <> T.pack (show c)
    task <- foldM (convertSelectItem typeF tAliasMap) emptyTask (zip [1::Int ..] selItems)
    --apply projections
    fProjection <- if S.null (taskProjections task) then
                     pure id
                   else do
                     let projFolder (attrNames, b) (QualifiedProjectionName [ProjectionName nam]) =
                           pure (S.insert nam attrNames, b)
                         projFolder (attrNames, b) (QualifiedProjectionName [ProjectionName nameA, ProjectionName nameB]) =
                           pure $ (S.insert (T.concat [nameA, ".", nameB]) attrNames, b)
                         projFolder (attrNames, relExprAttributes) (QualifiedProjectionName [ProjectionName tname, Asterisk]) =
                           pure $ (attrNames, relExprAttributes <> [tname])
                     (attrNames, relExprRvs) <- foldM projFolder mempty (S.toList (taskProjections task))
                     let attrsProj = A.some (map (\rv -> RelationalExprAttributeNames (RelationVariable rv ())) relExprRvs <> [AttributeNames attrNames])
                     pure $ Project attrsProj
    -- apply extensions
    let fExtended = foldr (\ext acc -> (Extend ext) . acc) id (taskExtenders task)
    -- apply rename
    renamesSet <- foldM (\acc (qProjName, (AliasName newName)) -> do
                          oldName <- convertProjectionName qProjName
                          pure $ S.insert (oldName, newName) acc) S.empty (taskRenames task)
    let fRenames = if S.null renamesSet then id else Rename renamesSet
    pure (fProjection . fExtended . fRenames)

convertProjectionName :: QualifiedProjectionName -> Either SQLError AttributeName
convertProjectionName (QualifiedProjectionName names) = do
    let namer (ProjectionName t) = pure t
        namer Asterisk = Left (NotSupportedError "asterisk in projection conversion")
    names' <- mapM namer names
    pure (T.concat names')

convertQualifiedName :: QualifiedName -> Either SQLError AttributeName
convertQualifiedName (QualifiedName ts) = pure $ T.intercalate "." ts

convertQualifiedProjectionName :: QualifiedProjectionName -> Either SQLError AttributeName
convertQualifiedProjectionName (QualifiedProjectionName names) = do
  let namer (ProjectionName t) = pure t
      namer Asterisk = error "wrong asterisk"
  names' <- mapM namer names
  pure (T.concat names')
                      
        
convertTableExpr :: TypeForRelExprF -> TableExpr -> Either SQLError (DataFrameExpr, TableContext, ColumnRemap)
convertTableExpr typeF tExpr = do
    (fromExpr, tableAliasMap, columnRemap) <- convertFromClause typeF (fromClause tExpr)
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
    pure (dfExpr, tableAliasMap, columnRemap)

convertWhereClause :: TypeForRelExprF -> RestrictionExpr -> Either SQLError RestrictionPredicateExpr
convertWhereClause typeF (RestrictionExpr rexpr) = do
    let wrongType t = Left $ TypeMismatchError t BoolAtomType --must be boolean expression
        attrName' (QualifiedName ts) = T.intercalate "." ts
    case rexpr of
      IntegerLiteral{} -> wrongType IntegerAtomType
      DoubleLiteral{} -> wrongType DoubleAtomType
      StringLiteral{} -> wrongType TextAtomType
      Identifier i -> wrongType TextAtomType -- could be a better error here
      BinaryOperator (Identifier a) (QualifiedName ["="]) exprMatch -> --we don't know here if this results in a boolean expression, so we pass it down
        AttributeEqualityPredicate (attrName' a) <$> convertScalarExpr typeF exprMatch
      BinaryOperator exprA qn exprB -> do
        a <- convertScalarExpr typeF exprA
        b <- convertScalarExpr typeF exprB
        f <- lookupFunc qn
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
        dfExpr <- convertSelect typeF subQ
        --pretty sure I have to rename attributes in both the top-level query and in this one to prevent attribute conflicts- we can't rename all the attributes in the subquery, because the renamer won't know which attributes actually refer to the top-level attributes- should we just prefix all attributes unconditionally or send a signal upstream to rename attributes?
        when (usesDataFrameFeatures dfExpr) $ Left (NotSupportedError "ORDER BY/LIMIT/OFFSET in EXISTS subquery")
        let rexpr = Equals (Project A.empty (convertExpr dfExpr)) (RelationVariable "true" ())
        pure (RelationalExprPredicate rexpr)


convertScalarExpr :: TypeForRelExprF -> ScalarExpr -> Either SQLError AtomExpr
convertScalarExpr typeF expr = do
    let naked = pure . NakedAtomExpr
    case expr of
      IntegerLiteral i -> naked (IntegerAtom i)
      DoubleLiteral d -> naked (DoubleAtom d)
      StringLiteral s -> naked (TextAtom s)
      -- we don't have enough type context with a cast, so we default to text
      NullLiteral -> naked (ConstructedAtom "Nothing" (maybeAtomType TextAtomType) [])
      Identifier i ->
        AttributeAtomExpr <$> convertQualifiedName i
      BinaryOperator exprA qn exprB -> do
        a <- convertScalarExpr typeF exprA
        b <- convertScalarExpr typeF exprB
        f <- lookupFunc qn 
        pure $ f [a,b]

convertProjectionScalarExpr :: TypeForRelExprF -> ProjectionScalarExpr -> Either SQLError AtomExpr
convertProjectionScalarExpr typeF expr = do
    let naked = pure . NakedAtomExpr
    case expr of
      IntegerLiteral i -> naked (IntegerAtom i)
      DoubleLiteral d -> naked (DoubleAtom d)
      StringLiteral s -> naked (TextAtom s)
      NullLiteral -> naked (ConstructedAtom "Nothing" (maybeAtomType TextAtomType) [])      
      Identifier i ->
        AttributeAtomExpr <$> convertQualifiedProjectionName i
      BinaryOperator exprA qn exprB -> do
        a <- convertProjectionScalarExpr typeF exprA
        b <- convertProjectionScalarExpr typeF exprB
        f <- lookupFunc qn 
        pure $ f [a,b]

convertOrderByClause :: TypeForRelExprF -> [SortExpr] -> Either SQLError [AttributeOrderExpr]
convertOrderByClause typeF exprs =
  mapM converter exprs
    where
      converter (SortExpr sexpr mDirection mNullsOrder) = do
        atomExpr <- convertScalarExpr typeF sexpr
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

type ColumnRemap = M.Map QualifiedName QualifiedName

convertFromClause :: TypeForRelExprF -> [TableRef] -> Either SQLError (RelationalExpr, TableContext, ColumnRemap)
convertFromClause typeF (firstRef:trefs) = do
    --the first table ref must be a straight RelationVariable
  let convertFirstTableRef (SimpleTableRef qn@(QualifiedName [nam])) = do
        let rv = RelationVariable nam ()
        typeR <- wrapTypeF typeF rv
        let tContext = TableContext (M.singleton qn (rv, attributes typeR))
        pure (rv, tContext) -- include with clause even for simple cases because we use this mapping to columns to tables
      convertFirstTableRef (AliasedTableRef tref (AliasName alias)) = do
        (rvExpr, TableContext tContext) <- convertFirstTableRef tref
        (rvExpr', tContext') <- case rvExpr of
          RelationVariable oldName () ->
            let origQn = QualifiedName [oldName] in
            case M.lookup origQn tContext of
              Just res -> pure $ (RelationVariable alias (),
                                  M.delete origQn (M.insert (QualifiedName [alias]) res tContext))
              Nothing -> Left (MissingTableReferenceError origQn)
          other -> Left (UnexpectedRelationalExprError other)
        pure (rvExpr', TableContext tContext')
  (firstRel, tableAliases) <- convertFirstTableRef firstRef
  (expr', tContext'') <- foldM (joinTableRef typeF) (firstRel, tableAliases) (zip [1..] trefs)
  pure (expr', tContext'', mempty {- FIXME add column remapping-})

-- | Convert TableRefs after the first one (assumes all additional TableRefs are for joins).
convertTableRef :: TypeForRelExprF -> TableContext -> TableRef -> Either SQLError (QualifiedName, RelationalExpr, TableContext)
convertTableRef typeF tableContext tref =
  case tref of
    SimpleTableRef qn@(QualifiedName [nam]) -> do
      let rv = RelationVariable nam ()
      typeRel <- wrapTypeF typeF rv 
      tContext' <- insertTable qn rv (attributes typeRel) tableContext
      pure (qn, rv, tContext') -- include with clause even for simple cases because we use this mapping to 
    AliasedTableRef (SimpleTableRef qn@(QualifiedName [nam])) (AliasName newName) -> do
      traceShowM ("aliased", nam, newName)
      typeRel <- wrapTypeF typeF (RelationVariable nam ())
      let rv = RelationVariable newName ()
          newKey = QualifiedName [newName]
      tContext' <- insertTable newKey rv (attributes typeRel) tableContext
      pure $ (newKey, RelationVariable nam (), tContext')
    x -> Left $ NotSupportedError (T.pack (show x))

  
joinTableRef :: TypeForRelExprF -> (RelationalExpr, TableContext) -> (Int, TableRef) -> Either SQLError (RelationalExpr, TableContext)
joinTableRef typeF (rvA, tcontext) (c,tref) = do
  let attrRenamer x expr attrs = do
        renamed <- mapM (renameOneAttr x expr) attrs
        pure (Rename (S.fromList renamed) expr)
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
            traceShowM ("jointref", rvB, tmap')
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
            (_, rvB, tcontext') <- convertTableRef typeF tcontext jtref
            let jCondAttrs = S.fromList $ map convertUnqualifiedName qnames
            (attrsIntersection, attrsA, attrsB) <- commonAttributeNames typeF rvA rvB
            --rename attributes used in the join condition
            let attrsToRename = S.difference  attrsIntersection jCondAttrs
--            traceShowM ("inner", attrsToRename, attrsIntersection, jCondAttrs)
            exprA <- attrRenamer "a" rvA (S.toList attrsToRename)
            pure (Join exprA rvB, tcontext')
            
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
            rvPrefixA <- rvPrefix rvA
            rvPrefixB <- rvPrefix rvB
            exprA <- attrRenamer rvPrefixA rvA (S.toList attrsA)
            exprB <- attrRenamer rvPrefixB rvB (S.toList attrsB)            
            -- for the join condition, we can potentially extend to include all the join criteria columns, then project them away after constructing the join condition
            let joinExpr' = renameIdentifier renamer joinExpr
                renamer n@(QualifiedName [tableAlias,attr]) = --lookup prefixed with table alias
                  case M.lookup n allAliases of
                    -- the table was not renamed, but the attribute may have been renamed
                    -- find the source of the attribute
                    Nothing -> n
                    Just found -> error (show (tableAlias, found))
                renamer n@(QualifiedName [attr]) = error (show n)
            joinRe <- convertScalarExpr typeF joinExpr'
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

convertUnqualifiedName :: UnqualifiedName -> AttributeName
convertUnqualifiedName (UnqualifiedName t) = t

-- this could be amended to support more complex expressions such as coalesce by returning an [AtomExpr] -> AtomExpr function
lookupFunc :: QualifiedName -> Either SQLError ([AtomExpr] -> AtomExpr)
lookupFunc qname =
  case qname of
    QualifiedName [nam] ->
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

    ------------------------------------------------
{-
class SQLConvert sqlexpr where
  type ConverterF sqlexpr :: Type
  convert :: TypeForRelExprF -> sqlexpr -> Either SQLError (ConverterF sqlexpr)

instance SQLConvert Select where
  type ConverterF Select = DataFrameExpr
  convert typeF sel = do
    --new strategy- rename all attributes by default and keep a mapping of discovered attributes. At the end of conversion, if there is no overlap in base attribute names, remove the table alias prefixes.
    projF <- convert typeF (projectionClause sel)
    -- we have explicit with clauses written by the user, but also our own implementation-specific with expressions
    explicitWithF <- case withClause sel of
                       Nothing -> pure id
                       Just wClause -> do
                         wExprs <- convert typeF wClause
                         pure (With wExprs)
      
    let baseDFExpr = DataFrameExpr { convertExpr = MakeRelationFromExprs (Just []) (TupleExprs () []),
                                     orderExprs = [],
                                     offset = Nothing,
                                     limit = Nothing }
    case tableExpr sel of
      Nothing -> 
      Just tExpr -> do
        (dfExpr, withNames) <- convert typeF tExpr
        let withF = case withNames of
                      [] -> id
                      _ -> With withNames                      
        pure (dfExpr { convertExpr = explicitWithF (withF (projF (convertExpr dfExpr))) })
                       

instance SQLConvert [SelectItem] where
  type ConverterF [SelectItem] = (RelationalExpr -> RelationalExpr)
  convert typeF selItems = do
    --SQL projections conflate static values to appear in a table with attribute names to include in the resultant relation
    --split the projections and extensions
{-    let (projections, extensions) = partition isProjection selItems
        isProjection (Identifier{},_) = True
        isProjection _ = False-}
    let emptyTask = SelectItemsConvertTask { taskProjections = S.empty,
                                             taskRenames = mempty,
                                             taskExtenders = mempty }
        attrName' (Just (AliasName nam)) _ = nam
        attrName' Nothing c = "attr_" <> T.pack (show c)
        
    let selItemFolder :: SelectItemsConvertTask -> (Int, SelectItem) -> Either SQLError SelectItemsConvertTask
        selItemFolder acc (_, (Identifier (QualifiedProjectionName [Asterisk]), Nothing)) = pure acc
        --select a from s
        selItemFolder acc (_, (Identifier qpn@(QualifiedProjectionName [ProjectionName _]), Nothing)) = 
          pure $ acc { taskProjections = S.insert qpn (taskProjections acc)
                     }
        --select t.a from test as t -- we don't support schemas yet- that would require matching three name components
        selItemFolder acc (_, (Identifier qpn@(QualifiedProjectionName [ProjectionName tname, ProjectionName colname]), Nothing)) =
          pure $ acc { taskProjections = S.insert qpn (taskProjections acc),
                       taskRenames = taskRenames acc <> [(QualifiedProjectionName [ProjectionName colname], AliasName (T.intercalate "." [tname,colname]))] }
        -- select city as x from s
        selItemFolder acc (_, (Identifier qn, Just newName@(AliasName newNameTxt))) = do
          pure $ acc { taskProjections = S.insert (QualifiedProjectionName [ProjectionName newNameTxt]) (taskProjections acc),
                       taskRenames = taskRenames acc <> [(qn, newName)] }
        -- select sup.* from s as sup
        selItemFolder acc (_, (Identifier qpn@(QualifiedProjectionName [ProjectionName _, Asterisk]), Nothing)) =
          pure $ acc { taskProjections = S.insert qpn (taskProjections acc) }
         
        selItemFolder acc (c, (scalarExpr, mAlias)) = do
          atomExpr <- convert typeF scalarExpr
          let newAttrName = attrName' mAlias c
          -- we need to apply the projections after the extension!
          pure $ acc { taskExtenders = AttributeExtendTupleExpr newAttrName atomExpr : taskExtenders acc,
                       taskProjections = S.insert (QualifiedProjectionName [ProjectionName newAttrName]) (taskProjections acc)
                     }
    task <- foldM selItemFolder emptyTask (zip [1::Int ..] selItems)
    --apply projections
    fProjection <- if S.null (taskProjections task) then
                     pure id
                   else do
                     let projFolder (attrNames, b) (QualifiedProjectionName [ProjectionName nam]) =
                           pure (S.insert nam attrNames, b)
                         projFolder (attrNames, b) (QualifiedProjectionName [ProjectionName nameA, ProjectionName nameB]) =
                           pure $ (S.insert (T.concat [nameA, ".", nameB]) attrNames, b)
                         projFolder (attrNames, relExprAttributes) (QualifiedProjectionName [ProjectionName tname, Asterisk]) =
                           pure $ (attrNames, relExprAttributes <> [tname])
                     (attrNames, relExprRvs) <- foldM projFolder mempty (S.toList (taskProjections task))
                     let attrsProj = A.some (map (\rv -> RelationalExprAttributeNames (RelationVariable rv ())) relExprRvs <> [AttributeNames attrNames])
                     pure $ Project attrsProj
    -- apply extensions
    let fExtended = foldr (\ext acc -> (Extend ext) . acc) id (taskExtenders task)
    -- apply rename
    renamesSet <- foldM (\acc (qProjName, (AliasName newName)) -> do
                          oldName <- convert typeF qProjName
                          pure $ S.insert (oldName, newName) acc) S.empty (taskRenames task)
    let fRenames = if S.null renamesSet then id else Rename renamesSet
    pure (fProjection . fExtended . fRenames)

instance SQLConvert TableExpr where
  --pass with exprs up because they must be applied after applying projections
  type ConverterF TableExpr = (DataFrameExpr, WithNamesAssocs)
  convert typeF tExpr = do
    let renameAllAttrs = case whereClause tExpr of
          Nothing -> False
          Just wClause -> needsToRenameAllAttributes wClause
      
    (fromExpr, tableAliasMap) <- convert typeF (fromClause tExpr)
    let tableAliasMap' = M.filterWithKey filterRedundantAlias tableAliasMap
        filterRedundantAlias (QualifiedName [nam]) (RelationVariable nam' ())
          | nam == nam' = False
        filterRedundantAlias _ _ = True
    withExprs <- mapM (\(qnam, expr) -> do
                          nam <- convert typeF qnam
                          pure (WithNameExpr nam (), expr)) (M.toList tableAliasMap')

      
    expr' <- case whereClause tExpr of
      Just whereExpr -> do
        restrictPredExpr <- convert typeF whereExpr
        pure $ Restrict restrictPredExpr fromExpr        
      Nothing -> pure fromExpr
    orderExprs <- convert typeF (orderByClause tExpr)
    let dfExpr = DataFrameExpr { convertExpr = expr',
                                 orderExprs = orderExprs,
                                 offset = offsetClause tExpr,
                                 limit = limitClause tExpr }
    pure (dfExpr, withExprs)
    --group by
    --having

instance SQLConvert [SortExpr] where
  type ConverterF [SortExpr] = [AttributeOrderExpr]
  convert typeF exprs = mapM converter exprs
    where
      converter (SortExpr sexpr mDirection mNullsOrder) = do
        atomExpr <- convert typeF sexpr
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
    
instance SQLConvert [TableRef] where
  -- returns base relation expressions plus top-level renames required
  type ConverterF [TableRef] = (RelationalExpr, TableAliasMap)
  convert _ [] = pure (ExistingRelation relationFalse, M.empty)
  convert typeF (firstRef:trefs) = do
    --the first table ref must be a straight RelationVariable
    (firstRel, tableAliases) <- convert typeF firstRef
    (expr', tableAliases') <- foldM joinTRef (firstRel, tableAliases) (zip [1..] trefs)
    pure (expr', tableAliases')
    where
    --TODO: if any of the previous relations have overlap in their attribute names, we must change it to prevent a natural join!
      joinTRef (rvA,tAliasesA) (c,tref) = do
        let attrRenamer x expr attrs = do
              renamed <- mapM (renameOneAttr x expr) attrs
              pure (Rename (S.fromList renamed) expr)
            renameOneAttr x expr old_name = pure (old_name, new_name)
              where
                  new_name = T.concat [prefix, ".", old_name]
                  prefix = case expr of
                             RelationVariable rvName () -> rvName
                             _ -> x -- probably need to return errors for some expressions
                
        case tref of
          NaturalJoinTableRef jtref -> do
            -- then natural join is the only type of join which the relational algebra supports natively
            (rvB, tAliasesB) <- convert typeF jtref
            pure $ (Join rvA rvB, M.union tAliasesA tAliasesB)
          CrossJoinTableRef jtref -> do
            --rename all columns to prefix them with a generated alias to prevent any natural join occurring, then perform normal join
            -- we need the type to get all the attribute names for both relexprs
            (rvB, tAliasesB) <- convert typeF jtref
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
                    pure (Join exprA rvB, M.union tAliasesA tAliasesB)
          InnerJoinTableRef jtref (JoinUsing qnames) -> do
            (rvB, tAliasesB) <- convert typeF jtref
            jCondAttrs <- S.fromList <$> mapM (convert typeF) qnames
            (attrsIntersection, attrsA, attrsB) <- commonAttributeNames typeF rvA rvB
            --rename attributes used in the join condition
            let attrsToRename = S.difference  attrsIntersection jCondAttrs
--            traceShowM ("inner", attrsToRename, attrsIntersection, jCondAttrs)
            exprA <- attrRenamer "a" rvA (S.toList attrsToRename)
            pure (Join exprA rvB, M.union tAliasesA tAliasesB)
            
          InnerJoinTableRef jtref (JoinOn (JoinOnCondition joinExpr)) -> do
            --create a cross join but extend with the boolean sexpr
            --extend the table with the join conditions, then join on those
            --exception: for simple attribute equality, use regular join renames using JoinOn logic
            
            (rvB, tAliasesB) <- convert typeF jtref
            --rvA and rvB now reference potentially aliased relation variables (needs with clause to execute), but this is useful for making attributes rv-prefixed
--            traceShowM ("converted", rvA, rvB, tAliases)
            --extract all table aliases to create a remapping for SQL names discovered in the sexpr
            let allAliases = M.union tAliasesA tAliasesB
            withExpr <- With <$> tableAliasesAsWithNameAssocs allAliases
            (commonAttrs, attrsA, attrsB) <- commonAttributeNames typeF (withExpr rvA) (withExpr rvB)
            -- first, execute the rename, renaming all attributes according to their table aliases
            let rvPrefix rvExpr =
                  case rvExpr of
                    RelationVariable nam () -> pure nam
                    x -> Left $ NotSupportedError ("cannot derived name for relational expression " <> T.pack (show x))
            rvPrefixA <- rvPrefix rvA
            rvPrefixB <- rvPrefix rvB
            exprA <- attrRenamer rvPrefixA rvA (S.toList attrsA)
            exprB <- attrRenamer rvPrefixB rvB (S.toList attrsB)            
            -- for the join condition, we can potentially extend to include all the join criteria columns, then project them away after constructing the join condition
            let joinExpr' = renameIdentifier renamer joinExpr
                renamer n@(QualifiedName [tableAlias,attr]) = --lookup prefixed with table alias
                  case M.lookup n allAliases of
                    -- the table was not renamed, but the attribute may have been renamed
                    -- find the source of the attribute
                    Nothing -> n
                    Just found -> error (show (tableAlias, found))
                renamer n@(QualifiedName [attr]) = error (show n)
            joinRe <- convert typeF joinExpr'
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
            pure (projectAwayJoinMatch (joinMatchRestriction (Extend extender (Join exprB exprA))), allAliases)


--type AttributeNameRemap = M.Map RelVarName AttributeName

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


  
-- convert a TableRef in isolation- to be used with the first TableRef only
instance SQLConvert TableRef where
  -- return base relation variable expression plus a function to apply top-level rv renames using WithNameExpr
  type ConverterF TableRef = (RelationalExpr, TableAliasMap)
  --SELECT x FROM a,_b_ creates a cross join
  convert _ (SimpleTableRef qn@(QualifiedName [nam])) = do
    let rv = RelationVariable nam ()
    pure (rv, M.singleton qn rv) -- include with clause even for simple cases because we use this mapping to 
  convert typeF (AliasedTableRef tnam (AliasName newName)) = do
    (rv, _) <- convert typeF tnam
    pure $ (RelationVariable newName (), M.singleton (QualifiedName [newName]) rv)
  convert _ x = Left $ NotSupportedError (T.pack (show x))


instance SQLConvert RestrictionExpr where
  type ConverterF RestrictionExpr = RestrictionPredicateExpr
  convert typeF (RestrictionExpr rexpr) = do
    let wrongType t = Left $ TypeMismatchError t BoolAtomType --must be boolean expression
        attrName' (QualifiedName ts) = T.intercalate "." ts
    case rexpr of
      IntegerLiteral{} -> wrongType IntegerAtomType
      DoubleLiteral{} -> wrongType DoubleAtomType
      StringLiteral{} -> wrongType TextAtomType
      Identifier i -> wrongType TextAtomType -- could be a better error here
      BinaryOperator (Identifier a) (QualifiedName ["="]) exprMatch -> --we don't know here if this results in a boolean expression, so we pass it down
        AttributeEqualityPredicate (attrName' a) <$> convert typeF exprMatch
      BinaryOperator exprA qn exprB -> do
        a <- convert typeF exprA
        b <- convert typeF exprB
        f <- lookupFunc qn
        pure (AtomExprPredicate (f [a,b]))
      InExpr inOrNotIn sexpr (InList matches') -> do
        eqExpr <- convert typeF sexpr
        let (match:matches) = reverse matches'
        firstItem <- convert typeF match
        let inFunc a b = AtomExprPredicate (FunctionAtomExpr "eq" [a,b] ())
            predExpr' = inFunc eqExpr firstItem
            folder predExpr'' sexprItem = do
              item <- convert typeF sexprItem
              pure $ OrPredicate (inFunc eqExpr item) predExpr''
        res <- foldM folder predExpr' matches --be careful here once we introduce NULLs
        case inOrNotIn of
          In -> pure res
          NotIn -> pure (NotPredicate res)
      ExistsExpr subQ -> do
        dfExpr <- convert typeF subQ
        --pretty sure I have to rename attributes in both the top-level query and in this one to prevent attribute conflicts- we can't rename all the attributes in the subquery, because the renamer won't know which attributes actually refer to the top-level attributes- should we just prefix all attributes unconditionally or send a signal upstream to rename attributes?
        when (usesDataFrameFeatures dfExpr) $ Left (NotSupportedError "ORDER BY/LIMIT/OFFSET in EXISTS subquery")
        let rexpr = Equals (Project A.empty (convertExpr dfExpr)) (RelationVariable "true" ())
        pure (RelationalExprPredicate rexpr)
          
        
-}
{-      
instance SQLConvert ScalarExpr where
  type ConverterF ScalarExpr = AtomExpr
  convert typeF expr = do
    let naked = pure . NakedAtomExpr
    case expr of
      IntegerLiteral i -> naked (IntegerAtom i)
      DoubleLiteral d -> naked (DoubleAtom d)
      StringLiteral s -> naked (TextAtom s)
      -- we don't have enough type context with a cast, so we default to text
      NullLiteral -> naked (ConstructedAtom "Nothing" (maybeAtomType TextAtomType) [])
      Identifier i ->
        AttributeAtomExpr <$> convert typeF i
      BinaryOperator exprA qn exprB -> do
        a <- convert typeF exprA
        b <- convert typeF exprB
        f <- lookupFunc qn 
        pure $ f [a,b]

--      PrefixOperator qn expr -> do


instance SQLConvert JoinOnCondition where
  type ConverterF JoinOnCondition = (RelationalExpr -> RelationalExpr)
  convert typeF (JoinOnCondition expr) = do
    case expr of
      Identifier (QualifiedName [tAlias, colName]) -> undefined

instance SQLConvert ProjectionScalarExpr where
  type ConverterF ProjectionScalarExpr = AtomExpr
  convert typeF expr = do
    let naked = pure . NakedAtomExpr
    case expr of
      IntegerLiteral i -> naked (IntegerAtom i)
      DoubleLiteral d -> naked (DoubleAtom d)
      StringLiteral s -> naked (TextAtom s)
      NullLiteral -> naked (ConstructedAtom "Nothing" (maybeAtomType TextAtomType) [])      
      Identifier i ->
        AttributeAtomExpr <$> convert typeF i
      BinaryOperator exprA qn exprB -> do
        a <- convert typeF exprA
        b <- convert typeF exprB
        f <- lookupFunc qn 
        pure $ f [a,b]

instance SQLConvert QualifiedName where
  type ConverterF QualifiedName = AttributeName
  convert _ (QualifiedName ts) = pure $ T.intercalate "." ts

instance SQLConvert UnqualifiedName where
  type ConverterF UnqualifiedName = AttributeName
  convert _ (UnqualifiedName t) = pure t

instance SQLConvert QualifiedProjectionName where
  type ConverterF QualifiedProjectionName = AttributeName
  convert _ (QualifiedProjectionName names) = do
    let namer (ProjectionName t) = pure t
        namer Asterisk = error "wrong asterisk"
    names' <- mapM namer names
    pure (T.concat names')

instance SQLConvert WithClause where
  type ConverterF WithClause = WithNamesAssocs
  convert typeF (WithClause True _) = Left (NotSupportedError "recursive CTEs")
  convert typeF (WithClause False ctes) = do
    let mapper (WithExpr (UnqualifiedName nam) subquery) = do
          dfExpr <- convert typeF subquery
          -- we don't support dataframe features in the cte query
          when (usesDataFrameFeatures dfExpr) $ Left (NotSupportedError "ORDER BY/LIMIT/OFFSET in CTE subexpression")
          pure (WithNameExpr nam (), (convertExpr dfExpr))
    -- if the subquery is a Select, how do I get a rvexpr out of it rather than a data frame- perhaps a different conversion function?
    mapM mapper (NE.toList ctes)
-}    

-- | Used to remap SQL qualified names to new names to prevent conflicts in join conditions.
renameIdentifier :: (QualifiedName -> QualifiedName) -> ScalarExpr -> ScalarExpr
renameIdentifier renamer sexpr = Fold.cata renamer' sexpr
  where
    renamer' :: ScalarExprBaseF QualifiedName ScalarExpr -> ScalarExpr
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
  

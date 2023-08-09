--convert SQL into relational or database context expressions
{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables, TypeApplications #-}
module SQL.Interpreter.Convert where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.DataFrame (DataFrameExpr(..), AttributeOrderExpr(..), AttributeOrder(..),Order(..))
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

import Debug.Trace

data SQLError = NotSupportedError T.Text |
                TypeMismatchError AtomType AtomType |
                NoSuchSQLFunctionError QualifiedName |
                DuplicateTableReferenceError QualifiedName |
                SQLRelationalError RelationalError
  deriving (Show, Eq)

type TypeForRelExprF = RelationalExpr -> Either RelationalError Relation

class SQLConvert sqlexpr where
  type ConverterF sqlexpr :: Type
  convert :: TypeForRelExprF -> sqlexpr -> Either SQLError (ConverterF sqlexpr)

instance SQLConvert Select where
  type ConverterF Select = DataFrameExpr
  convert typeF sel = do
    projF <- convert typeF (projectionClause sel)
    let baseDFExpr = DataFrameExpr { convertExpr = ExistingRelation relationTrue,
                                     orderExprs = [],
                                     offset = Nothing,
                                     limit = Nothing }
    case tableExpr sel of
      Nothing -> pure baseDFExpr
      Just tExpr -> do
        (dfExpr, withNames) <- convert typeF tExpr
        let withF = case withNames of
                      [] -> id
                      _ -> With withNames
        pure (dfExpr { convertExpr = withF (projF (convertExpr dfExpr)) })
                       

tableAliasesAsWithNameAssocs :: TableAliasMap -> Either SQLError WithNamesAssocs
tableAliasesAsWithNameAssocs tmap =
  filter notSelfRef <$> mapM mapper (M.toList tmap)
  where
    notSelfRef (WithNameExpr nam (), RelationVariable nam' ()) | nam == nam' = False
                                                            | otherwise = True
    notSelfRef _ = True
    mapper (QualifiedName [nam], rvExpr) = pure (WithNameExpr nam (), rvExpr)
    mapper (qn, _) = Left (NotSupportedError ("schema qualified table names: " <> T.pack (show qn)))

data SelectItemsConvertTask = SelectItemsConvertTask { taskProjections :: S.Set QualifiedProjectionName,
                                                       taskRenames :: [(QualifiedProjectionName, AliasName)],
                                                       taskExtenders :: [ExtendTupleExpr]
                                                     } deriving (Show, Eq)
                                          
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
    let fExtended = foldl' (\acc ext -> (Extend ext) . acc) id (taskExtenders task)
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
            traceShowM ("joinExpr'", joinExpr')
            joinRe <- convert typeF joinExpr'
            traceShowM ("joinRe", joinRe)
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


--over the course of conversion, we collect all the table aliases we encounter, including non-aliased table references            
type TableAliasMap = M.Map QualifiedName RelationalExpr

insertTableAlias :: QualifiedName -> RelationalExpr -> TableAliasMap -> Either SQLError TableAliasMap
insertTableAlias qn expr map' =
  case M.lookup qn map' of
    Nothing -> pure $ M.insert qn expr map'
    Just _ -> Left (DuplicateTableReferenceError qn)
  
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
      
instance SQLConvert ScalarExpr where
  type ConverterF ScalarExpr = AtomExpr
  convert typeF expr = do
    let naked = pure . NakedAtomExpr
    case expr of
      IntegerLiteral i -> naked (IntegerAtom i)
      DoubleLiteral d -> naked (DoubleAtom d)
      StringLiteral s -> naked (TextAtom s)
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

-- | Used to remap SQL qualified names to new names to prevent conflicts in join conditions.
renameIdentifier :: (QualifiedName -> QualifiedName) -> ScalarExpr -> ScalarExpr
renameIdentifier renamer sexpr = Fold.cata renamer' sexpr
  where
    renamer' :: ScalarExprBaseF QualifiedName ScalarExpr -> ScalarExpr
    renamer' (IdentifierF n) = Identifier (renamer n)
    renamer' x = Fold.embed x


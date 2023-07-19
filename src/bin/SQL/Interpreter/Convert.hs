--convert SQL into relational or database context expressions
{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables, TypeApplications #-}
module SQL.Interpreter.Convert where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.AttributeNames as A
import ProjectM36.Attribute as A
import qualified ProjectM36.WithNameExpr as W
import SQL.Interpreter.Select
import Data.Kind (Type)
import Data.Text as T (pack,intercalate,Text,concat)
import ProjectM36.Relation
import Control.Monad (foldM)
import qualified Data.Set as S
import Data.List (foldl')
import qualified Data.Functor.Foldable as Fold

import Debug.Trace

data SQLError = NotSupportedError T.Text |
                TypeMismatch AtomType AtomType |
                NoSuchFunction QualifiedName |
                SQLRelationalError RelationalError
  deriving (Show, Eq)

type TypeForRelExprF = RelationalExpr -> Either RelationalError Relation

class SQLConvert sqlexpr where
  type ConverterF sqlexpr :: Type
  convert :: TypeForRelExprF -> sqlexpr -> Either SQLError (ConverterF sqlexpr)

instance SQLConvert Select where
  type ConverterF Select = RelationalExpr
  convert typeF sel = do
    projF <- convert typeF (projectionClause sel)
    case tableExpr sel of
      Nothing -> pure $ ExistingRelation relationTrue
      Just tExpr -> do
        (rvExpr, withNames) <- convert typeF tExpr
        let withF = case withNames of
                      [] -> id
                      _ -> With withNames
        pure (withF (projF rvExpr))

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
          pure $ acc { taskExtenders = AttributeExtendTupleExpr (attrName' mAlias c) atomExpr : taskExtenders acc }
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
    fRenames <- foldM (\acc (qProjName, (AliasName newName)) -> do
                          oldName <- convert typeF qProjName
                          pure $ Rename oldName newName . acc) id (taskRenames task)
    pure (fExtended . fProjection . fRenames)

instance SQLConvert TableExpr where
  type ConverterF TableExpr = (RelationalExpr, WithNamesAssocs)
  --does not handle non-relational aspects such as offset, order by, or limit
  convert typeF tExpr = do
    (fromExpr, withExprs) <- convert typeF (fromClause tExpr)
    expr' <- case whereClause tExpr of
      Just whereExpr -> do
        restrictPredExpr <- convert typeF whereExpr
        pure $ Restrict restrictPredExpr fromExpr        
      Nothing -> pure fromExpr
    pure (expr', withExprs)
    --group by
    --having


instance SQLConvert [TableRef] where
  -- returns base relation expressions plus top-level renames required
  type ConverterF [TableRef] = (RelationalExpr, WithNamesAssocs)
  convert _ [] = pure (ExistingRelation relationFalse, [])
  convert typeF (firstRef:trefs) = do
    --the first table ref must be a straight RelationVariable
    (firstRel, withRenames) <- convert typeF firstRef
    (expr', withRenames') <- foldM joinTRef (firstRel, withRenames) (zip [1..] trefs)
    pure (expr', withRenames')
    where
    --TODO: if any of the previous relations have overlap in their attribute names, we must change it to prevent a natural join!
      joinTRef (rvA,withRenames) (c,tref) = do
        let renamerFolder x expr old_name =
              let new_name = T.concat [old_name, "_", x, T.pack (show c)]
              in
                pure $ Rename old_name new_name expr
        case tref of
          NaturalJoinTableRef jtref -> do
            -- then natural join is the only type of join which the relational algebra supports natively
            (rvB, withRenames') <- convert typeF jtref
            pure $ (Join rvA rvB, withRenames <> withRenames')
          CrossJoinTableRef jtref -> do
            --rename all columns to prefix them with a generated alias to prevent any natural join occurring, then perform normal join
            -- we need the type to get all the attribute names for both relexprs
            (rvB, withRenames') <- convert typeF jtref
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
                    traceShowM ("cross gonk", attrsIntersection)
                    exprA <- foldM (renamerFolder "a") rvA (S.toList attrsIntersection)
                    pure (Join exprA rvB, withRenames')
          InnerJoinTableRef jtref (JoinUsing qnames) -> do
            (rvB, withRenames') <- convert typeF jtref
            jCondAttrs <- S.fromList <$> mapM (convert typeF) qnames
            (attrsIntersection, attrsA, attrsB) <- commonAttributeNames typeF rvA rvB
            --rename attributes which are not part of the join condition
            let attrsToRename = S.difference  attrsIntersection jCondAttrs
            traceShowM ("inner", attrsToRename, attrsIntersection, jCondAttrs)
            exprA <- foldM (renamerFolder "a") rvA (S.toList attrsToRename)
            pure (Join exprA rvB, withRenames')
          InnerJoinTableRef jtref (JoinOn sexpr) -> do
            --create a cross join but extend with the boolean sexpr
            --extend the table with the join conditions, then join on those
            --exception: for simple attribute equality, use regular join renames using JoinOn logic
            (rvB, withRenames') <- convert typeF jtref
            --extract all table aliases to create a remapping for SQL names discovered in the sexpr
            (commonAttrs, attrsA, attrsB) <- commonAttributeNames typeF rvA rvB            
            let sexpr' = renameIdentifier renamer sexpr
                renamer n@(QualifiedName [tableAlias,attr]) = --lookup prefixed with table alias
                  case W.lookup tableAlias withRenames' of
                    Nothing -> QualifiedName [attr]-- the table was not renamed, but the attribute may have been renamed- how do we know at this point when the sexpr' converter hasn't run yet?!
                    Just found -> error (show (tableAlias, found))
                renamer n@(QualifiedName [attr]) = error (show n)
            joinRe <- convert typeF sexpr'

            --rename all common attrs and use the new names in the join condition
            exprA <- foldM (renamerFolder "a") rvA (S.toList commonAttrs)
            exprB <- foldM (renamerFolder "b") rvB (S.toList commonAttrs)
            let allAttrs = S.union attrsA attrsB
                firstAvailableName c allAttrs' =
                  let new_name = T.pack ("join_" <> show c) in
                  if S.member new_name allAttrs' then
                    firstAvailableName (c + 1) allAttrs'
                    else
                    new_name
                extender = AttributeExtendTupleExpr (firstAvailableName 1 allAttrs) joinRe
            pure (Join (Extend extender exprA) exprB, withRenames')


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
  type ConverterF TableRef = (RelationalExpr, WithNamesAssocs)
  --SELECT x FROM a,_b_ creates a cross join
  convert _ (SimpleTableRef (QualifiedName [nam])) =
    pure (RelationVariable nam (), [])
  convert typeF (AliasedTableRef tnam (AliasName newName)) = do
    (rv, withNames) <- convert typeF tnam
    pure $ (RelationVariable newName (), (WithNameExpr newName (), rv):withNames) 
  convert _ x = Left $ NotSupportedError (T.pack (show x))


instance SQLConvert RestrictionExpr where
  type ConverterF RestrictionExpr = RestrictionPredicateExpr
  convert typeF (RestrictionExpr rexpr) = do
    let wrongType t = Left $ TypeMismatch t BoolAtomType --must be boolean expression
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

-- this could be amended to support more complex expressions such as coalesce by returning an [AtomExpr] -> AtomExpr function
lookupFunc :: QualifiedName -> Either SQLError ([AtomExpr] -> AtomExpr)
lookupFunc qname =
  case qname of
    QualifiedName [nam] ->
      case lookup nam sqlFuncs of
        Nothing -> Left $ NoSuchFunction qname
        Just match -> pure match
  where
    f n args = FunctionAtomExpr n args ()
    sqlFuncs = [(">",f "gt"),
                 ("<",f "lt"),
                 (">=",f "gte"),
                 ("<=",f "lte"),
                 ("=",f "eq"),
                 ("!=",f "not_eq"), -- function missing
                 ("<>",f "not_eq") -- function missing
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


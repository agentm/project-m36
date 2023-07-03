--convert SQL into relational or database context expressions
{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables, TypeApplications #-}
module SQL.Interpreter.Convert where
import ProjectM36.Base
import ProjectM36.AttributeNames as A
import SQL.Interpreter.Select
import Data.Kind (Type)
import Data.Text as T (pack,intercalate,Text,concat)
import ProjectM36.Relation
import Control.Monad (foldM)
import qualified Data.Set as S
import Data.List (foldl')

import Debug.Trace

data SQLError = NotSupportedError T.Text |
                TypeMismatch AtomType AtomType |
                NoSuchFunction QualifiedName
  deriving (Show, Eq)

class SQLConvert sqlexpr where
  type ConverterF sqlexpr :: Type
  convert :: sqlexpr -> Either SQLError (ConverterF sqlexpr)

instance SQLConvert Select where
  type ConverterF Select = RelationalExpr
  convert sel = do
    projF <- convert (projectionClause sel)
    case tableExpr sel of
      Nothing -> pure $ ExistingRelation relationTrue
      Just tExpr -> do
        (rvExpr, withNames) <- convert tExpr
        let withF = case withNames of
                      [] -> id
                      wnames -> With withNames
        pure (withF (projF rvExpr))

data SelectItemsConvertTask = SelectItemsConvertTask { taskProjections :: S.Set QualifiedProjectionName,
                                                       taskRenames :: [(QualifiedProjectionName, AliasName)],
                                                       taskExtenders :: [ExtendTupleExpr]
                                                     } deriving (Show, Eq)
                                          
instance SQLConvert [SelectItem] where
  type ConverterF [SelectItem] = (RelationalExpr -> RelationalExpr)
  convert selItems = do
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
        selItemFolder acc (_, (Identifier qpn@(QualifiedProjectionName [ProjectionName pname]), Nothing)) = 
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
        selItemFolder acc (_, (Identifier qpn@(QualifiedProjectionName [ProjectionName tname, Asterisk]), mAlias)) =
          pure $ acc { taskProjections = S.insert qpn (taskProjections acc) }
         
        selItemFolder acc (c, (scalarExpr, mAlias)) = do
          atomExpr <- convert scalarExpr
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
                          oldName <- convert qProjName
                          pure $ Rename oldName newName . acc) id (taskRenames task)
    pure (fExtended . fProjection . fRenames)

instance SQLConvert TableExpr where
  type ConverterF TableExpr = (RelationalExpr, WithNamesBlock)
  --does not handle non-relational aspects such as offset, order by, or limit
  convert tExpr = do
    (fromExpr, withExprs) <- convert (fromClause tExpr)
    expr' <- case whereClause tExpr of
      Just whereExpr -> do
        restrictPredExpr <- convert whereExpr
        pure $ Restrict restrictPredExpr fromExpr        
      Nothing -> pure fromExpr
    pure (expr', withExprs)
    --group by
    --having


instance SQLConvert [TableRef] where
  -- returns base relation expressions plus top-level renames required
  type ConverterF [TableRef] = (RelationalExpr, WithNamesBlock)
  convert [] = pure (ExistingRelation relationFalse, [])
  convert (firstRef:trefs) = do
    --the first table ref must be a straight RelationVariable
    (firstRel, withRenames) <- convert firstRef
    expr' <- foldM joinTRef firstRel trefs
    pure (expr', withRenames)
    where
    --TODO: if any of the previous relations have overlap in their attribute names, we must change it to prevent a natural join!      
      joinTRef = undefined

-- convert a TableRef in isolation- to be used with the first TableRef only
instance SQLConvert TableRef where
  -- return base relation variable expression plus a function to apply top-level rv renames using WithNameExpr
  type ConverterF TableRef = (RelationalExpr, WithNamesBlock)
  --SELECT x FROM a,_b_ creates a cross join
  convert (SimpleTableRef (QualifiedName [nam])) =
    pure (RelationVariable nam (), [])
  convert (AliasedTableRef tnam (AliasName newName)) = do
    (rv, withNames) <- convert tnam
    pure $ (RelationVariable newName (), (WithNameExpr newName (), rv):withNames) 
  convert x = Left $ NotSupportedError (T.pack (show x))


instance SQLConvert RestrictionExpr where
  type ConverterF RestrictionExpr = RestrictionPredicateExpr
  convert (RestrictionExpr rexpr) = do
    let wrongType t = Left $ TypeMismatch t BoolAtomType --must be boolean expression
        attrName' (QualifiedName ts) = T.intercalate "." ts
    case rexpr of
      IntegerLiteral{} -> wrongType IntegerAtomType
      DoubleLiteral{} -> wrongType DoubleAtomType
      StringLiteral{} -> wrongType TextAtomType
      Identifier i -> wrongType TextAtomType -- could be a better error here
      BinaryOperator (Identifier a) (QualifiedName ["="]) exprMatch -> --we don't know here if this results in a boolean expression, so we pass it down
        AttributeEqualityPredicate (attrName' a) <$> convert exprMatch
      BinaryOperator exprA qn exprB -> do
        a <- convert exprA
        b <- convert exprB
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
  convert expr = do
    let naked = pure . NakedAtomExpr
    case expr of
      IntegerLiteral i -> naked (IntegerAtom i)
      DoubleLiteral d -> naked (DoubleAtom d)
      StringLiteral s -> naked (TextAtom s)
      Identifier i ->
        AttributeAtomExpr <$> convert i
      BinaryOperator exprA qn exprB -> do
        a <- convert exprA
        b <- convert exprB
        f <- lookupFunc qn 
        pure $ f [a,b]

instance SQLConvert ProjectionScalarExpr where
  type ConverterF ProjectionScalarExpr = AtomExpr
  convert expr = do
    let naked = pure . NakedAtomExpr
    case expr of
      IntegerLiteral i -> naked (IntegerAtom i)
      DoubleLiteral d -> naked (DoubleAtom d)
      StringLiteral s -> naked (TextAtom s)
      Identifier i ->
        AttributeAtomExpr <$> convert i
      BinaryOperator exprA qn exprB -> do
        a <- convert exprA
        b <- convert exprB
        f <- lookupFunc qn 
        pure $ f [a,b]

instance SQLConvert QualifiedName where
  type ConverterF QualifiedName = AttributeName
  convert (QualifiedName ts) = pure $ T.intercalate "." ts

instance SQLConvert QualifiedProjectionName where
  type ConverterF QualifiedProjectionName = AttributeName
  convert (QualifiedProjectionName names) = do
    let namer (ProjectionName t) = pure t
        namer Asterisk = error "wrong asterisk"
    names' <- mapM namer names
    pure (T.concat names')

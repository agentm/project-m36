--convert SQL into relational or database context expressions
{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables #-}
module SQL.Interpreter.Convert where
import ProjectM36.Base
import SQL.Interpreter.Select
import Data.Kind (Type)
import Data.Text as T (pack,intercalate,Text)
import ProjectM36.Relation
import Control.Monad (foldM)
import qualified Data.Set as S

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
      Just tExpr -> projF <$> convert tExpr

instance SQLConvert [SelectItem] where
  type ConverterF [SelectItem] = (RelationalExpr -> RelationalExpr)
  convert selItems = do
    --SQL projections conflate static values to appear in a table with attribute names to include in the resultant relation    
    let folder :: (RelationalExpr -> RelationalExpr) -> (Int, SelectItem) -> Either SQLError (RelationalExpr -> RelationalExpr)
        folder _ (c,selItem) = do
           let ext atom mAlias = pure $ Extend (AttributeExtendTupleExpr (attrName' mAlias) (NakedAtomExpr atom))
           --simple projection, no alias
               proj :: AttributeName -> Maybe AliasName -> (RelationalExpr -> RelationalExpr)
               proj attr Nothing f = Project (AttributeNames (S.singleton attr)) f
               proj attr alias f = Rename attr (attrName' alias) (proj attr Nothing f)
               attrName' (Just (AliasName nam)) = nam
               attrName' Nothing = "attr_" <> T.pack (show c)
           case selItem of
             (IntegerLiteral i, mAlias) -> ext (IntegerAtom i) mAlias
               -- select * - does nothing 
             (Identifier (QualifiedProjectionName [Asterisk]), Nothing) -> pure id
               -- select a, simple, unqualified attribute projection
             (Identifier (QualifiedProjectionName [ProjectionName nam]), Nothing) -> pure $ proj nam Nothing
    foldM folder id (zip [1::Int ..] selItems)

instance SQLConvert TableExpr where
  type ConverterF TableExpr = RelationalExpr
  --does not handle non-relational aspects such as offset, order by, or limit
  convert tExpr = do
    fromExpr <- convert (fromClause tExpr)
    case whereClause tExpr of
      Just whereExpr -> do
        restrictPredExpr <- convert whereExpr
        pure $ Restrict restrictPredExpr fromExpr        
      Nothing -> pure fromExpr
    --group by
    --having


instance SQLConvert [TableRef] where
  type ConverterF [TableRef] = RelationalExpr
  convert [] = pure (ExistingRelation relationFalse)
  convert (firstRef:trefs) = do
    firstRel <- convert firstRef
    foldM joinTRef firstRel trefs
    where
      joinTRef = undefined

instance SQLConvert TableRef where
  type ConverterF TableRef = RelationalExpr
  convert (SimpleTableRef (QualifiedName [nam])) =
    pure $ RelationVariable nam ()

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

instance SQLConvert ScalarExpr where
  type ConverterF ScalarExpr = AtomExpr
  convert expr = do
    let naked = pure . NakedAtomExpr
        attrName' (QualifiedName ts) = T.intercalate "." ts
        sqlFuncs = [(">","gt"),
                    ("<","lt"),
                    (">=","gte"),
                    ("<=","lte"),
                    ("=","eq"),
                    ("!=","not_eq"), -- function missing
                    ("<>", "not_eq") -- function missing
                    ]
    case expr of
      IntegerLiteral i -> naked (IntegerAtom i)
      DoubleLiteral d -> naked (DoubleAtom d)
      StringLiteral s -> naked (TextAtom s)
      Identifier i -> pure $ AttributeAtomExpr (attrName' i)
      BinaryOperator exprA qn@(QualifiedName [op]) exprB -> do
        a <- convert exprA
        b <- convert exprB
        func <- case lookup op sqlFuncs of
          Nothing -> Left $ NoSuchFunction qn
          Just f -> pure f
        pure $ FunctionAtomExpr func [a,b] ()


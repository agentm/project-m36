--convert SQL into relational or database context expressions
{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module SQL.Interpreter.Convert where
import ProjectM36.Base
import SQL.Interpreter.Select
import Data.Kind (Type)
import Data.List (foldl')
import Data.Text as T (pack)
import ProjectM36.Relation
import Control.Monad (foldM)

data ConvertError = NotSupportedError

class SQLConvert sqlexpr where
  type ConverterF sqlexpr :: Type
  convert :: sqlexpr -> Either ConvertError (ConverterF sqlexpr)

instance SQLConvert Select where
  type ConverterF Select = RelationalExpr
  convert sel = do
    (extendExprs, attrNames) <- convert (projectionClause sel)
    let projectionAttrExprs = foldl' UnionAttributeNames (AttributeNames mempty) attrNames
    relExpr <- case tableExpr sel of
      Nothing -> pure $ ExistingRelation relationTrue
      Just tExpr -> convert tExpr
    -- add projection, if necessary
    let projection =
          if null attrNames then
            relExpr
          else
            Project projectionAttrExprs relExpr
        extendedExpr = foldl' (\acc extExpr ->
                                 Extend extExpr acc) projection extendExprs
    pure extendedExpr

instance SQLConvert [SelectItem] where
  type ConverterF [SelectItem] = ([ExtendTupleExpr],[AttributeNames])
  convert selItems = 
    --SQL projections conflate static values to appear in a table with attribute names to include in the resultant relation
    pure $ foldl' (\(extendExprs', projectionAttrExprs') (c,selItem) ->
                                      let ext :: Atom -> Maybe AliasName -> ([ExtendTupleExpr], [AttributeNames])
                                          ext atom mAlias = (extendExprs' <> [AttributeExtendTupleExpr (attrName' mAlias) (NakedAtomExpr atom)], projectionAttrExprs')
                                          --proj attr mAlias = (extendExprs', projectionAttrExprs' <> [])
                                          attrName' (Just (AliasName nam)) = nam
                                          attrName' Nothing = "attr_" <> T.pack (show c)
                                            in
                                      case selItem of
                                        (IntegerLiteral i, mAlias) -> ext (IntegerAtom i) mAlias
                                        (Identifier (QualifiedName [Asterisk]), Nothing) -> (extendExprs', projectionAttrExprs')
                                      ) mempty
                            (zip [1::Int ..] selItems)

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
  convert (firstRef:trefs) = do
    firstRel <- convert firstRef
    foldM joinTRef firstRel trefs
    where
      joinTRef = undefined

instance SQLConvert TableRef where
  type ConverterF TableRef = RelationalExpr
  convert (SimpleTableRef (QualifiedName [Name nam])) =
    pure $ RelationVariable nam ()

instance SQLConvert RestrictionExpr where
  type ConverterF RestrictionExpr = RestrictionPredicateExpr
  convert = undefined

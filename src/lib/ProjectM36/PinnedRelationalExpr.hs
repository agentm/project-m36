{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
module ProjectM36.PinnedRelationalExpr where
import ProjectM36.Base

{-
-- | Convert an ADT which may reference the uncommitted transaction to the version of the ADT which does not include the ADT or 
class ToPinned a where
  toPinned :: a GraphRefTransactionMarker -> Maybe (a TransactionId)


instance ToPinned RelationalExprBase where
  toPinned expr =
    case expr of
      MakeRelationFromExprs mAttrExprs tupleExprs -> do
        tupleExprs' <- toPinnedRelationalExpr tupleExprs
        mAttrExprs' <- case mAttrExprs of
                         Nothing -> pure Nothing
                         Just es -> Just <$> mapM toPinnedRelationalExpr es
        pure (MakeRelationFromExprs mAttrExprs' tupleExprs')

      MakeStaticRelation attrs tupSet ->
        pure (MakeStaticRelation attrs tupSet)

      ExistingRelation rel ->
        ExistingRelation rel

      RelationVariable rv m ->
        RelationVariable rv <$> toPinned
-}

toPinnedRelationalExpr :: GraphRefRelationalExpr -> Maybe PinnedRelationalExpr
toPinnedRelationalExpr = mapM mapper
  where
    mapper UncommittedContextMarker = Nothing
    mapper (TransactionMarker tid) = Just tid
  
    

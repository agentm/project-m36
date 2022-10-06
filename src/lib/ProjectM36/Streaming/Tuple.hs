{-# LANGUAGE FlexibleContexts #-}
module ProjectM36.Streaming.Tuple where
import ProjectM36.Base
import ProjectM36.Error
import qualified ProjectM36.Relation as Rel
import qualified ProjectM36.Attribute as A
import Streamly.Prelude hiding (notElem)
import Control.Monad.Catch
import qualified Streamly.Prelude as S
import ProjectM36.Tuple
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as HM
import Control.Monad.Trans.Class

type RestrictionFilter = RelationTuple -> Either RelationalError Bool

restrict :: MonadAsync m => RestrictionFilter -> AsyncT m RelationTuple -> AsyncT m RelationTuple
restrict func s = s >>= \tupIn -> case func tupIn of
  Left err -> throwM err
  Right filt -> if filt then pure tupIn else S.nil
  
cardinality :: MonadAsync m => AsyncT m RelationTuple -> m RelationCardinality
cardinality s = Finite <$> S.length (S.fromAsync s)

-- error handling needs to happen at type-checking time
rename :: MonadAsync m => AttributeName -> AttributeName -> AsyncT m RelationTuple -> AsyncT m RelationTuple
rename attrA attrB = S.mapM (pure . tupleRenameAttribute attrA attrB)

project :: MonadAsync m => Attributes -> AsyncT m RelationTuple -> AsyncT m RelationTuple
project attrs = S.mapM (\tup ->
                           case tupleProject attrs tup of
                             Left err -> throwM err
                             Right tup' -> pure tup')

union :: MonadAsync m => AsyncT m RelationTuple -> AsyncT m RelationTuple -> AsyncT m RelationTuple
union = (<>)

--nested loop join requires traversing one of the stream n times
--call S.toList on first tuple stream
--for error handling, use MonadThrow?
join :: (MonadAsync m, MonadThrow m) => Attributes -> [RelationTuple] -> AsyncT m RelationTuple -> AsyncT m RelationTuple
join joinCondition rt1List rt2 = do
  mS2Val <- lift (S.uncons (S.fromAsync rt2))
  case mS2Val of
    Nothing -> S.nil
    Just (s2Val, s2remainder) -> do
      case singleTupleSetJoin joinCondition s2Val (RelationTupleSet rt1List) of
        Left err -> throwM err
        Right joined -> S.fromFoldable joined <> join joinCondition rt1List s2remainder

difference :: (MonadAsync m) => [RelationTuple] -> AsyncT m RelationTuple -> AsyncT m RelationTuple
difference filterTuples = S.filter filt
  where
    filt tup = tup `notElem` filterTuples
    
--perhaps the relation atom should also contain a stream of tuples
group :: MonadAsync m => Attributes -> AttributeName -> Attributes -> AsyncT m RelationTuple -> AsyncT m RelationTuple
group groupAttrs newAttrName attrs tupsIn = do
  let groupAttrNames = A.attributeNameSet groupAttrs
      nonGroupAttrNames = A.nonMatchingAttributeNameSet groupAttrNames (Set.fromList (V.toList (A.attributeNames attrs)))
      nonGroupAttrs = A.attributesForNames nonGroupAttrNames attrs
      handleError val = case val of 
        Left err -> throwM err
        Right val' -> pure val'
      groupFolder oneTup accum =
        case tupleProject nonGroupAttrs oneTup of
          Right ungroupedProjection -> do
            let subTuples = case HM.lookup ungroupedProjection accum of
                              Nothing -> [oneTup]
                              Just l -> oneTup : l
            HM.insert ungroupedProjection subTuples accum                    
          Left err -> error $ "group failed" <> show err

  nonGroupProjectionAttributes <- handleError $ A.projectionAttributesForNames nonGroupAttrNames attrs
  groupProjectionAttributes <- handleError $ A.projectionAttributesForNames groupAttrNames attrs
  let newAttrs = A.addAttribute groupAttr nonGroupProjectionAttributes
      groupAttr = Attribute newAttrName (RelationAtomType groupProjectionAttributes)
      tupleGenerator nongrouped groups' accum = do
        subRel <- Rel.mkRelationFromTuples newAttrs groups'
        let subTup = RelationTuple (A.singleton groupAttr) (V.singleton (RelationAtom subRel))
        let newTup = tupleExtend nongrouped subTup
--        (:) <$> pure newTup <*> accum
        (newTup:) <$> accum
      
  groupedMap <- lift $ S.foldr groupFolder HM.empty (S.fromAsync tupsIn)
  case HM.foldrWithKey tupleGenerator (Right []) groupedMap of
    Left err -> throwM err
    Right tuples -> S.fromFoldable tuples
        
attributesForRelval :: AttributeName -> Attributes -> Either RelationalError Attributes
attributesForRelval relvalAttrName attrs = do
  atomType <- A.atomTypeForAttributeName relvalAttrName attrs
  case atomType of
    (RelationAtomType relAttrs) -> Right relAttrs
    _ -> Left $ AttributeIsNotRelationValuedError relvalAttrName
    
ungroup :: MonadAsync m => AttributeName -> Attributes -> AsyncT m RelationTuple -> AsyncT m RelationTuple
ungroup ungroupName attrs tupStream = do 
  let newAttrs = A.addAttributes attrs nonGroupAttrs
      nonGroupAttrs = A.deleteAttributeName ungroupName attrs
      mapper =
        tupleUngroup ungroupName newAttrs
  S.concatMap mapper tupStream
        
--take an relval attribute name and a tuple and ungroup the relval
tupleUngroup :: MonadAsync m => AttributeName -> Attributes -> RelationTuple -> AsyncT m RelationTuple
tupleUngroup relvalAttrName newAttrs tuple = 
  case relationForAttributeName relvalAttrName tuple of
    Left err -> throwM err
    Right relvalRelation -> 
      S.mapM mapper (tupleStream relvalRelation)
 where
   mapper rvTup = case tupleProject newAttrs tuple of
                    Left err -> throwM err
                    Right nonGroupTupleProjection ->
                      pure $ tupleExtend nonGroupTupleProjection rvTup

tupleStream :: MonadAsync m => Relation -> AsyncT m RelationTuple
tupleStream (Relation _ tupSet) = S.fromFoldable (asList tupSet)



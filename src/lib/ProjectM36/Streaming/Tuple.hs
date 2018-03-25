{-# LANGUAGE FlexibleContexts #-}
module ProjectM36.Streaming.Tuple where
import ProjectM36.Base
import ProjectM36.Error
import qualified ProjectM36.Relation as Rel
import qualified ProjectM36.Attribute as A
import Streamly
import Control.Monad.Catch
import qualified Streamly.Prelude as S
import ProjectM36.Tuple
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as HM

type RestrictionFilter = RelationTuple -> Either RelationalError Bool

type TupleStream m = StreamT m RelationTuple

restrict :: MonadAsync m => RestrictionFilter -> TupleStream m -> StreamT m RelationTuple
restrict func s = s >>= \tupIn -> case func tupIn of
  Left err -> throwM err
  Right filt -> if filt then pure tupIn else S.nil
  
cardinality :: MonadAsync m => StreamT m RelationTuple -> m RelationCardinality
cardinality m = Finite <$> S.length m

-- error handling needs to happen at type-checking time
rename :: MonadAsync m => AttributeName -> AttributeName -> StreamT m RelationTuple -> StreamT m RelationTuple
rename attrA attrB s = S.mapM (\tup -> pure (tupleRenameAttribute attrA attrB tup)) s

project :: MonadAsync m => Set.Set AttributeName -> StreamT m RelationTuple -> StreamT m RelationTuple
project attrs s = S.mapM (\tup -> pure (tupleProject attrs tup)) s

union :: MonadAsync m => StreamT m RelationTuple -> StreamT m RelationTuple -> StreamT m RelationTuple
union = (<>)

--nested loop join requires traversing one of the stream n times
--call S.toList on first tuple stream    
join :: (MonadAsync m) => Attributes -> [RelationTuple] -> StreamT m RelationTuple -> StreamT m RelationTuple
join joinCondition rt1List rt2 = do
  mS2Val <- lift (S.uncons rt2)
  case mS2Val of
    Nothing -> S.nil
    Just (s2Val, s2remainder) -> do
      case singleTupleListJoin joinCondition s2Val rt1List of
        Left err -> throwM err
        Right joined -> S.each joined <> join joinCondition rt1List s2remainder

difference :: (MonadAsync m) => [RelationTuple] -> StreamT m RelationTuple -> StreamT m RelationTuple
difference filterTuples s = S.filter filt s
  where
    filt tup = tup `notElem` filterTuples
    
--perhaps the relation atom should also contain a stream of tuples
group :: MonadAsync m => Set.Set AttributeName -> AttributeName -> Attributes -> StreamT m RelationTuple -> StreamT m RelationTuple
group groupAttrNames newAttrName attrs tupsIn = do
  let nonGroupAttrNames = A.nonMatchingAttributeNameSet groupAttrNames (Set.fromList (V.toList (A.attributeNames attrs)))
      handleError val = case val of 
        Left err -> throwM err
        Right val' -> pure val'
      groupFolder oneTup accum = do
        let ungroupedProjection = tupleProject nonGroupAttrNames oneTup 
            subTuples = case HM.lookup ungroupedProjection accum of
              Nothing -> [oneTup]
              Just l -> oneTup : l
        HM.insert ungroupedProjection subTuples accum        
  nonGroupProjectionAttributes <- handleError $ A.projectionAttributesForNames nonGroupAttrNames attrs
  groupProjectionAttributes <- handleError $ A.projectionAttributesForNames groupAttrNames attrs
  let newAttrs = A.addAttribute groupAttr nonGroupProjectionAttributes
      groupAttr = Attribute newAttrName (RelationAtomType groupProjectionAttributes)
      tupleGenerator nongrouped groups accum = do
        subRel <- Rel.mkRelationFromTuples newAttrs groups
        let subTup = RelationTuple (V.singleton groupAttr) (V.singleton (RelationAtom subRel))
        let newTup = tupleExtend nongrouped subTup
        (:) <$> pure newTup <*> accum
      
  groupedMap <- lift $ S.foldr groupFolder HM.empty tupsIn
  case HM.foldrWithKey tupleGenerator (Right []) groupedMap of
    Left err -> throwM err
    Right tuples -> S.each tuples
        
attributesForRelval :: AttributeName -> Attributes -> Either RelationalError Attributes
attributesForRelval relvalAttrName attrs = do
  atomType <- A.atomTypeForAttributeName relvalAttrName attrs
  case atomType of
    (RelationAtomType relAttrs) -> Right relAttrs
    _ -> Left $ AttributeIsNotRelationValuedError relvalAttrName
    
ungroup :: MonadAsync m => AttributeName -> Attributes -> StreamT m RelationTuple -> StreamT m RelationTuple
ungroup ungroupName attrs tupStream = do 
  let newAttrs = A.addAttributes attrs nonGroupAttrs
      nonGroupAttrs = A.deleteAttributeName ungroupName attrs
      processStream tups = do
        mVal <- lift (S.uncons tups)
        case mVal of
          Nothing -> S.nil
          Just (onetup, sRemainder) -> do
            tupleUngroup ungroupName newAttrs onetup <> processStream sRemainder
  processStream tupStream
         
--take an relval attribute name and a tuple and ungroup the relval
tupleUngroup :: MonadAsync m => AttributeName -> Attributes -> RelationTuple -> StreamT m RelationTuple
tupleUngroup relvalAttrName newAttrs tuple = 
  case relationForAttributeName relvalAttrName tuple of
    Left err -> throwM err
    Right relvalRelation -> ungroupOneTuple (tupleStream relvalRelation)
 where
   nonGroupAttrNames = A.attributeNameSet newAttrs
   nonGroupTupleProjection = tupleProject nonGroupAttrNames tuple   
   ungroupOneTuple tups = do
     mVal <- lift (S.uncons tups)
     case mVal of
       Nothing -> S.nil
       Just (oneSubTup, sRemainder) -> pure (tupleExtend nonGroupTupleProjection oneSubTup) <> ungroupOneTuple sRemainder
         
tupleStream :: MonadAsync m => Relation -> StreamT m RelationTuple
tupleStream (Relation _ tupSet) = S.each (asList tupSet)
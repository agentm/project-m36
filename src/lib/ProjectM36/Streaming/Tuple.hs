{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module ProjectM36.Streaming.Tuple where
import ProjectM36.Base
import ProjectM36.Error
import qualified ProjectM36.Relation as Rel
import qualified ProjectM36.Attribute as A
import Control.Monad.Catch (throwM, MonadThrow)
import Control.Exception (throw)
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream.Prelude (MonadAsync)
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.Stream.Prelude as SP
import qualified Streamly.Internal.Data.Stream as SD
import ProjectM36.Tuple
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as HM
import Control.Monad.Trans.Class

type RestrictionFilter = RelationTuple -> Either RelationalError Bool

restrict :: MonadAsync m => RestrictionFilter -> Stream m RelationTuple -> Stream m RelationTuple
restrict func = S.filterM filt
  where
    filt tupIn =
      case func tupIn of
        Left err -> throwM err
        Right filt' -> pure filt'
  
cardinality :: MonadAsync m => Stream m RelationTuple -> m RelationCardinality
cardinality s = Finite <$> S.fold FL.length s

-- error handling needs to happen at type-checking time
rename :: MonadAsync m => AttributeName -> AttributeName -> Stream m RelationTuple -> Stream m RelationTuple
rename attrA attrB = S.mapM (pure . tupleRenameAttribute attrA attrB)

project :: MonadAsync m => Attributes -> Stream m RelationTuple -> Stream m RelationTuple
project attrs = S.mapM (\tup ->
                           case tupleProject attrs tup of
                             Left err -> throwM err
                             Right tup' -> pure tup')

union :: MonadAsync m => Stream m RelationTuple -> Stream m RelationTuple -> Stream m RelationTuple
union s1 s2 = unionMulti [s1,s2]

unionMulti :: MonadAsync m => [Stream m RelationTuple] -> Stream m RelationTuple
unionMulti = SP.parList id

--nested loop join requires traversing one of the stream n times
--call S.toList on first tuple stream
--for error handling, use MonadThrow?
join :: forall m.(MonadAsync m, MonadThrow m, Applicative m) => Attributes -> [RelationTuple] -> Stream m RelationTuple -> Stream m RelationTuple
join joinCondition tup1List =
  SP.parConcatMap id naiveJoiner
  where
    -- O(n^2) scanning join function
    naiveJoiner :: RelationTuple -> Stream m RelationTuple
    naiveJoiner tup2In =
      case singleTupleSetJoin joinCondition tup2In (RelationTupleSet tup1List) of
        Left err -> throw err
        Right joined -> SP.fromList joined

difference :: (MonadAsync m) => [RelationTuple] -> Stream m RelationTuple -> Stream m RelationTuple
difference filterTuples = S.filter filt
  where
    filt tup = tup `notElem` filterTuples

{-
group :: MonadAsync m => Attributes -> AttributeName -> Attributes -> Stream m RelationTuple -> Stream m RelationTuple
group groupAttrs newAttrName attrs tupSIn =
  let nonGroupAttrNames = A.nonMatchingAttributeNameSet groupAttrNames (Set.fromList (V.toList (A.attributeNames attrs)))
      groupAttrNames = A.attributeNameSet groupAttrs
      nonGroupAttrs = A.attributesForNames nonGroupAttrNames attrs
      handleError val = case val of 
        Left err -> throwM err
        Right val' -> pure val'
  in
    unCrossStream $ do
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
      
  groupedMap <- lift $ S.foldr groupFolder HM.empty tupsIn
  case HM.foldrWithKey tupleGenerator (Right []) groupedMap of
    Left err -> throwM err
    Right tuples -> SP.fromList tuples

    
    case A.projectionAttributesForNames nonGroupAttrNames attrs of
      Left err -> throwM err
      Right nonGroupProjectionAttributes ->
      case A.projectionAttributesForNames groupAttrNames attrs of
        Left err -> throw err
        Right groupProjectionAttributes ->
          let grouper tupIn =
                --naive algorithm: find matching attributes in ungrouped tuples
                
          in
            SP.parConcatMap id groupFolder tupSIn
-}
  
--perhaps the relation atom should also contain a stream of tuples
group' :: MonadAsync m => Attributes -> AttributeName -> Attributes -> Stream m RelationTuple -> Stream m RelationTuple
group' groupAttrs newAttrName attrs tupsIn = SD.unCross $ do
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
      
  groupedMap <- lift $ S.foldr groupFolder HM.empty tupsIn
  case HM.foldrWithKey tupleGenerator (Right []) groupedMap of
    Left err -> throwM err
    Right tuples -> SD.mkCross (SP.fromList tuples)
        
attributesForRelval :: AttributeName -> Attributes -> Either RelationalError Attributes
attributesForRelval relvalAttrName attrs = do
  atomType <- A.atomTypeForAttributeName relvalAttrName attrs
  case atomType of
    (RelationAtomType relAttrs) -> Right relAttrs
    _ -> Left $ AttributeIsNotRelationValuedError relvalAttrName
    
ungroup :: MonadAsync m => AttributeName -> Attributes -> Stream m RelationTuple -> Stream m RelationTuple
ungroup ungroupName attrs tupStream = do 
  let newAttrs = A.addAttributes attrs nonGroupAttrs
      nonGroupAttrs = A.deleteAttributeName ungroupName attrs
      mapper =
        tupleUngroup ungroupName newAttrs
  S.concatMap mapper tupStream
        
--take an relval attribute name and a tuple and ungroup the relval
tupleUngroup :: MonadAsync m => AttributeName -> Attributes -> RelationTuple -> Stream m RelationTuple
tupleUngroup relvalAttrName newAttrs tuple = 
  case relationForAttributeName relvalAttrName tuple of
    Left err -> throw err
    Right relvalRelation -> 
      S.mapM mapper (tupleStream relvalRelation)
 where
   mapper rvTup = case tupleProject newAttrs tuple of
                    Left err -> throwM err
                    Right nonGroupTupleProjection ->
                      pure $ tupleExtend nonGroupTupleProjection rvTup

tupleStream :: MonadAsync m => Relation -> Stream m RelationTuple
tupleStream (Relation _ tupSet) = SP.fromList (asList tupSet)



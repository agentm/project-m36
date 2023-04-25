{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, BangPatterns #-}
module ProjectM36.Streaming.RelationalExpression where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.RelationalExpression
import ProjectM36.Attribute as A
import ProjectM36.Tuple
import ProjectM36.Relation hiding (relationTrue, relationFalse)
import ProjectM36.WithNameExpr
import Streamly.Data.Stream (Stream)
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Stream.Prelude as Stream
import Streamly.Internal.Control.Concurrent (MonadAsync)
import qualified Streamly.Internal.Data.Stream.StreamD as SD
import qualified Streamly.Internal.Data.Stream.StreamK as StreamK

import qualified Data.Map as M
import Control.Monad.IO.Class
import qualified Data.Vector as V
import Control.Exception
import qualified Data.HashSet as HS
import qualified Data.Set as S
import Control.DeepSeq (force)

type RelExprExecPlan = RelExprExecPlanBase ()

type GraphRefRelExprExecPlan = RelExprExecPlanBase GraphRefTransactionMarker

--this will become more useful once we have multiple join strategies, etc.
data RelExprExecPlanBase a =
  -- | Read relvar expr from transaction graph to generate tuple stream.
--  ReadExprFromTransGraph RelVarName a |
  -- | Read tuples from a tuple set cache.
  StreamTuplesFromCacheFilePlan Attributes FilePath |
  -- | Read tuples from memory.
  ReadTuplesFromMemoryPlan Attributes RelationTupleSet |
                       
                       RestrictTupleStreamPlan RestrictionFilter (RelExprExecPlanBase a) | 
                       ProjectTupleStreamPlan Attributes (RelExprExecPlanBase a) |
                       RenameTupleStreamPlan AttributeName AttributeName (RelExprExecPlanBase a) |
                       GroupTupleStreamPlan Attributes AttributeName (RelExprExecPlanBase a) |
                       UngroupTupleStreamPlan AttributeName (RelExprExecPlanBase a) |
                       ExtendTupleStreamPlan ExtendTupleProcessor (RelExprExecPlanBase a) |
                       
                       UnionTupleStreamsPlan (RelExprExecPlanBase a) (RelExprExecPlanBase a) | -- ^ no uniquification implied with Union, if it's needed, planner needs to add it
                       NaiveJoinTupleStreamsPlan (RelExprExecPlanBase a) (RelExprExecPlanBase a) |
                       DifferenceTupleStreamsPlan (RelExprExecPlanBase a) (RelExprExecPlanBase a) |
                       EqualTupleStreamsPlan (RelExprExecPlanBase a) (RelExprExecPlanBase a) |
                       NotEqualTupleStreamsPlan (RelExprExecPlanBase a) (RelExprExecPlanBase a) |
                       
                       MakeStaticRelationPlan Attributes RelationTupleSet |
                       ExistingRelationPlan Relation |
                       UniqueifyTupleStreamPlan (RelExprExecPlanBase a) -- ^ handy uniqueifier for cases when we know the sub-plan could include duplicates

instance Show (RelExprExecPlanBase a) where
  show expr =
    case expr of
      StreamTuplesFromCacheFilePlan attrs path -> "StreamTuplesFromCacheFilePlan " <> show attrs <> " " <> path
      ReadTuplesFromMemoryPlan attrs tupSet -> "ReadTuplesFromMemoryPlan " <> show attrs <> " " <> show tupSet
      RestrictTupleStreamPlan _ expr' -> "RestrictTupleStreamPlan <function> (" <> show expr' <> ")"
      ProjectTupleStreamPlan attrs expr' -> "ProjectTupleStreamPlan " <> show attrs <> " (" <> show expr' <> ")"
      RenameTupleStreamPlan attrs nam expr' -> "RenameTupleStreamPlan " <> show attrs <> " " <> show nam <> " (" <> show expr' <> ")"
      GroupTupleStreamPlan attrs nam expr' -> "GroupTupleStreamPlan " <> show attrs <> " " <> show nam <> " (" <> show expr' <> ")"
      UngroupTupleStreamPlan nam expr' -> "UngroupTupleStreamPlan " <> show nam <> " (" <> show expr' <> ")"
      ExtendTupleStreamPlan _ expr' -> "ExtendTupleStreamPlan <function> (" <> show expr' <> ")"
      UnionTupleStreamsPlan e1 e2 -> "UnionTupleStreamsPlan (" <> show e1 <> ") (" <> show e2 <> ")"
      NaiveJoinTupleStreamsPlan e1 e2 -> "NaiveJoinTupleStreamsPlan (" <> show e1 <> ") (" <> show e2 <> ")"
      DifferenceTupleStreamsPlan e1 e2 -> "DifferenceTupleStreamsPlan (" <> show e1 <> ") (" <> show e2 <> ")"
      EqualTupleStreamsPlan e1 e2 -> "EqualTupleStreamsPlan (" <> show e1 <> ") (" <> show e2 <> ")"
      NotEqualTupleStreamsPlan e1 e2 -> "NotEqualTupleStreamsPlan (" <> show e1 <> ") (" <> show e2 <> ")"
      MakeStaticRelationPlan attrs tupSet -> "MakeStaticRelationPlan " <> show attrs <> " " <> show tupSet
      ExistingRelationPlan rel -> "ExistingRelationPlan " <> show rel
      UniqueifyTupleStreamPlan e -> "UniqueifyTupleStreamPlan (" <> show e <> ")"
      
--todo- identify nodes which need uniqueifying
planGraphRefRelationalExpr :: GraphRefRelationalExpr -> 
                              GraphRefRelationalExprEnv -> 
                              Either RelationalError GraphRefRelExprExecPlan
planGraphRefRelationalExpr (RelationVariable name tid) gfEnv = do
  ctx <- runGraphRefRelationalExprM gfEnv (gfDatabaseContextForMarker tid)
  let rvMap = relationVariables ctx
  case M.lookup name rvMap of
    Nothing -> Left (RelVarNotDefinedError name)
    Just rvExpr -> planGraphRefRelationalExpr rvExpr gfEnv
  
planGraphRefRelationalExpr (Project attrNames expr) gfEnv = do
  exprT <- runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr expr)
  projectionAttrNames <- runGraphRefRelationalExprM gfEnv (evalGraphRefAttributeNames attrNames expr)
  case projectionAttributesForNames projectionAttrNames (attributes exprT) of
    Left err -> Left err
    Right attrs' -> do
      subExpr <- planGraphRefRelationalExpr expr gfEnv
      --if we know that the projection attributes represent a key, then we don't need to run the uniquification
      pure (UniqueifyTupleStreamPlan (ProjectTupleStreamPlan attrs' subExpr))
  
planGraphRefRelationalExpr (Union exprA exprB) state = do  
  planA <- planGraphRefRelationalExpr exprA state
  planB <- planGraphRefRelationalExpr exprB state
  pure (UnionTupleStreamsPlan planA planB)
  
planGraphRefRelationalExpr (Join exprA exprB) state = do  
  planA <- planGraphRefRelationalExpr exprA state
  planB <- planGraphRefRelationalExpr exprB state
  pure (NaiveJoinTupleStreamsPlan planA planB)
  
planGraphRefRelationalExpr (Difference exprA exprB) state = do
  planA <- planGraphRefRelationalExpr exprA state
  planB <- planGraphRefRelationalExpr exprB state
  pure (DifferenceTupleStreamsPlan planA planB)

planGraphRefRelationalExpr (MakeStaticRelation attributeSet tupSet) _ = pure (MakeStaticRelationPlan attributeSet tupSet)

planGraphRefRelationalExpr expr@MakeRelationFromExprs{} gfEnv = do
  rel <- runGraphRefRelationalExprM gfEnv (evalGraphRefRelationalExpr expr)
  pure (ExistingRelationPlan rel)
  
planGraphRefRelationalExpr (ExistingRelation rel) _ = pure (ExistingRelationPlan rel)  

planGraphRefRelationalExpr (Rename oldAttrName newAttrName relExpr) state = 
  RenameTupleStreamPlan oldAttrName newAttrName <$> planGraphRefRelationalExpr relExpr state
  
planGraphRefRelationalExpr (Group groupAttrNames newAttrName relExpr) gfEnv = do
  groupAttrs <- runGraphRefRelationalExprM gfEnv $ do
    groupTypes <- typeForGraphRefRelationalExpr (Project groupAttrNames relExpr)
    pure (attributes groupTypes)
  GroupTupleStreamPlan groupAttrs newAttrName <$> planGraphRefRelationalExpr relExpr gfEnv
  
planGraphRefRelationalExpr (Ungroup attrName relExpr) state = 
  UngroupTupleStreamPlan attrName <$> planGraphRefRelationalExpr relExpr state
  
planGraphRefRelationalExpr (Restrict predExpr relExpr) gfEnv = do
  rfilt <- runGraphRefRelationalExprM gfEnv $ do
    exprT <- typeForGraphRefRelationalExpr relExpr    
    predicateRestrictionFilter (attributes exprT) predExpr
  RestrictTupleStreamPlan rfilt <$> planGraphRefRelationalExpr relExpr gfEnv
  
planGraphRefRelationalExpr (Equals relExprA relExprB) state =   
  EqualTupleStreamsPlan <$> planGraphRefRelationalExpr relExprA state <*> planGraphRefRelationalExpr relExprB state
  
planGraphRefRelationalExpr (NotEquals relExprA relExprB) state =
  NotEqualTupleStreamsPlan <$> planGraphRefRelationalExpr relExprA state <*> planGraphRefRelationalExpr relExprB state
  
planGraphRefRelationalExpr (Extend extendTupleExpr relExpr) gfEnv = do
  subExprT <- runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr relExpr)
  extendProc <- runGraphRefRelationalExprM gfEnv (extendGraphRefTupleExpressionProcessor (attributes subExprT) extendTupleExpr)
  ExtendTupleStreamPlan extendProc <$> planGraphRefRelationalExpr relExpr gfEnv

planGraphRefRelationalExpr (With macros expr) gfEnv = 
  --TODO: determine if macros should be expanded or executed separately- perhaps calculate how many times a macro appears and it's calculation and size cost to determine if it should be precalculated.
  planGraphRefRelationalExpr (substituteWithNameMacros macros expr) gfEnv

data StreamRelation m = StreamRelation {
  sRelAttributes :: Attributes,
  sRelTupStream :: Stream m RelationTuple
  }

-- | Process a tuple stream into a Relation. This function presumes all validation has already been completed and will not remove duplicate tuples or tuples which do not match the attributes.
streamRelationAsRelation :: MonadIO m => StreamRelation m -> m Relation
streamRelationAsRelation (StreamRelation attrs tupS) = do
  tupSet <- Stream.toList tupS
  pure (Relation attrs (RelationTupleSet tupSet))


-- a local variant of parallel stream map which includes deepseq
streamRelationMap :: MonadIO m =>
          Attributes ->
          (RelationTuple -> m RelationTuple) -> StreamRelation m -> StreamRelation m
streamRelationMap newAttrs fun (StreamRelation _ tupSIn) =
  StreamRelation newAttrs (Stream.mapM eval tupSIn)
 where
   eval tup = do
        tup' <- fun tup
        liftIO $ evaluate (force tup')

--parFilter 
  
--until we can stream results to disk or socket, we return a lazy-list-based Relation
-- | Build a tuple stream from an execution plan to enable parallel execution.
executePlan :: (MonadIO m, MonadAsync m) => GraphRefRelExprExecPlan -> ContextTuples -> Either RelationalError (StreamRelation m)
executePlan (ReadTuplesFromMemoryPlan attrs tupSet) _ =
  pure $ StreamRelation attrs (Stream.fromList (asList tupSet))
executePlan (StreamTuplesFromCacheFilePlan{}) _ =
  --todo: enable streaming tuples from file
  undefined
executePlan (RenameTupleStreamPlan oldName newName expr) ctx = do
  relS <- executePlan expr ctx
  let newAttrs = renameAttributes oldName newName (sRelAttributes relS)
  --potential optimization- lookup attrs in advance to rename the correct vector index
  pure $ streamRelationMap newAttrs (pure . tupleRenameAttribute oldName newName) relS
executePlan (RestrictTupleStreamPlan restrictionFilter expr) ctx = do
  (StreamRelation attrs tupS) <- executePlan expr ctx
  let tupS' = Stream.filterM filt tupS
      -- since we are building up a stream data structure, we can represent in-stream failure using exceptions- we won't be able to execute the stream here to extract errors
      filt t =
        pure $ case restrictionFilter t ctx of
        Left err -> throw err -- this will blow up in a separate thread but streamly should shuttle it to the caller (I hope)
        Right !t' -> t'
  pure $ StreamRelation attrs tupS'
executePlan (ProjectTupleStreamPlan attrs expr) ctx = do
  (StreamRelation _ tupS) <- executePlan expr ctx
  let tupS' = fmap projector tupS
      --optimize by projecting on vector indexes instead
      projector t = case tupleProject attrs t of
                      Left err -> throw err
                      Right t' -> t'
  pure $ StreamRelation attrs tupS'
executePlan (UnionTupleStreamsPlan exprA exprB) ctx = do
  --ideally, the streams would have pre-ordered tuples and we could zip them together right away- if we have an ordered representation, we can drop the sorting here
  -- glue two streams together, then uniqueify
  (StreamRelation attrsA tupSa) <- executePlan exprA ctx
  (StreamRelation _ tupSb) <- executePlan exprB ctx
  let tupS' = Stream.parList id [tupSa, tupSb]
  pure (StreamRelation attrsA tupS')
executePlan (EqualTupleStreamsPlan exprA exprB) ctx = do
  (StreamRelation _ tupSa) <- executePlan exprA ctx
  (StreamRelation _ tupSb) <- executePlan exprB ctx
  let hsA = tuplesHashSet tupSa
      hscmp = tuplesHashSet (Stream.parList id [tupSa, tupSb])
      tupS' = SD.unCross $ do
        tA <- liftIO hsA
        tcmp <- liftIO hscmp
        SD.mkCross $ Stream.fromList $ 
          [RelationTuple mempty mempty | HS.size tA == HS.size tcmp]
  pure (StreamRelation mempty tupS')

executePlan (NotEqualTupleStreamsPlan exprA exprB) ctx = do
  (StreamRelation _ tupS) <- executePlan (EqualTupleStreamsPlan exprA exprB) ctx
  let tupS' = SD.unCross $ do
        el <- liftIO $ SD.head tupS
        SD.mkCross $ Stream.fromList $ case el of
                            Nothing -> [RelationTuple mempty mempty]
                            Just _ -> []
  pure (StreamRelation mempty tupS')
executePlan (MakeStaticRelationPlan attrs tupSet) _ =
  pure (StreamRelation attrs (Stream.fromList (asList tupSet)))
executePlan (ExistingRelationPlan rel) _ = do
  pure (StreamRelation (attributes rel) (Stream.fromList (asList (tupleSet rel))))
executePlan (ExtendTupleStreamPlan (newAttrs, extendProcessor) expr) ctx = do
  relS <- executePlan expr ctx
  let extender tup =
        case extendProcessor tup ctx of
          Left err -> throw err
          Right t' -> pure t'
  pure (streamRelationMap newAttrs extender relS)
executePlan (NaiveJoinTupleStreamsPlan exprA exprB) ctx = do
  --naive join by scanning both exprB into a list for repeated O(n^2) scans which is fine for "small" tables
  (StreamRelation attrsA tupSa) <- executePlan exprA ctx
  (StreamRelation attrsB tupSb) <- executePlan exprB ctx
  attrsOut <- joinAttributes attrsA attrsB
  let tupS' = SD.unCross $ do
        bTupleList <- liftIO $ Stream.toList tupSb        
        let tupleJoiner :: RelationTuple -> [RelationTuple]
            tupleJoiner tupleFromA =
              concatMap (\tupleFromB ->
                           case singleTupleJoin attrsOut tupleFromA tupleFromB of
                             Left err -> throw err
                             Right Nothing -> []
                             Right (Just joinedTuple) -> [joinedTuple]
                        ) bTupleList
        SD.mkCross $ Stream.concatMap (Stream.fromList . tupleJoiner) tupSa
  pure (StreamRelation attrsOut tupS')
executePlan (DifferenceTupleStreamsPlan exprA exprB) ctx = do
  (StreamRelation attrsA tupSa) <- executePlan exprA ctx
  (StreamRelation _ tupSb) <- executePlan exprB ctx
  let tupS' = SD.unCross $ do
        bTupleList <- liftIO $ Stream.toList tupSb        
        SD.mkCross $ Stream.filter (`notElem` bTupleList) tupSa
  pure (StreamRelation attrsA tupS')
executePlan (GroupTupleStreamPlan groupAttrs newAttrName expr) ctx = do
  --naive implementation scans for image relation for each grouped value
  (StreamRelation attrsIn tupS) <- executePlan expr ctx
  let nonGroupAttrNames = nonMatchingAttributeNameSet groupAttrNames (A.attributeNameSet attrsIn)
      groupAttrNames = A.attributeNameSet groupAttrs
  nonGroupProjectionAttributes <- projectionAttributesForNames nonGroupAttrNames attrsIn
  groupProjectionAttributes <- projectionAttributesForNames groupAttrNames attrsIn
  (StreamRelation _ nonGroupProjectionTupS) <- executePlan (ProjectTupleStreamPlan nonGroupProjectionAttributes expr) ctx
  let outAttrs = addAttribute newAttr nonGroupProjectionAttributes
      matchAttrs = V.fromList $ S.toList $ attributeNameSet nonGroupProjectionAttributes
      newAttr = Attribute newAttrName (RelationAtomType groupProjectionAttributes)
      tupS' = SD.unCross $ do
        origTupList <- liftIO $ Stream.toList tupS
        -- find matching tuples from list
        let singleTupleGroupMatcher tup =
              let matchingTuples =
                    filter (\groupTup ->
                              atomsForAttributeNames matchAttrs tup == atomsForAttributeNames matchAttrs groupTup) origTupList
                  newtups = RelationTupleSet $ map (ehandler . tupleProject groupProjectionAttributes) matchingTuples 
                  ehandler (Left err) = throw err
                  ehandler (Right t) = t
                  groupedRel = Relation groupProjectionAttributes newtups
              in
              tupleExtend tup (RelationTuple (A.singleton newAttr) (V.singleton (RelationAtom groupedRel)))
        SD.mkCross $ fmap singleTupleGroupMatcher nonGroupProjectionTupS
  pure (StreamRelation outAttrs tupS')
executePlan (UngroupTupleStreamPlan groupAttrName expr) ctx = do
  (StreamRelation attrsIn tupS) <- executePlan expr ctx
  subRelAttrs <- case atomTypeForAttributeName groupAttrName attrsIn of
    Right (RelationAtomType attrs) -> pure attrs
    _ -> Left (AttributeIsNotRelationValuedError groupAttrName)
  let outAttrs = attrsIn <> subRelAttrs
      ungroup' tup =
        --unwrap subrelation
        let subrel = case atomForAttributeName groupAttrName tup of
              Left err -> throw err
              Right (RelationAtom r) -> r
              Right _ -> throw (AttributeIsNotRelationValuedError groupAttrName) --typechecker should ensure that this never happens
            flattenedTuples = map (tupleExtend tup) (asList (tupleSet subrel))
        in
        Stream.fromList flattenedTuples
  pure (StreamRelation outAttrs (Stream.concatMap ungroup' tupS))
executePlan (UniqueifyTupleStreamPlan e) ctx = do
  (StreamRelation attrs tupS) <- executePlan e ctx
  let tupS' = SD.unCross $ do
        uniqTups <- liftIO $ tuplesHashSet tupS
        SD.mkCross $ StreamK.toStream (StreamK.fromFoldable uniqTups)
  pure (StreamRelation attrs tupS')
  

relationTrue :: (MonadIO m, Stream.MonadAsync m) => StreamRelation m
relationTrue = StreamRelation mempty (Stream.fromList [RelationTuple mempty mempty])

relationFalse :: (MonadIO m, Stream.MonadAsync m) => StreamRelation m
relationFalse = StreamRelation mempty (Stream.fromList [])

tuplesHashSet :: MonadIO m => Stream m RelationTuple -> m (HS.HashSet RelationTuple)
tuplesHashSet =
  Stream.foldr HS.insert mempty

test1 :: IO ()
test1 = do
  let attrs1 = attributesFromList [Attribute "x" IntegerAtomType]
      gfPlan = ReadTuplesFromMemoryPlan attrs1 (RelationTupleSet [RelationTuple attrs1 (V.singleton (IntegerAtom 1))])
  let (StreamRelation attrsOut tupStream) = case executePlan gfPlan (singletonContextTuple emptyTuple) of
        Left err -> throw err
        Right res -> res
  _ <- Stream.toList $ Stream.mapM print tupStream
  print attrsOut

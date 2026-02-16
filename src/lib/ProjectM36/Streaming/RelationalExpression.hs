{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, BangPatterns, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProjectM36.Streaming.RelationalExpression where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.RelationalExpression
import ProjectM36.Attribute as A
import ProjectM36.Tuple
import ProjectM36.TransactionGraph
import ProjectM36.PinnedRelationalExpr
import ProjectM36.Cache.RelationalExprCache as RECache
import ProjectM36.DatabaseContext.Types
import ProjectM36.Relation (RestrictionFilter, ContextTuples, attributes, contextTupleAtomForAttributeName, tupleSet)
import ProjectM36.WithNameExpr
import Streamly.Data.Stream (Stream)
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.Data.Stream as SD
import qualified Streamly.Data.StreamK as StreamK
import Prettyprinter
import Prettyprinter.Render.Text

import qualified Data.Map as M
import Control.Monad.IO.Class
import qualified Data.Vector as V
import qualified Data.HashSet as HS
import qualified Data.Set as S
import Control.DeepSeq (force)
import qualified Data.Text as T
import Data.Time.Clock (DiffTime)
--import qualified Data.List.NonEmpty as NE
import Control.Exception
import Control.Concurrent.STM
import qualified Data.UUID as U

newtype CacheMissException = CacheMissException FilePath
  deriving Show

instance Exception CacheMissException

type RelExprExecPlan = RelExprExecPlanBase () ()

type GraphRefRelExprExecPlan = RelExprExecPlanBase GraphRefTransactionMarker ()

type PostExecutionGraphRefRelExprExecPlan = RelExprExecPlanBase GraphRefTransactionMarker PlanNodeExecutionInfo

newtype PlanNodeExecutionInfo = PlanNodeExecutionInfo { duration :: DiffTime }
  
--this will become more useful once we have multiple join strategies, etc.
-- a: transaction marker
-- t: collected execution information as the graph is evaluated
data RelExprExecPlanBase marker execInfo =
  -- | Read relvar expr from transaction graph to generate tuple stream.
  -- Instead of locking cache entries during plans so that the entry is not ejected before the plan can run (race condition), the plan can point to a cache entry, but if the entry no longer exists, then the plan must provide an alternative solution.

--  ReadExprFromTransGraph RelVarName a |
  -- | Read tuples from a tuple set cache.
--  StreamTuplesFromCacheFilePlan Attributes FilePath execInfo (RelationalExprBase marker) |
  -- | Read tuples from memory. -- TODO: do we need this even if we have caching?
--  ReadTuplesFromMemoryPlan Attributes RelationTupleSet execInfo |
  -- | Alternative plans in case of failure. For example, the first node in the list may be to read from a cache file but the cache has since deleted the entry, so we proceed with an alternative. This is not a node to use for otherwise fatal errors.
--  AlternativePlan (RelExprExecPlanBase a t) (NE.NonEmpty (RelExprExecPlanBase a t)) t |
  -- | Run all the nodes simultaneously and return the results from the node that returns results first.
  -- RacePlan (RelExprExecPlanBase a t) (NE.NonEmpty (RelExprExecPlanBase a t)) t |
                       
                       RestrictTupleStreamPlan RestrictionFilter (RestrictionPredicateExprBase marker) (RelExprExecPlanBase marker execInfo) execInfo (RelationalExprBase marker) | -- include compiled mode for stream execution and ADT version for planning printer
                       ProjectTupleStreamPlan Attributes (RelExprExecPlanBase marker execInfo) execInfo (RelationalExprBase marker) |
                       RenameTupleStreamPlan (S.Set (AttributeName, AttributeName)) (RelExprExecPlanBase marker execInfo) execInfo (RelationalExprBase marker) |
                       GroupTupleStreamPlan Attributes AttributeName (RelExprExecPlanBase marker execInfo) execInfo (RelationalExprBase marker) |
                       UngroupTupleStreamPlan AttributeName (RelExprExecPlanBase marker execInfo) execInfo (RelationalExprBase marker) |
                       ExtendTupleStreamPlan ExtendTupleProcessor (ExtendTupleExprBase marker) (RelExprExecPlanBase marker execInfo) execInfo (RelationalExprBase marker) |
                       RelationValuedAttributeStreamPlan Attribute execInfo (RelationalExprBase marker) | 
                       
                       UnionTupleStreamsPlan (RelExprExecPlanBase marker execInfo) (RelExprExecPlanBase marker execInfo) execInfo (RelationalExprBase marker) | -- ^ no uniquification implied with Union, if it's needed, planner needs to add it
                       NaiveJoinTupleStreamsPlan (RelExprExecPlanBase marker execInfo) (RelExprExecPlanBase marker execInfo) execInfo (RelationalExprBase marker) |
                       DifferenceTupleStreamsPlan (RelExprExecPlanBase marker execInfo) (RelExprExecPlanBase marker execInfo) execInfo (RelationalExprBase marker) |
                       EqualTupleStreamsPlan (RelExprExecPlanBase marker execInfo) (RelExprExecPlanBase marker execInfo) execInfo (RelationalExprBase marker) |
                       NotEqualTupleStreamsPlan (RelExprExecPlanBase marker execInfo) (RelExprExecPlanBase marker execInfo) execInfo (RelationalExprBase marker) |
                       
                       MakeStaticRelationPlan Attributes RelationTupleSet execInfo |
                       -- TODO: ideally, the planner would create a new structure which extracts all necessary context so that we don't need to pass the entire transaction graph to execute MakeRelationFromExprsPlan
                       MakeRelationFromExprsPlan (Maybe [AttributeExprBase marker]) (TupleExprsBase marker) execInfo |
                       ExistingRelationPlan Relation execInfo |
                       UniqueifyTupleStreamPlan (RelExprExecPlanBase marker execInfo) execInfo -- ^ handy uniqueifier for cases when we know the sub-plan could include duplicates

-- | Get the original relational expression linked to the node. It can be used to generate the cache key, if relevant.
originalRelExpr :: RelExprExecPlanBase a t -> Maybe (RelationalExprBase a)
--originalRelExpr StreamTuplesFromCacheFilePlan{} = Nothing
--originalRelExpr ReadTuplesFromMemoryPlan{} = Nothing
originalRelExpr (RestrictTupleStreamPlan _ _ _ _ e) = Just e
originalRelExpr (ProjectTupleStreamPlan _ _ _ e) = Just e
originalRelExpr (RenameTupleStreamPlan _ _ _ e) = Just e
originalRelExpr (GroupTupleStreamPlan _ _ _ _ e) = Just e
originalRelExpr (UngroupTupleStreamPlan _ _ _ e) = Just e
originalRelExpr (ExtendTupleStreamPlan _ _ _ _ e) = Just e
originalRelExpr (RelationValuedAttributeStreamPlan _ _ e) = Just e
originalRelExpr (UnionTupleStreamsPlan _ _ _ e) = Just e
originalRelExpr (NaiveJoinTupleStreamsPlan _ _ _ e) = Just e
originalRelExpr (DifferenceTupleStreamsPlan _ _ _ e) = Just e
originalRelExpr (EqualTupleStreamsPlan _ _ _ e) = Just e
originalRelExpr (NotEqualTupleStreamsPlan _ _ _ e) = Just e
originalRelExpr MakeStaticRelationPlan{} = Nothing
originalRelExpr (MakeRelationFromExprsPlan mAttrExprs tupleExprs _) = Just (MakeRelationFromExprs mAttrExprs tupleExprs)
originalRelExpr (ExistingRelationPlan rel _) = Just (ExistingRelation rel)
originalRelExpr UniqueifyTupleStreamPlan{} = Nothing

{-
junction :: T.Text
junction = "\x251c\x2500"

horizontalLine :: T.Text
horizontalLine = "\x2502"
-}

-- | Create a plan Doc to show the user.
instance (Pretty t, Pretty a, Show a) => Pretty (RelExprExecPlanBase a t) where
  pretty expr =
    case expr of
{-      StreamTuplesFromCacheFilePlan attrs path t ->
        prettyNode "StreamTuplesFromCacheFilePlan" [pretty attrs, pretty path] [] t
      ReadTuplesFromMemoryPlan attrs _tupSet t ->
        prettyNode "ReadTuplesFromMemoryPlan" [pretty attrs, "<tuples elided"] [] t-}
      RestrictTupleStreamPlan _ predExpr expr' t _origExpr ->
        prettyNode "RestrictTupleStreamPlan" [pretty predExpr] [pretty expr'] t
      ProjectTupleStreamPlan attrs expr' t _ ->
        prettyNode "ProjectTupleStreamPlan " [pretty attrs] [pretty expr'] t
      RenameTupleStreamPlan attrs expr' t _ ->
        prettyNode "RenameTupleStreamPlan" [pretty attrs] [pretty expr'] t
      GroupTupleStreamPlan attrs nam expr' t _ ->
        prettyNode "GroupTupleStreamPlan" [pretty attrs <+> "as" <+> pretty nam] [pretty expr'] t
      UngroupTupleStreamPlan nam expr' t _ ->
        prettyNode "UngroupTupleStreamPlan" [pretty nam] [pretty expr'] t
      ExtendTupleStreamPlan _ extender expr' t _ ->
        prettyNode "ExtendTupleStreamPlan" [pretty extender] [pretty expr'] t
      RelationValuedAttributeStreamPlan relAttr t _ ->
        prettyNode "RelationValuedAttributeStreamPlan" [pretty relAttr] [] t
      UnionTupleStreamsPlan e1 e2 t _ ->
        prettyNode "UnionTupleStreamsPlan" [] [pretty e1, pretty e2] t
      NaiveJoinTupleStreamsPlan e1 e2 t _ ->
        prettyNode "NaiveJoinTupleStreamsPlan" [] [pretty e1, pretty e2] t
      DifferenceTupleStreamsPlan e1 e2 t _ ->
        prettyNode "DifferenceTupleStreamsPlan" [] [pretty e1, pretty e2] t
      EqualTupleStreamsPlan e1 e2 t _->
        prettyNode "EqualTupleStreamsPlan" [] [pretty e1, pretty e2] t
      NotEqualTupleStreamsPlan e1 e2 t _ ->
        prettyNode "NotEqualTupleStreamsPlan" [] [pretty e1, pretty e2] t
      MakeStaticRelationPlan attrs _tupSet t ->
        prettyNode "MakeStaticRelationPlan" [pretty attrs] ["<tuples elided>"] t
      MakeRelationFromExprsPlan mAttrs tupExprs t ->
        prettyNode "MakeRelationFromExprsPlan" [pretty mAttrs] [pretty tupExprs] t
      ExistingRelationPlan rel t ->
        prettyRelation "ExistingRelationPlan" rel t
      UniqueifyTupleStreamPlan e t ->
        prettyNode "UniqueifyTupleStreamPlan" [] [pretty e] t
{-      AlternativePlan one remainder t ->
        prettyNode "AlternativePlan" [] (pretty one : (map pretty (NE.toList remainder))) t-}
{-      RacePlan one remainder t ->
        prettyNode "RacePlan" [] (pretty one : (map pretty (NE.toList remainder))) t -}
    where
      prettyNode :: forall b. T.Text -> [Doc b] -> [Doc b] -> t -> Doc b
      prettyNode nodeName details subexprs execInfo =
        vsep ([pretty nodeName] <>
          map (indent 2) details <>
          [indent 3 (pretty execInfo)] <>
          map (indent 4) subexprs)
      prettyRelation :: forall b. T.Text -> Relation -> t -> Doc b
      prettyRelation nodeName rel execInfo =
        vsep (pretty nodeName :
         [indent 2 (pretty (attributes rel)),
          indent 4 (pretty (tupleSet rel)),
          indent 3 (pretty execInfo)]
             )



parensList :: [Doc ann] -> Doc ann
parensList = group . encloseSep (flatAlt "( " "(") (flatAlt " )" ")") ", "
      
--todo- identify nodes which need uniqueifying
planGraphRefRelationalExpr :: GraphRefRelationalExpr -> 
                              GraphRefRelationalExprEnv -> 
                              Either RelationalError GraphRefRelExprExecPlan
planGraphRefRelationalExpr (RelationVariable name tid) gfEnv = do
  ctx <- runGraphRefRelationalExprM gfEnv (gfDatabaseContextForMarker tid)
  let graph = gre_graph gfEnv
  rvMap <- resolveDBC' graph ctx relationVariables
  case M.lookup name rvMap of
    Nothing -> Left (RelVarNotDefinedError name)
    Just rvExpr -> planGraphRefRelationalExpr rvExpr gfEnv
planGraphRefRelationalExpr orig@(RelationValuedAttribute relAttrName) gfEnv = do
  case A.attributeForName relAttrName (envAttributes gfEnv) of
    Left err -> throw err
    Right relAttr -> 
      pure (RelationValuedAttributeStreamPlan relAttr () orig)
  
planGraphRefRelationalExpr orig@(Project attrNames expr) gfEnv = do
  exprT <- runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr expr)
  projectionAttrNames <- runGraphRefRelationalExprM gfEnv (evalGraphRefAttributeNames attrNames expr)
  case projectionAttributesForNames projectionAttrNames (attributes exprT) of
    Left err -> Left err
    Right attrs' -> do
      subExpr <- planGraphRefRelationalExpr expr gfEnv
      --if we know that the projection attributes represent a key, then we don't need to run the uniquification
      pure (UniqueifyTupleStreamPlan (ProjectTupleStreamPlan attrs' subExpr () orig) ())
  
planGraphRefRelationalExpr orig@(Union exprA exprB) state = do  
  planA <- planGraphRefRelationalExpr exprA state
  planB <- planGraphRefRelationalExpr exprB state
  pure (UnionTupleStreamsPlan planA planB () orig)
  
planGraphRefRelationalExpr orig@(Join exprA exprB) state = do  
  planA <- planGraphRefRelationalExpr exprA state
  planB <- planGraphRefRelationalExpr exprB state
  pure (NaiveJoinTupleStreamsPlan planA planB () orig)
  
planGraphRefRelationalExpr orig@(Difference exprA exprB) state = do
  planA <- planGraphRefRelationalExpr exprA state
  planB <- planGraphRefRelationalExpr exprB state
  pure (DifferenceTupleStreamsPlan planA planB () orig)

planGraphRefRelationalExpr (MakeStaticRelation attributeSet tupSet) _ = pure (MakeStaticRelationPlan attributeSet tupSet ())

-- MakeRelationFromExprs could include expensive atom functions
planGraphRefRelationalExpr (MakeRelationFromExprs mAttrExprs tupExprs) _gfEnv =
  pure (MakeRelationFromExprsPlan mAttrExprs tupExprs ())
  
planGraphRefRelationalExpr (ExistingRelation rel) _ = pure (ExistingRelationPlan rel ())  

planGraphRefRelationalExpr orig@(Rename renameAssoc relExpr) state = 
  RenameTupleStreamPlan renameAssoc <$> planGraphRefRelationalExpr relExpr state <*> pure () <*> pure orig
  
planGraphRefRelationalExpr orig@(Group groupAttrNames newAttrName relExpr) gfEnv = do
  groupAttrs <- runGraphRefRelationalExprM gfEnv $ do
    groupTypes <- typeForGraphRefRelationalExpr (Project groupAttrNames relExpr)
    pure (attributes groupTypes)
  GroupTupleStreamPlan groupAttrs newAttrName <$> planGraphRefRelationalExpr relExpr gfEnv <*> pure () <*> pure orig
  
planGraphRefRelationalExpr orig@(Ungroup attrName relExpr) state = 
  UngroupTupleStreamPlan attrName <$> planGraphRefRelationalExpr relExpr state <*> pure () <*> pure orig
  
planGraphRefRelationalExpr origExpr@(Restrict predExpr relExpr) gfEnv = do
  rfilt <- runGraphRefRelationalExprM gfEnv $ do
    exprT <- typeForGraphRefRelationalExpr relExpr    
    predicateRestrictionFilter (attributes exprT) predExpr
  RestrictTupleStreamPlan rfilt predExpr <$> planGraphRefRelationalExpr relExpr gfEnv <*> pure () <*> pure origExpr
  
planGraphRefRelationalExpr orig@(Equals relExprA relExprB) state =   
  EqualTupleStreamsPlan <$> planGraphRefRelationalExpr relExprA state <*> planGraphRefRelationalExpr relExprB state <*> pure () <*> pure orig
  
planGraphRefRelationalExpr orig@(NotEquals relExprA relExprB) state =
  NotEqualTupleStreamsPlan <$> planGraphRefRelationalExpr relExprA state <*> planGraphRefRelationalExpr relExprB state <*> pure () <*> pure orig
  
planGraphRefRelationalExpr orig@(Extend extendTupleExpr relExpr) gfEnv = do
  subExprT <- runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr relExpr)
  extendProc <- runGraphRefRelationalExprM gfEnv (extendGraphRefTupleExpressionProcessor (attributes subExprT) extendTupleExpr)
  ExtendTupleStreamPlan extendProc extendTupleExpr <$> planGraphRefRelationalExpr relExpr gfEnv <*> pure () <*> pure orig

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

type CacheKeyBlackList = HS.HashSet PinnedRelationalExpr
--until we can stream results to disk or socket, we return a lazy-list-based Relation
-- | Build a tuple stream from an execution plan to enable parallel execution.
executePlan :: GraphRefRelExprExecPlan -> ContextTuples -> GraphRefRelationalExprEnv -> CacheKeyBlackList -> RelExprCache -> IO (Either RelationalError (StreamRelation IO))
{-executePlan (ReadTuplesFromMemoryPlan attrs tupSet ()) _ _ _cache =
  pure $ StreamRelation attrs (Stream.fromList (asList tupSet))
executePlan (StreamTuplesFromCacheFilePlan{}) _ _ _cache =
  --todo: enable streaming tuples from file
  undefined
-}
executePlan plan ctxTuples gfEnv cacheKeyBlackList cache = do  
  -- check if plan is available in cache
  let checkCacheOr noCacheExec =
        case originalRelExpr plan of
          Nothing -> noCacheExec
          Just origRelExpr ->
            case toPinnedRelationalExpr origRelExpr of
              Nothing ->  -- no cache key available for plan
                noCacheExec
              Just cacheKey -> do
                mCachedVal <-
                  if cacheKey `HS.member` cacheKeyBlackList then -- the blacklist prevents infinite recursion into the cache
                    pure Nothing
                  else do
                    liftIO $ atomically $ RECache.lookup cacheKey cache
                case mCachedVal of -- check if the key is in the cache
                  Nothing -> noCacheExec
                  Just cacheInfo ->
                    case result cacheInfo of
                      PinnedExpressionRep pinnedRelExpr -> do
                        -- plan and execute alternative, cached expression which is equivalent to the results of the origPlan
                        let eNewPlan = planGraphRefRelationalExpr (toGraphRefRelationalExpr pinnedRelExpr) (freshGraphRefRelationalExprEnv Nothing emptyTransactionGraph)
                        case eNewPlan of
                          Left err -> pure (Left err)
                          Right newPlan -> do
                            executePlan newPlan ctxTuples gfEnv (HS.insert cacheKey cacheKeyBlackList) cache --prevent infinite loop using key blacklist
                      UnsortedTupleSetRep attrs tupSet -> do
                        pure (Right (StreamRelation attrs (Stream.fromList (asList tupSet))))
                      SortedTuplesRep _tupList _sortInfo -> error "sortedtupsrep unimplemented"
  case plan of
    RenameTupleStreamPlan attrsAssoc expr () _rexpr -> do
      checkCacheOr $ do
        eRelS <- executePlan expr ctxTuples gfEnv cacheKeyBlackList cache
        case eRelS of
          Left err -> pure (Left err)
          Right relS -> do
            let newAttrs = renameAttributes' attrsAssoc (sRelAttributes relS)
            --potential optimization- lookup attrs in advance to rename the correct vector index
            pure (Right (streamRelationMap newAttrs (pure . tupleRenameAttributes attrsAssoc) relS))
            
    RestrictTupleStreamPlan restrictionFilter _predExpr expr () _origExpr ->
      checkCacheOr $ do
        eStream <- executePlan expr ctxTuples gfEnv cacheKeyBlackList cache
        case eStream of
          Left err -> pure (Left err)
          Right (StreamRelation attrs tupS) -> do
            let tupS' = Stream.filterM filt tupS
      -- since we are building up a stream data structure, we can represent in-stream failure using exceptions- we won't be able to execute the stream here to extract errors
                filt t =
                  pure $ case restrictionFilter t ctxTuples of
                           Left err -> throw err -- this will blow up in a separate thread but streamly should shuttle it to the caller (I hope)
                           Right !t' -> t'
            pure (Right (StreamRelation attrs tupS'))
    ProjectTupleStreamPlan attrs expr () _ -> do
      checkCacheOr $ do
        eS <- executePlan expr ctxTuples gfEnv cacheKeyBlackList cache
        case eS of
          Left err -> pure (Left err)
          Right (StreamRelation _ tupS) -> do
            let tupS' = fmap projector tupS
            --optimize by projecting on vector indexes instead
                projector t = case tupleProject attrs t of
                                Left err -> throw err
                                Right t' -> t'
            pure (Right (StreamRelation attrs tupS'))
    UnionTupleStreamsPlan exprA exprB () _ -> do
      checkCacheOr $ do
        --ideally, the streams would have pre-ordered tuples and we could zip them together right away- if we have an ordered representation, we can drop the sorting here
  -- glue two streams together, then uniqueify
        ePlanA <- executePlan exprA ctxTuples gfEnv cacheKeyBlackList cache
        ePlanB <- executePlan exprB ctxTuples gfEnv cacheKeyBlackList cache
        case ePlanA of
          Left err -> pure (Left err)
          Right (StreamRelation attrsA tupSa) ->
            case ePlanB of
              Left err -> pure (Left err)
              Right (StreamRelation _ tupSb) -> do
                let tupS' = Stream.parList id [tupSa, tupSb]
                pure (Right (StreamRelation attrsA tupS'))
    EqualTupleStreamsPlan exprA exprB () _ -> do
      checkCacheOr $ do
        ePlanA <- executePlan exprA ctxTuples gfEnv cacheKeyBlackList cache
        ePlanB <- executePlan exprB ctxTuples gfEnv cacheKeyBlackList cache
        case ePlanA of
          Left err -> pure (Left err)
          Right (StreamRelation _ tupSa) ->
            case ePlanB of
              Left err -> pure (Left err)
              Right (StreamRelation _ tupSb) -> do
                let hsA = tuplesHashSet tupSa
                    hscmp = tuplesHashSet (Stream.parList id [tupSa, tupSb])
                    tupS' = SD.unCross $ do
                      tA <- liftIO hsA
                      tcmp <- liftIO hscmp
                      SD.mkCross $ Stream.fromList $ 
                        [RelationTuple mempty mempty | HS.size tA == HS.size tcmp]
                pure (Right (StreamRelation mempty tupS'))
    RelationValuedAttributeStreamPlan relAttr () _ -> checkCacheOr $ do
      case contextTupleAtomForAttributeName ctxTuples (A.attributeName relAttr) of
        Left err -> pure (Left err)
        Right relAtom@(RelationAtom{}) -> do
          let newTup = RelationTuple (A.singleton relAttr) (V.singleton relAtom)
              tupS = Stream.fromList [newTup]
          pure (Right (StreamRelation (A.singleton relAttr) tupS))
        Right _ -> pure (Left (AttributeIsNotRelationValuedError (A.attributeName relAttr)))
    NotEqualTupleStreamsPlan exprA exprB () orig -> checkCacheOr $ do
      eS <- executePlan (EqualTupleStreamsPlan exprA exprB () orig) ctxTuples gfEnv cacheKeyBlackList cache
      case eS of
        Left err -> pure (Left err)
        Right (StreamRelation _ tupS) -> do
          let tupS' = SD.unCross $ do
                el <- liftIO $ SD.head tupS
                SD.mkCross $ Stream.fromList $ case el of
                                                 Nothing -> [RelationTuple mempty mempty]
                                                 Just _ -> []
          pure (Right (StreamRelation mempty tupS'))
    MakeStaticRelationPlan attrs tupSet () -> checkCacheOr $ do
      pure (Right (StreamRelation attrs (Stream.fromList (asList tupSet))))
    MakeRelationFromExprsPlan mAttrExprs tupExprs _ -> checkCacheOr $ do
      let expr = MakeRelationFromExprs mAttrExprs tupExprs
      case runGraphRefRelationalExprM gfEnv (evalGraphRefRelationalExpr expr) of
        Left err -> pure (Left err)
        Right rel ->
          pure (Right (StreamRelation (attributes rel) (Stream.fromList (asList (tupleSet rel)))))
    ExistingRelationPlan rel () -> do
      pure (Right (StreamRelation (attributes rel) (Stream.fromList (asList (tupleSet rel)))))
    ExtendTupleStreamPlan (newAttrs, extendProcessor) _extendExpr expr () _ -> checkCacheOr $ do
      eRelS <- executePlan expr ctxTuples gfEnv cacheKeyBlackList cache
      case eRelS of
        Left err -> pure (Left err)
        Right relS -> do
          let extender tup =
                case extendProcessor tup ctxTuples of
                  Left err -> throw err
                  Right t' -> pure t'
          pure (Right (streamRelationMap newAttrs extender relS))
    NaiveJoinTupleStreamsPlan exprA exprB () _ -> checkCacheOr $ do
      --naive join by scanning both exprB into a list for repeated O(n^2) scans which is fine for "small" tables
      eSA <- executePlan exprA ctxTuples gfEnv cacheKeyBlackList cache
      eSB <- executePlan exprB ctxTuples gfEnv cacheKeyBlackList cache
      case eSA of
        Left err -> pure (Left err)
        Right (StreamRelation attrsA tupSa) ->
          case eSB of
            Left err -> pure (Left err)
            Right (StreamRelation attrsB tupSb) -> do
              case joinAttributes attrsA attrsB of
                Left err -> pure (Left err)
                Right attrsOut -> do
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
                  pure (Right (StreamRelation attrsOut tupS'))
                  
    DifferenceTupleStreamsPlan exprA exprB () _ -> checkCacheOr $ do
      eSA <- executePlan exprA ctxTuples gfEnv cacheKeyBlackList cache
      eSB <- executePlan exprB ctxTuples gfEnv cacheKeyBlackList cache
      case eSA of
        Left err -> pure (Left err)
        Right (StreamRelation attrsA tupSa) ->
          case eSB of
            Left err -> pure (Left err)
            Right (StreamRelation _ tupSb) -> do
              let tupS' = SD.unCross $ do
                    bTupleList <- liftIO $ Stream.toList tupSb        
                    SD.mkCross $ Stream.filter (`notElem` bTupleList) tupSa
              pure (Right (StreamRelation attrsA tupS'))
    GroupTupleStreamPlan groupAttrs newAttrName expr () orig -> checkCacheOr $ do
      --naive implementation scans for image relation for each grouped value
      eS <- executePlan expr ctxTuples gfEnv cacheKeyBlackList cache
      case eS of
        Left err -> pure (Left err)
        Right (StreamRelation attrsIn tupS) -> do
          let nonGroupAttrNames = nonMatchingAttributeNameSet groupAttrNames (A.attributeNameSet attrsIn)
              groupAttrNames = A.attributeNameSet groupAttrs
          case projectionAttributesForNames nonGroupAttrNames attrsIn of
            Left err -> pure (Left err)
            Right nonGroupProjectionAttributes -> do
              case projectionAttributesForNames groupAttrNames attrsIn of
                Left err -> pure (Left err)
                Right groupProjectionAttributes -> do
                  eGroupS <- executePlan (UniqueifyTupleStreamPlan (ProjectTupleStreamPlan nonGroupProjectionAttributes expr () orig) ()) ctxTuples gfEnv cacheKeyBlackList cache
                  case eGroupS of
                    Left err -> pure (Left err)
                    Right (StreamRelation _ nonGroupProjectionTupS) -> do
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
                      pure (Right (StreamRelation outAttrs tupS'))
    UngroupTupleStreamPlan groupAttrName expr () _ -> checkCacheOr $ do
      eS <- executePlan expr ctxTuples gfEnv cacheKeyBlackList cache
      case eS of
        Left err -> pure (Left err)
        Right (StreamRelation attrsIn tupS) -> do
          case atomTypeForAttributeName groupAttrName attrsIn of
            Right (RelationAtomType subRelAttrs) -> do
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
              pure (Right (StreamRelation outAttrs (Stream.concatMap ungroup' tupS)))
            _ -> pure (Left (AttributeIsNotRelationValuedError groupAttrName))
    UniqueifyTupleStreamPlan e () -> checkCacheOr $ do
      eS <- executePlan e ctxTuples gfEnv cacheKeyBlackList cache
      case eS of
        Left err -> pure (Left err)
        Right (StreamRelation attrs tupS) -> do
          let tupS' = SD.unCross $ do
                uniqTups <- liftIO $ tuplesHashSet tupS
                SD.mkCross $ StreamK.toStream (StreamK.fromFoldable uniqTups)
          pure (Right (StreamRelation attrs tupS'))
{-    AlternativePlan first remainder () -> do
      -- catch cache miss exception- the cache entry may have been deleted
      let initOptions = first : NE.init remainder
          lastOption = NE.last remainder -- if the last option throws an exception, just bubble it up
      --convert to streamly
          runOptions :: (MonadIO m, MonadAsync m) => [GraphRefRelExprExecPlan] -> GraphRefRelExprExecPlan -> StreamRelation m
          runOptions (fstStream:remStream) finalStream =
            Stream.handle (\(_e :: CacheMissException) -> runOptions remStream finalStream) fstStream -- we may have other alternative-triggering exceptions in the future
          runOptions [] finalStream = finalStream
      initStreams <- mapM ((flip executePlan) ctx) initOptions
      finalStream <- executePlan lastOption ctx
      pure $ runOptions initStreams finalStream
  -}                         

relationTrue :: (MonadIO m, Stream.MonadAsync m) => StreamRelation m
relationTrue = StreamRelation mempty (Stream.fromList [RelationTuple mempty mempty])

relationFalse :: (MonadIO m, Stream.MonadAsync m) => StreamRelation m
relationFalse = StreamRelation mempty (Stream.fromList [])

tuplesHashSet :: MonadIO m => Stream m RelationTuple -> m (HS.HashSet RelationTuple)
tuplesHashSet =
  Stream.foldr HS.insert mempty

instance Pretty Attributes where
  pretty attrs = encloseSep "{" "}" "," (map pretty (V.toList (attributesVec attrs)))

instance Pretty RelationTupleSet where
  pretty tupSet = vsep (map pretty (asList tupSet))

instance Pretty RenameAssocs where
  pretty renameSet = hsep (map pretty (S.toList renameSet))

instance Pretty Attribute where
  pretty attr = pretty (attributeName attr) <+> pretty (atomType attr)

instance Pretty RelationTuple where
  pretty tup = pretty (tupleAttributes tup) <+> vsep (map (indent 4 . pretty) (V.toList (tupleAtoms tup)))

instance Pretty a => Pretty (AttributeExprBase a) where
  pretty (AttributeAndTypeNameExpr attrName tCons a) =
    pretty attrName <+> pretty tCons <+> pretty a
  pretty (NakedAttributeExpr attrName) = pretty attrName

instance Pretty a => Pretty (TupleExprsBase a) where
  pretty (TupleExprs a tupleExprs) =
    pretty a <+> vsep (map pretty tupleExprs)

instance Pretty a => Pretty (TupleExprBase a) where
  pretty (TupleExpr attrAtomMap) =
    "TupleExpr" <+> indent 4 (hsep (map prettyMap (M.toList attrAtomMap)))
    where
      prettyMap (attrName, atomExpr) =
        pretty attrName <> ": " <> pretty atomExpr

instance Pretty a => Pretty (AtomExprBase a) where
  pretty atomExpr =
    case atomExpr of
      AttributeAtomExpr attrName ->
        "AttributeAtomExpr" <+> pretty attrName
      SubrelationAttributeAtomExpr attrName subAttrName ->
        "SubrelationAttributeAtomExpr" <+> pretty attrName <+> pretty subAttrName
      NakedAtomExpr atom ->
        "NakedAtomExpr" <+> pretty atom
      FunctionAtomExpr fname fargs marker ->
        "FunctionAtomExpr" <+> pretty fname <+> parens (pretty fargs) <+> pretty marker
      RelationAtomExpr relExpr ->
        "RelationAtomExpr" <+> pretty relExpr
      IfThenAtomExpr if' then' else' ->
        "IfThenAtomExpr" <+> pretty if' <+> pretty then' <+> pretty else'
      ConstructedAtomExpr dConsName atomExprs marker ->
        "ConstructedAtomExpr" <+> pretty dConsName <+> pretty atomExprs <+> pretty marker

instance Pretty a => Pretty (RelationalExprBase a) where
  pretty relExpr =
    case relExpr of
      MakeRelationFromExprs mAttrExprs tupExprs ->
        "MakeRelationFromExprs" <+> pretty mAttrExprs <+> pretty tupExprs
      MakeStaticRelation attrs tupSet ->
        "MakeStaticRelation" <+> pretty attrs <+> pretty tupSet
      ExistingRelation _rel ->
        "ExistingRelation"
      RelationVariable rvName marker ->
        "RelationVariable" <+> pretty rvName <+> pretty marker
      RelationValuedAttribute attrName ->
        "RelationValuedAttribute" <+> pretty attrName
      Project attrNames expr ->
        "Project" <+> pretty attrNames <+> pretty expr
      Union exprA exprB ->
        "Union" <+> pretty exprA <+> pretty exprB
      Join exprA exprB ->
        "Join" <+> pretty exprA <+> pretty exprB
      Rename renames expr ->
        "Rename" <+> pretty renames <+> pretty expr
      Difference exprA exprB ->
        "Difference" <+> pretty exprA <+> pretty exprB
      Group attrNames groupAttr expr ->
        "Group" <+> pretty attrNames <+> pretty groupAttr <+> pretty expr
      Ungroup attrName expr ->
        "Ungroup" <+> pretty attrName <+> pretty expr
      Restrict predExpr expr ->
        "Restrict" <+> pretty predExpr <+> pretty expr
      Equals exprA exprB ->
        "Equals" <+> pretty exprA <+> pretty exprB
      NotEquals exprA exprB ->
        "NotEquals" <+> pretty exprA <+> pretty exprB
      Extend extendExpr expr ->
        "Extend" <+> pretty extendExpr <+> pretty expr
      With withs expr ->
        "With" <+> pretty withs <+> pretty expr

instance Pretty a => Pretty (ExtendTupleExprBase a) where
  pretty (AttributeExtendTupleExpr attrName atomExpr) =
    pretty attrName <+> pretty atomExpr
  
instance Pretty a => Pretty (WithNameExprBase a) where
  pretty (WithNameExpr rvName marker) = "WithNameExpr" <+> pretty rvName <+> pretty marker

instance Pretty a => Pretty (AttributeNamesBase a) where
  pretty (AttributeNames attrNameSet) = "AttributeNames" <+> vsep (map pretty (S.toList attrNameSet))
  pretty (InvertedAttributeNames attrNameSet) = "InvertedAttributeNames" <+> vsep (map pretty (S.toList attrNameSet))
  pretty (UnionAttributeNames namesA namesB) =
    "UnionAttributeNames" <+> parens (pretty namesA) <+> parens (pretty namesB)
  pretty (IntersectAttributeNames namesA namesB) =
    "IntersectAttributeNames" <+> parens (pretty namesA) <+> parens (pretty namesB)
  pretty (RelationalExprAttributeNames expr) =
    "RelationalExprAttributeNames" <+> pretty expr
                                                                                  

instance Pretty TypeConstructor where
  pretty (ADTypeConstructor tConsName []) =
    pretty tConsName
  pretty (ADTypeConstructor tConsName tArgs) =
    parens (pretty tConsName <+> vsep (map pretty tArgs))
  pretty (PrimitiveTypeConstructor tConsName _) =
    pretty tConsName
  pretty (RelationAtomTypeConstructor attrExprs) =
    parens (vsep (map pretty attrExprs))
  pretty (TypeVariable tVarName) =
    pretty tVarName
  
instance Pretty AtomType where
  pretty aType = pretty (show aType)

instance Pretty Atom where
  pretty atom = pretty (show atom)

instance Pretty a => Pretty (RestrictionPredicateExprBase a) where
  pretty x =
    case x of
      TruePredicate -> "True"
      AndPredicate exprA exprB ->
        "And" <+> parens (pretty exprA) <+> parens (pretty exprB)
      OrPredicate exprA exprB ->
        "Or" <+> parens (pretty exprA) <+> parens (pretty exprB)
      NotPredicate expr ->
        "Not" <+> parens (pretty expr)
      RelationalExprPredicate expr ->
        "RelationalExpr" <+> pretty expr
      AtomExprPredicate atomExpr ->
        "AtomExpr" <+> pretty atomExpr
      AttributeEqualityPredicate attrName atomExpr ->
        "AttributeEquality" <+> pretty attrName <+> pretty atomExpr

--instance Pretty a => Pretty (ExtendTupleExprBase a) where
  

instance Pretty GraphRefTransactionMarker where
  pretty (TransactionMarker tid) = "TransactionId" <+> pretty (U.toText tid)
  pretty UncommittedContextMarker = "Uncomitted"

renderPretty :: (Show a, Pretty t, Pretty a) => RelExprExecPlanBase a t -> T.Text
renderPretty = renderStrict . layoutPretty defaultLayoutOptions . pretty

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
--import qualified Streamly.Internal.Data.Stream as Stream
import Streamly.Internal.Control.Concurrent (MonadAsync)
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
data RelExprExecPlanBase a t =
  -- | Read relvar expr from transaction graph to generate tuple stream.
  -- Instead of locking cache entries during plans so that the entry is not ejected before the plan can run (race condition), the plan can point to a cache entry, but if the entry no longer exists, then the plan must provide an alternative solution.

--  ReadExprFromTransGraph RelVarName a |
  -- | Read tuples from a tuple set cache.
  StreamTuplesFromCacheFilePlan Attributes FilePath t |
  -- | Read tuples from memory.
  ReadTuplesFromMemoryPlan Attributes RelationTupleSet t |
  -- | Alternative plans in case of failure. For example, the first node in the list may be to read from a cache file but the cache has since deleted the entry, so we proceed with an alternative. This is not a node to use for otherwise fatal errors.
--  AlternativePlan (RelExprExecPlanBase a t) (NE.NonEmpty (RelExprExecPlanBase a t)) t |
  -- | Run all the nodes simultaneously and return the results from the node that returns results first.
  -- RacePlan (RelExprExecPlanBase a t) (NE.NonEmpty (RelExprExecPlanBase a t)) t |
                       
                       RestrictTupleStreamPlan RestrictionFilter (RestrictionPredicateExprBase a) (RelExprExecPlanBase a t) t (RelationalExprBase a) | -- include compiled mode for stream execution and ADT version for planning printer
                       ProjectTupleStreamPlan Attributes (RelExprExecPlanBase a t) t (RelationalExprBase a) |
                       RenameTupleStreamPlan (S.Set (AttributeName, AttributeName)) (RelExprExecPlanBase a t) t (RelationalExprBase a) |
                       GroupTupleStreamPlan Attributes AttributeName (RelExprExecPlanBase a t) t (RelationalExprBase a) |
                       UngroupTupleStreamPlan AttributeName (RelExprExecPlanBase a t) t (RelationalExprBase a) |
                       ExtendTupleStreamPlan ExtendTupleProcessor (ExtendTupleExprBase a) (RelExprExecPlanBase a t) t (RelationalExprBase a) |
                       RelationValuedAttributeStreamPlan Attribute t (RelationalExprBase a) | 
                       
                       UnionTupleStreamsPlan (RelExprExecPlanBase a t) (RelExprExecPlanBase a t) t (RelationalExprBase a) | -- ^ no uniquification implied with Union, if it's needed, planner needs to add it
                       NaiveJoinTupleStreamsPlan (RelExprExecPlanBase a t) (RelExprExecPlanBase a t) t (RelationalExprBase a) |
                       DifferenceTupleStreamsPlan (RelExprExecPlanBase a t) (RelExprExecPlanBase a t) t (RelationalExprBase a) |
                       EqualTupleStreamsPlan (RelExprExecPlanBase a t) (RelExprExecPlanBase a t) t (RelationalExprBase a) |
                       NotEqualTupleStreamsPlan (RelExprExecPlanBase a t) (RelExprExecPlanBase a t) t (RelationalExprBase a) |
                       
                       MakeStaticRelationPlan Attributes RelationTupleSet t |
                       -- TODO: ideally, the planner would create a new structure which extracts all necessary context so that we don't need to pass the entire transaction graph to execute MakeRelationFromExprsPlan
                       MakeRelationFromExprsPlan (Maybe [AttributeExprBase a]) (TupleExprsBase a) t |
                       ExistingRelationPlan Relation t |
                       UniqueifyTupleStreamPlan (RelExprExecPlanBase a t) t -- ^ handy uniqueifier for cases when we know the sub-plan could include duplicates

-- | Get the original relational expression linked to the node. It can be used to generate the cache key, if relevant.
originalRelExpr :: RelExprExecPlanBase a t -> Maybe (RelationalExprBase a)
originalRelExpr StreamTuplesFromCacheFilePlan{} = Nothing
originalRelExpr ReadTuplesFromMemoryPlan{} = Nothing
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
      StreamTuplesFromCacheFilePlan attrs path t ->
        prettyNode "StreamTuplesFromCacheFilePlan" [pretty attrs, pretty path] [] t
      ReadTuplesFromMemoryPlan attrs _tupSet t ->
        prettyNode "ReadTuplesFromMemoryPlan" [pretty attrs, "<tuples elided"] [] t
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
  
--until we can stream results to disk or socket, we return a lazy-list-based Relation
-- | Build a tuple stream from an execution plan to enable parallel execution.
executePlan :: (MonadIO m, MonadAsync m) => GraphRefRelExprExecPlan -> ContextTuples -> GraphRefRelationalExprEnv -> Either RelationalError (StreamRelation m)
executePlan (ReadTuplesFromMemoryPlan attrs tupSet ()) _ _ =
  pure $ StreamRelation attrs (Stream.fromList (asList tupSet))
executePlan (StreamTuplesFromCacheFilePlan{}) _ _ =
  --todo: enable streaming tuples from file
  undefined
executePlan (RenameTupleStreamPlan attrsAssoc expr () _) ctx gfEnv = do
  relS <- executePlan expr ctx gfEnv
  let newAttrs = renameAttributes' attrsAssoc (sRelAttributes relS)
  --potential optimization- lookup attrs in advance to rename the correct vector index
  pure $ streamRelationMap newAttrs (pure . tupleRenameAttributes attrsAssoc) relS
executePlan (RestrictTupleStreamPlan restrictionFilter _predExpr expr () _origExpr) ctx gfEnv = do
  (StreamRelation attrs tupS) <- executePlan expr ctx gfEnv
  let tupS' = Stream.filterM filt tupS
      -- since we are building up a stream data structure, we can represent in-stream failure using exceptions- we won't be able to execute the stream here to extract errors
      filt t =
        pure $ case restrictionFilter t ctx of
        Left err -> throw err -- this will blow up in a separate thread but streamly should shuttle it to the caller (I hope)
        Right !t' -> t'
  pure $ StreamRelation attrs tupS'
executePlan (ProjectTupleStreamPlan attrs expr () _) ctx gfEnv = do
  (StreamRelation _ tupS) <- executePlan expr ctx gfEnv
  let tupS' = fmap projector tupS
      --optimize by projecting on vector indexes instead
      projector t = case tupleProject attrs t of
                      Left err -> throw err
                      Right t' -> t'
  pure $ StreamRelation attrs tupS'
executePlan (UnionTupleStreamsPlan exprA exprB () _) ctx gfEnv = do
  --ideally, the streams would have pre-ordered tuples and we could zip them together right away- if we have an ordered representation, we can drop the sorting here
  -- glue two streams together, then uniqueify
  (StreamRelation attrsA tupSa) <- executePlan exprA ctx gfEnv
  (StreamRelation _ tupSb) <- executePlan exprB ctx gfEnv
  let tupS' = Stream.parList id [tupSa, tupSb]
  pure (StreamRelation attrsA tupS')
executePlan (EqualTupleStreamsPlan exprA exprB () _) ctx gfEnv = do
  (StreamRelation _ tupSa) <- executePlan exprA ctx gfEnv
  (StreamRelation _ tupSb) <- executePlan exprB ctx gfEnv
  let hsA = tuplesHashSet tupSa
      hscmp = tuplesHashSet (Stream.parList id [tupSa, tupSb])
      tupS' = SD.unCross $ do
        tA <- liftIO hsA
        tcmp <- liftIO hscmp
        SD.mkCross $ Stream.fromList $ 
          [RelationTuple mempty mempty | HS.size tA == HS.size tcmp]
  pure (StreamRelation mempty tupS')
executePlan (RelationValuedAttributeStreamPlan relAttr () _) ctx _ = do
  case contextTupleAtomForAttributeName ctx (A.attributeName relAttr) of
    Left err -> Left err
    Right relAtom@(RelationAtom{}) -> do
      let newTup = RelationTuple (A.singleton relAttr) (V.singleton relAtom)
          tupS = Stream.fromList [newTup]
      pure (StreamRelation (A.singleton relAttr) tupS)
    Right _ -> Left (AttributeIsNotRelationValuedError (A.attributeName relAttr))
executePlan (NotEqualTupleStreamsPlan exprA exprB () orig) ctx gfEnv = do
  (StreamRelation _ tupS) <- executePlan (EqualTupleStreamsPlan exprA exprB () orig) ctx gfEnv
  let tupS' = SD.unCross $ do
        el <- liftIO $ SD.head tupS
        SD.mkCross $ Stream.fromList $ case el of
                            Nothing -> [RelationTuple mempty mempty]
                            Just _ -> []
  pure (StreamRelation mempty tupS')
executePlan (MakeStaticRelationPlan attrs tupSet ()) _ _ = do
  pure (StreamRelation attrs (Stream.fromList (asList tupSet)))
executePlan (MakeRelationFromExprsPlan mAttrExprs tupExprs _) _ctx gfEnv = do
  let expr = MakeRelationFromExprs mAttrExprs tupExprs
  rel <- runGraphRefRelationalExprM gfEnv (evalGraphRefRelationalExpr expr)
  pure (StreamRelation (attributes rel) (Stream.fromList (asList (tupleSet rel))))
executePlan (ExistingRelationPlan rel ()) _ _ = do
  pure (StreamRelation (attributes rel) (Stream.fromList (asList (tupleSet rel))))
executePlan (ExtendTupleStreamPlan (newAttrs, extendProcessor) _extendExpr expr () _) ctx gfEnv = do
  relS <- executePlan expr ctx gfEnv
  let extender tup =
        case extendProcessor tup ctx of
          Left err -> throw err
          Right t' -> pure t'
  pure (streamRelationMap newAttrs extender relS)
executePlan (NaiveJoinTupleStreamsPlan exprA exprB () _) ctx gfEnv = do
  --naive join by scanning both exprB into a list for repeated O(n^2) scans which is fine for "small" tables
  (StreamRelation attrsA tupSa) <- executePlan exprA ctx gfEnv
  (StreamRelation attrsB tupSb) <- executePlan exprB ctx gfEnv
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
executePlan (DifferenceTupleStreamsPlan exprA exprB () _) ctx gfEnv = do
  (StreamRelation attrsA tupSa) <- executePlan exprA ctx gfEnv
  (StreamRelation _ tupSb) <- executePlan exprB ctx gfEnv
  let tupS' = SD.unCross $ do
        bTupleList <- liftIO $ Stream.toList tupSb        
        SD.mkCross $ Stream.filter (`notElem` bTupleList) tupSa
  pure (StreamRelation attrsA tupS')
executePlan (GroupTupleStreamPlan groupAttrs newAttrName expr () orig) ctx gfEnv = do
  --naive implementation scans for image relation for each grouped value
  (StreamRelation attrsIn tupS) <- executePlan expr ctx gfEnv
  let nonGroupAttrNames = nonMatchingAttributeNameSet groupAttrNames (A.attributeNameSet attrsIn)
      groupAttrNames = A.attributeNameSet groupAttrs
  nonGroupProjectionAttributes <- projectionAttributesForNames nonGroupAttrNames attrsIn
  groupProjectionAttributes <- projectionAttributesForNames groupAttrNames attrsIn
  (StreamRelation _ nonGroupProjectionTupS) <- executePlan (ProjectTupleStreamPlan nonGroupProjectionAttributes expr () orig) ctx gfEnv
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
executePlan (UngroupTupleStreamPlan groupAttrName expr () _) ctx gfEnv = do
  (StreamRelation attrsIn tupS) <- executePlan expr ctx gfEnv
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
executePlan (UniqueifyTupleStreamPlan e ()) ctx gfEnv = do
  (StreamRelation attrs tupS) <- executePlan e ctx gfEnv
  let tupS' = SD.unCross $ do
        uniqTups <- liftIO $ tuplesHashSet tupS
        SD.mkCross $ StreamK.toStream (StreamK.fromFoldable uniqTups)
  pure (StreamRelation attrs tupS')
{-executePlan (AlternativePlan first remainder ()) ctx = do
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

executePlanWithCache :: (MonadIO m, MonadAsync m) => GraphRefRelExprExecPlan -> ContextTuples -> GraphRefRelationalExprEnv -> RelExprCache -> m (Either RelationalError (StreamRelation m))
executePlanWithCache = executePlanWithCache' HS.empty

executePlanWithCache' :: (MonadIO m, MonadAsync m) => HS.HashSet PinnedRelationalExpr -> GraphRefRelExprExecPlan -> ContextTuples -> GraphRefRelationalExprEnv -> RelExprCache -> m (Either RelationalError (StreamRelation m))
executePlanWithCache' cacheKeyBlackList origPlan ctxTuples gfEnv cache = do
  let uncachedExec = pure $ executePlan origPlan ctxTuples gfEnv
--  traceShowM ("origPlan"::String, renderPretty origPlan)
  case originalRelExpr origPlan of
    Nothing -> uncachedExec
    Just origRelExpr -> do
      case toPinnedRelationalExpr origRelExpr of -- check that we can generate a key for the cache
        Nothing -> uncachedExec
        Just key -> do
          mCachedVal <- if key `HS.member` cacheKeyBlackList then -- the blacklist prevents infinite recursion into the cache
                          pure Nothing
                        else do
                          liftIO $ atomically $ RECache.lookup key cache
          case mCachedVal of -- check if the key is in the cache
            Just cacheInfo ->
              case result cacheInfo of
                PinnedExpressionRep pinnedRelExpr -> do
                  -- plan and execute alternative, cached expression which is equivalent to the results of the origPlan
                  let eNewPlan = planGraphRefRelationalExpr (toGraphRefRelationalExpr pinnedRelExpr) (freshGraphRefRelationalExprEnv Nothing emptyTransactionGraph)
                  case eNewPlan of
                    Left err -> pure (Left err)
                    Right newPlan -> do
                      executePlanWithCache' (HS.insert key cacheKeyBlackList) newPlan ctxTuples gfEnv cache --prevent infinite loop using key blacklist
                UnsortedTupleSetRep attrs tupSet -> do
                  pure (Right (StreamRelation attrs (Stream.fromList (asList tupSet))))
                SortedTuplesRep _tupList _sortInfo -> error "sortedtupsrep"
            Nothing ->
              uncachedExec
              -- pipe result to cache or maybe pipe just the expensive parts to the cache. but then we need to track which parts of the pipeline are actually expensive
              -- we know this entry is not in the cache, so we want to add it, but we don't want to add the plan, but the mostly evaluated expression, which we don't have here.

      
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

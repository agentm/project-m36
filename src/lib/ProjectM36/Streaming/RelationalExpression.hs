{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, BangPatterns, FlexibleInstances, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProjectM36.Streaming.RelationalExpression where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.RelationalExpression
import ProjectM36.Attribute as A
import ProjectM36.Tuple
import ProjectM36.DatabaseContext
import ProjectM36.Relation (RestrictionFilter, ContextTuples, attributes, contextTupleAtomForAttributeName, tupleSet, singletonContextTuple)
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
import Optics.Core

data CacheMissException = CacheMissException FilePath
  deriving Show

instance Exception CacheMissException

type RelExprExecPlan = RelExprExecPlanBase () ()

type GraphRefRelExprExecPlan = RelExprExecPlanBase GraphRefTransactionMarker ()

type PostExecutionGraphRefRelExprExecPlan = RelExprExecPlanBase GraphRefTransactionMarker PlanNodeExecutionInfo

newtype PlanNodeExecutionInfo = PlanNodeExecutionInfo { duration :: DiffTime }
  
--this will become more useful once we have multiple join strategies, etc.
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
                       
                       RestrictTupleStreamPlan RestrictionFilter (RestrictionPredicateExprBase a) (RelExprExecPlanBase a t) t | -- include compiled mode for stream execution and ADT version for planning printer
                       ProjectTupleStreamPlan Attributes (RelExprExecPlanBase a t) t |
                       RenameTupleStreamPlan (S.Set (AttributeName, AttributeName)) (RelExprExecPlanBase a t) t|
                       GroupTupleStreamPlan Attributes AttributeName (RelExprExecPlanBase a t) t |
                       UngroupTupleStreamPlan AttributeName (RelExprExecPlanBase a t) t |
                       ExtendTupleStreamPlan ExtendTupleProcessor (ExtendTupleExprBase a) (RelExprExecPlanBase a t) t |
                       RelationValuedAttributeStreamPlan Attribute t | 
                       
                       UnionTupleStreamsPlan (RelExprExecPlanBase a t) (RelExprExecPlanBase a t) t | -- ^ no uniquification implied with Union, if it's needed, planner needs to add it
                       NaiveJoinTupleStreamsPlan (RelExprExecPlanBase a t) (RelExprExecPlanBase a t) t |
                       DifferenceTupleStreamsPlan (RelExprExecPlanBase a t) (RelExprExecPlanBase a t) t |
                       EqualTupleStreamsPlan (RelExprExecPlanBase a t) (RelExprExecPlanBase a t) t |
                       NotEqualTupleStreamsPlan (RelExprExecPlanBase a t) (RelExprExecPlanBase a t) t |
                       
                       MakeStaticRelationPlan Attributes RelationTupleSet t |
                       ExistingRelationPlan Relation t |
                       UniqueifyTupleStreamPlan (RelExprExecPlanBase a t) t -- ^ handy uniqueifier for cases when we know the sub-plan could include duplicates

{-
junction :: T.Text
junction = "\x251c\x2500"

horizontalLine :: T.Text
horizontalLine = "\x2502"
-}

-- | Create a plan Doc to show the user.
instance (Pretty t, Show a) => Pretty (RelExprExecPlanBase a t) where
  pretty expr =
    case expr of
      StreamTuplesFromCacheFilePlan attrs path t ->
        prettyNode "StreamTuplesFromCacheFilePlan" [pretty attrs, pretty path] [] t
      ReadTuplesFromMemoryPlan attrs _tupSet t ->
        prettyNode "ReadTuplesFromMemoryPlan" [pretty attrs, "<tuples elided"] [] t
      RestrictTupleStreamPlan _ predExpr expr' t ->
        prettyNode "RestrictTupleStreamPlan" [pretty predExpr] [pretty expr'] t
      ProjectTupleStreamPlan attrs expr' t ->
        prettyNode "ProjectTupleStreamPlan " [pretty attrs] [pretty expr'] t
      RenameTupleStreamPlan attrs expr' t ->
        prettyNode "RenameTupleStreamPlan" [pretty attrs] [pretty expr'] t
      GroupTupleStreamPlan attrs nam expr' t ->
        prettyNode "GroupTupleStreamPlan" [pretty attrs <+> "as" <+> pretty nam] [pretty expr'] t
      UngroupTupleStreamPlan nam expr' t ->
        prettyNode "UngroupTupleStreamPlan" [pretty nam] [pretty expr'] t
      ExtendTupleStreamPlan _ extender expr' t ->
        prettyNode "ExtendTupleStreamPlan" [pretty extender] [pretty expr'] t
      RelationValuedAttributeStreamPlan relAttr t ->
        prettyNode "RelationValuedAttributeStreamPlan" [pretty relAttr] [] t
      UnionTupleStreamsPlan e1 e2 t ->
        prettyNode "UnionTupleStreamsPlan" [] [pretty e1, pretty e2] t
      NaiveJoinTupleStreamsPlan e1 e2 t ->
        prettyNode "NaiveJoinTupleStreamsPlan" [] [pretty e1, pretty e2] t
      DifferenceTupleStreamsPlan e1 e2 t ->
        prettyNode "DifferenceTupleStreamsPlan" [] [pretty e1, pretty e2] t
      EqualTupleStreamsPlan e1 e2 t ->
        prettyNode "EqualTupleStreamsPlan" [] [pretty e1, pretty e2] t
      NotEqualTupleStreamsPlan e1 e2 t ->
        prettyNode "NotEqualTupleStreamsPlan" [] [pretty e1, pretty e2] t
      MakeStaticRelationPlan attrs _tupSet t ->
        prettyNode "MakeStaticRelationPlan" [pretty attrs] ["<tuples elided>"] t
      ExistingRelationPlan rel t ->
        prettyNode "ExistingRelationPlan" [pretty (attributes rel)] ["<tuples elided>"] t
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



parensList :: [Doc ann] -> Doc ann
parensList = group . encloseSep (flatAlt "( " "(") (flatAlt " )" ")") ", "
      
--todo- identify nodes which need uniqueifying
planGraphRefRelationalExpr :: GraphRefRelationalExpr -> 
                              GraphRefRelationalExprEnv -> 
                              Either RelationalError GraphRefRelExprExecPlan
planGraphRefRelationalExpr (RelationVariable name tid) gfEnv = do
  ctx <- runGraphRefRelationalExprM gfEnv (gfDatabaseContextForMarker tid)
  let rvMap = ctx ^. relationVariables
  case M.lookup name rvMap of
    Nothing -> Left (RelVarNotDefinedError name)
    Just rvExpr -> planGraphRefRelationalExpr rvExpr gfEnv
planGraphRefRelationalExpr (RelationValuedAttribute relAttrName) gfEnv = do
  case A.attributeForName relAttrName (envAttributes gfEnv) of
    Left err -> throw err
    Right relAttr -> 
      pure (RelationValuedAttributeStreamPlan relAttr ())
  
planGraphRefRelationalExpr (Project attrNames expr) gfEnv = do
  exprT <- runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr expr)
  projectionAttrNames <- runGraphRefRelationalExprM gfEnv (evalGraphRefAttributeNames attrNames expr)
  case projectionAttributesForNames projectionAttrNames (attributes exprT) of
    Left err -> Left err
    Right attrs' -> do
      subExpr <- planGraphRefRelationalExpr expr gfEnv
      --if we know that the projection attributes represent a key, then we don't need to run the uniquification
      pure (UniqueifyTupleStreamPlan (ProjectTupleStreamPlan attrs' subExpr ()) ())
  
planGraphRefRelationalExpr (Union exprA exprB) state = do  
  planA <- planGraphRefRelationalExpr exprA state
  planB <- planGraphRefRelationalExpr exprB state
  pure (UnionTupleStreamsPlan planA planB ())
  
planGraphRefRelationalExpr (Join exprA exprB) state = do  
  planA <- planGraphRefRelationalExpr exprA state
  planB <- planGraphRefRelationalExpr exprB state
  pure (NaiveJoinTupleStreamsPlan planA planB ())
  
planGraphRefRelationalExpr (Difference exprA exprB) state = do
  planA <- planGraphRefRelationalExpr exprA state
  planB <- planGraphRefRelationalExpr exprB state
  pure (DifferenceTupleStreamsPlan planA planB ())

planGraphRefRelationalExpr (MakeStaticRelation attributeSet tupSet) _ = pure (MakeStaticRelationPlan attributeSet tupSet ())

planGraphRefRelationalExpr expr@MakeRelationFromExprs{} gfEnv = do
  rel <- runGraphRefRelationalExprM gfEnv (evalGraphRefRelationalExpr expr)
  pure (ExistingRelationPlan rel ())
  
planGraphRefRelationalExpr (ExistingRelation rel) _ = pure (ExistingRelationPlan rel ())  

planGraphRefRelationalExpr (Rename renameAssoc relExpr) state = 
  RenameTupleStreamPlan renameAssoc <$> planGraphRefRelationalExpr relExpr state <*> pure ()
  
planGraphRefRelationalExpr (Group groupAttrNames newAttrName relExpr) gfEnv = do
  groupAttrs <- runGraphRefRelationalExprM gfEnv $ do
    groupTypes <- typeForGraphRefRelationalExpr (Project groupAttrNames relExpr)
    pure (attributes groupTypes)
  GroupTupleStreamPlan groupAttrs newAttrName <$> planGraphRefRelationalExpr relExpr gfEnv <*> pure ()
  
planGraphRefRelationalExpr (Ungroup attrName relExpr) state = 
  UngroupTupleStreamPlan attrName <$> planGraphRefRelationalExpr relExpr state <*> pure ()
  
planGraphRefRelationalExpr (Restrict predExpr relExpr) gfEnv = do
  rfilt <- runGraphRefRelationalExprM gfEnv $ do
    exprT <- typeForGraphRefRelationalExpr relExpr    
    predicateRestrictionFilter (attributes exprT) predExpr
  RestrictTupleStreamPlan rfilt predExpr <$> planGraphRefRelationalExpr relExpr gfEnv <*> pure ()
  
planGraphRefRelationalExpr (Equals relExprA relExprB) state =   
  EqualTupleStreamsPlan <$> planGraphRefRelationalExpr relExprA state <*> planGraphRefRelationalExpr relExprB state <*> pure ()
  
planGraphRefRelationalExpr (NotEquals relExprA relExprB) state =
  NotEqualTupleStreamsPlan <$> planGraphRefRelationalExpr relExprA state <*> planGraphRefRelationalExpr relExprB state <*> pure ()
  
planGraphRefRelationalExpr (Extend extendTupleExpr relExpr) gfEnv = do
  subExprT <- runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr relExpr)
  extendProc <- runGraphRefRelationalExprM gfEnv (extendGraphRefTupleExpressionProcessor (attributes subExprT) extendTupleExpr)
  ExtendTupleStreamPlan extendProc extendTupleExpr <$> planGraphRefRelationalExpr relExpr gfEnv <*> pure ()

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
executePlan (ReadTuplesFromMemoryPlan attrs tupSet ()) _ =
  pure $ StreamRelation attrs (Stream.fromList (asList tupSet))
executePlan (StreamTuplesFromCacheFilePlan{}) _ =
  --todo: enable streaming tuples from file
  undefined
executePlan (RenameTupleStreamPlan attrsAssoc expr ()) ctx = do
  relS <- executePlan expr ctx
  let newAttrs = renameAttributes' attrsAssoc (sRelAttributes relS)
  --potential optimization- lookup attrs in advance to rename the correct vector index
  pure $ streamRelationMap newAttrs (pure . tupleRenameAttributes attrsAssoc) relS
executePlan (RestrictTupleStreamPlan restrictionFilter _predExpr expr ()) ctx = do
  (StreamRelation attrs tupS) <- executePlan expr ctx
  let tupS' = Stream.filterM filt tupS
      -- since we are building up a stream data structure, we can represent in-stream failure using exceptions- we won't be able to execute the stream here to extract errors
      filt t =
        pure $ case restrictionFilter t ctx of
        Left err -> throw err -- this will blow up in a separate thread but streamly should shuttle it to the caller (I hope)
        Right !t' -> t'
  pure $ StreamRelation attrs tupS'
executePlan (ProjectTupleStreamPlan attrs expr ()) ctx = do
  (StreamRelation _ tupS) <- executePlan expr ctx
  let tupS' = fmap projector tupS
      --optimize by projecting on vector indexes instead
      projector t = case tupleProject attrs t of
                      Left err -> throw err
                      Right t' -> t'
  pure $ StreamRelation attrs tupS'
executePlan (UnionTupleStreamsPlan exprA exprB ()) ctx = do
  --ideally, the streams would have pre-ordered tuples and we could zip them together right away- if we have an ordered representation, we can drop the sorting here
  -- glue two streams together, then uniqueify
  (StreamRelation attrsA tupSa) <- executePlan exprA ctx
  (StreamRelation _ tupSb) <- executePlan exprB ctx
  let tupS' = Stream.parList id [tupSa, tupSb]
  pure (StreamRelation attrsA tupS')
executePlan (EqualTupleStreamsPlan exprA exprB ()) ctx = do
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
executePlan (RelationValuedAttributeStreamPlan relAttr ()) ctx = do
  case contextTupleAtomForAttributeName ctx (A.attributeName relAttr) of
    Left err -> Left err
    Right relAtom@(RelationAtom{}) -> do
      let newTup = RelationTuple (A.singleton relAttr) (V.singleton relAtom)
          tupS = Stream.fromList [newTup]
      pure (StreamRelation (A.singleton relAttr) tupS)
    Right _ -> Left (AttributeIsNotRelationValuedError (A.attributeName relAttr))
executePlan (NotEqualTupleStreamsPlan exprA exprB ()) ctx = do
  (StreamRelation _ tupS) <- executePlan (EqualTupleStreamsPlan exprA exprB ()) ctx
  let tupS' = SD.unCross $ do
        el <- liftIO $ SD.head tupS
        SD.mkCross $ Stream.fromList $ case el of
                            Nothing -> [RelationTuple mempty mempty]
                            Just _ -> []
  pure (StreamRelation mempty tupS')
executePlan (MakeStaticRelationPlan attrs tupSet ()) _ =
  pure (StreamRelation attrs (Stream.fromList (asList tupSet)))
executePlan (ExistingRelationPlan rel ()) _ = do
  pure (StreamRelation (attributes rel) (Stream.fromList (asList (tupleSet rel))))
executePlan (ExtendTupleStreamPlan (newAttrs, extendProcessor) _extendExpr expr ()) ctx = do
  relS <- executePlan expr ctx
  let extender tup =
        case extendProcessor tup ctx of
          Left err -> throw err
          Right t' -> pure t'
  pure (streamRelationMap newAttrs extender relS)
executePlan (NaiveJoinTupleStreamsPlan exprA exprB ()) ctx = do
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
executePlan (DifferenceTupleStreamsPlan exprA exprB ()) ctx = do
  (StreamRelation attrsA tupSa) <- executePlan exprA ctx
  (StreamRelation _ tupSb) <- executePlan exprB ctx
  let tupS' = SD.unCross $ do
        bTupleList <- liftIO $ Stream.toList tupSb        
        SD.mkCross $ Stream.filter (`notElem` bTupleList) tupSa
  pure (StreamRelation attrsA tupS')
executePlan (GroupTupleStreamPlan groupAttrs newAttrName expr ()) ctx = do
  --naive implementation scans for image relation for each grouped value
  (StreamRelation attrsIn tupS) <- executePlan expr ctx
  let nonGroupAttrNames = nonMatchingAttributeNameSet groupAttrNames (A.attributeNameSet attrsIn)
      groupAttrNames = A.attributeNameSet groupAttrs
  nonGroupProjectionAttributes <- projectionAttributesForNames nonGroupAttrNames attrsIn
  groupProjectionAttributes <- projectionAttributesForNames groupAttrNames attrsIn
  (StreamRelation _ nonGroupProjectionTupS) <- executePlan (ProjectTupleStreamPlan nonGroupProjectionAttributes expr()) ctx
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
executePlan (UngroupTupleStreamPlan groupAttrName expr ()) ctx = do
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
executePlan (UniqueifyTupleStreamPlan e ()) ctx = do
  (StreamRelation attrs tupS) <- executePlan e ctx
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
      gfPlan = ReadTuplesFromMemoryPlan attrs1 (RelationTupleSet [RelationTuple attrs1 (V.singleton (IntegerAtom 1))]) ()
  let (StreamRelation attrsOut tupStream) = case executePlan gfPlan (singletonContextTuple emptyTuple) of
        Left err -> throw err
        Right res -> res
  _ <- Stream.toList $ Stream.mapM print tupStream
  print attrsOut

instance Pretty Attributes where
  pretty attrs = encloseSep "{" "}" "," (map pretty (V.toList (attributesVec attrs)))

{-instance Pretty RelationTupleSet where
  pretty tupSet = vsep (map pretty (asList tupSet))
-}

instance Pretty RenameAssocs where
  pretty renameSet = hsep (map pretty (S.toList renameSet))

instance Pretty Attribute where
  pretty attr = pretty (attributeName attr) <+> pretty (atomType attr)

{-instance Pretty RelationTuple where
  pretty tup = pretty (tupleAttributes tup) <+> vsep (map (indent 4 . pretty) (V.toList (tupleAtoms tup)))
-}

instance Pretty AtomType where
  pretty aType = pretty (show aType)

instance Pretty Atom where
  pretty atom = pretty (show atom)

instance Show a => Pretty (RestrictionPredicateExprBase a) where
  pretty x = pretty (show x)

instance Show a => Pretty (ExtendTupleExprBase a) where
  pretty x = pretty (show x)

renderPretty :: (Show a, Pretty t) => RelExprExecPlanBase a t -> T.Text
renderPretty = renderStrict . layoutPretty defaultLayoutOptions . pretty

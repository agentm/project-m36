{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module ProjectM36.Streaming.RelationalExpression where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.RelationalExpression
import ProjectM36.Attribute
import ProjectM36.Tuple
import ProjectM36.Relation hiding (relationTrue, relationFalse)
import ProjectM36.WithNameExpr

import qualified Data.Map as M
import qualified Streamly.Prelude as Stream
import Control.Monad.IO.Class
import qualified Data.Vector as V
import Control.Exception
import qualified Data.HashSet as HS

type RelExprExecPlan = RelExprExecPlanBase ()

type GraphRefRelExprExecPlan = RelExprExecPlanBase GraphRefTransactionMarker

--this will become more useful once we have multiple join strategies, etc.
data RelExprExecPlanBase a =
  -- | Read relvar expr from transaction graph to generate tuple stream.
  ReadExprFromTransGraph RelVarName a |
  -- | Read tuples from a tuple set cache.
  StreamTuplesFromCacheFilePlan Attributes FilePath |
  -- | Read tuples from memory.
  ReadTuplesFromMemoryPlan Attributes RelationTupleSet |
                       
                       RestrictTupleStreamPlan RestrictionFilter (RelExprExecPlanBase a) |
                       ProjectTupleStreamPlan Attributes (RelExprExecPlanBase a) |
                       RenameTupleStreamPlan AttributeName AttributeName (RelExprExecPlanBase a) |
                       GroupTupleStreamPlan (AttributeNamesBase a) AttributeName (RelExprExecPlanBase a) |
                       UngroupTupleStreamPlan AttributeName (RelExprExecPlanBase a) |
                       ExtendTupleStreamPlan (ExtendTupleExprBase a) (RelExprExecPlanBase a) |
                       
                       UnionTupleStreamsPlan (RelExprExecPlanBase a) (RelExprExecPlanBase a) |
                       JoinTupleStreamsPlan (RelExprExecPlanBase a) (RelExprExecPlanBase a) |
                       DifferenceStreamsPlan (RelExprExecPlanBase a) (RelExprExecPlanBase a) |
                       EqualTupleStreamsPlan (RelExprExecPlanBase a) (RelExprExecPlanBase a) |
                       NotEqualTupleStreamsPlan (RelExprExecPlanBase a) (RelExprExecPlanBase a) |
                       
                       MakeStaticRelationPlan Attributes RelationTupleSet |
                       ExistingRelationPlan Relation

planRelationalExpr :: GraphRefRelationalExpr -> 
                      M.Map RelVarName GraphRefRelExprExecPlan -> 
                      GraphRefRelationalExprEnv -> 
                      Either RelationalError GraphRefRelExprExecPlan
planRelationalExpr (RelationVariable name _) rvMap _ = case M.lookup name rvMap of
  Nothing -> Left (RelVarNotDefinedError name)
  Just plan -> Right plan
  
planRelationalExpr (Project attrNames expr) rvMap gfEnv = do
  exprT <- runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr expr)
  projectionAttrNames <- runGraphRefRelationalExprM gfEnv (evalGraphRefAttributeNames attrNames expr)
  case projectionAttributesForNames projectionAttrNames (attributes exprT) of
    Left err -> Left err
    Right attrs' -> do
      subExpr <- planRelationalExpr expr rvMap gfEnv
      pure (ProjectTupleStreamPlan attrs' subExpr)
  
planRelationalExpr (Union exprA exprB) rvMap state = do  
  planA <- planRelationalExpr exprA rvMap state
  planB <- planRelationalExpr exprB rvMap state
  pure (UnionTupleStreamsPlan planA planB)
  
planRelationalExpr (Join exprA exprB) rvMap state = do  
  planA <- planRelationalExpr exprA rvMap state
  planB <- planRelationalExpr exprB rvMap state
  pure (JoinTupleStreamsPlan planA planB)
  
planRelationalExpr (Difference exprA exprB) rvMap state = do
  planA <- planRelationalExpr exprA rvMap state
  planB <- planRelationalExpr exprB rvMap state
  pure (DifferenceStreamsPlan planA planB)

planRelationalExpr (MakeStaticRelation attributeSet tupSet) _ _ = pure (MakeStaticRelationPlan attributeSet tupSet)

planRelationalExpr expr@MakeRelationFromExprs{} _ gfEnv = do
  rel <- runGraphRefRelationalExprM gfEnv (evalGraphRefRelationalExpr expr)
  pure (ExistingRelationPlan rel)
  
planRelationalExpr (ExistingRelation rel) _ _ = pure (ExistingRelationPlan rel)  

planRelationalExpr (Rename oldAttrName newAttrName relExpr) rvMap state = 
  RenameTupleStreamPlan oldAttrName newAttrName <$> planRelationalExpr relExpr rvMap state
  
planRelationalExpr (Group oldAttrNames newAttrName relExpr) rvMap state = 
  GroupTupleStreamPlan oldAttrNames newAttrName <$> planRelationalExpr relExpr rvMap state
  
planRelationalExpr (Ungroup attrName relExpr) rvMap state = 
  UngroupTupleStreamPlan attrName <$> planRelationalExpr relExpr rvMap state
  
planRelationalExpr (Restrict predExpr relExpr) rvMap gfEnv = do
  rfilt <- runGraphRefRelationalExprM gfEnv $ do
    exprT <- typeForGraphRefRelationalExpr relExpr    
    predicateRestrictionFilter (attributes exprT) predExpr
  RestrictTupleStreamPlan rfilt <$> planRelationalExpr relExpr rvMap gfEnv
  
planRelationalExpr (Equals relExprA relExprB) rvMap state =   
  EqualTupleStreamsPlan <$> planRelationalExpr relExprA rvMap state <*> planRelationalExpr relExprB rvMap state
  
planRelationalExpr (NotEquals relExprA relExprB) rvMap state =
  NotEqualTupleStreamsPlan <$> planRelationalExpr relExprA rvMap state <*> planRelationalExpr relExprB rvMap state
  
planRelationalExpr (Extend tupleExpression relExpr) rvMap state = 
  ExtendTupleStreamPlan tupleExpression <$> planRelationalExpr relExpr rvMap state

planRelationalExpr (With macros expr) rvMap gfEnv = 
  --TODO: determine if macros should be expanded or executed separately- perhaps calculate how many times a macro appears and it's calculation and size cost to determine if it should be precalculated.
  planRelationalExpr (substituteWithNameMacros macros expr) rvMap gfEnv

data StreamRelation m = StreamRelation Attributes (Stream.AsyncT m RelationTuple)

--until we can stream results to disk or socket, we return a lazy-list-based Relation
-- | Build a tuple stream from an execution plan to enable parallel execution.
executePlan :: (MonadIO m, Stream.MonadAsync m) => GraphRefRelExprExecPlan -> Either RelationalError (StreamRelation m)
executePlan (ReadTuplesFromMemoryPlan attrs tupSet) =
  pure $ StreamRelation attrs (Stream.fromList (asList tupSet))
executePlan (StreamTuplesFromCacheFilePlan{}) =
  --todo: enable streaming tuples from file
  undefined
executePlan (RenameTupleStreamPlan oldName newName expr) = do
  (StreamRelation attrs tupS) <- executePlan expr
  let newAttrs = renameAttributes oldName newName attrs
  --potential optimization- lookup attrs in advance to rename the correct vector index
  pure $ StreamRelation newAttrs (Stream.mapM (pure . tupleRenameAttribute oldName newName) tupS)
executePlan (RestrictTupleStreamPlan restrictionFilter expr) = do
  (StreamRelation attrs tupS) <- executePlan expr
  let tupS' = Stream.filter filt tupS
      -- since we are building up a stream data structure, we can represent in-stream failure using exceptions- we won't be able to execute the stream here to extract errors
      filt t = case restrictionFilter t of
        Left err -> throw err -- this will blow up in a separate thread but streamly should shuttle it to the caller (I hope)
        Right t' -> t'
  pure $ StreamRelation attrs tupS'
executePlan (ProjectTupleStreamPlan attrs expr) = do
  (StreamRelation _ tupS) <- executePlan expr
  let tupS' = Stream.map projector tupS
      --optimize by projecting on vector indexes instead
      projector t = case tupleProject attrs t of
                      Left err -> throw err
                      Right t' -> t'
  pure $ StreamRelation attrs tupS'
executePlan (UnionTupleStreamsPlan exprA exprB) = do
  --ideally, the streams would have pre-ordered tuples and we could zip them together right away- if we have an ordered representation, we can drop the sorting here
  -- glue two streams together, then uniqueify
  (StreamRelation attrsA tupSa) <- executePlan exprA
  (StreamRelation _ tupSb) <- executePlan exprB
  
  let unionedHS = tuplesHashSet (tupSa <> tupSb)
      tupS' = do
        uhs <- liftIO unionedHS
        Stream.fromFoldable uhs
  pure (StreamRelation attrsA (Stream.fromSerial tupS'))
executePlan (EqualTupleStreamsPlan exprA exprB) = do
  (StreamRelation _ tupSa) <- executePlan exprA
  (StreamRelation _ tupSb) <- executePlan exprB
  let hsA = tuplesHashSet tupSa
      hscmp = tuplesHashSet (tupSa <> tupSb)
      tupS' = do
        tA <- liftIO hsA
        tcmp <- liftIO hscmp
        Stream.fromList $ if HS.size tA == HS.size tcmp then
          [RelationTuple mempty mempty]
          else
          []
  pure (StreamRelation mempty tupS')

executePlan (NotEqualTupleStreamsPlan exprA exprB) = do
  (StreamRelation _ tupS) <- executePlan (EqualTupleStreamsPlan exprA exprB)
  let tupS' = do
        el <- liftIO $ Stream.head (Stream.fromAsync tupS)
        Stream.fromList $ case el of
                            Nothing -> [RelationTuple mempty mempty]
                            Just _ -> []
  pure (StreamRelation mempty tupS')
executePlan (MakeStaticRelationPlan attrs tupSet) = do
  pure (StreamRelation attrs (Stream.fromList (asList tupSet)))
executePlan (ExistingRelationPlan rel) = do
  pure (StreamRelation (attributes rel) (Stream.fromList (asList (tupleSet rel))))

relationTrue :: (MonadIO m, Stream.MonadAsync m) => StreamRelation m
relationTrue = StreamRelation mempty (Stream.fromList [RelationTuple mempty mempty])

relationFalse :: (MonadIO m, Stream.MonadAsync m) => StreamRelation m
relationFalse = StreamRelation mempty (Stream.fromList [])

tuplesHashSet :: MonadIO m => Stream.AsyncT m RelationTuple -> m (HS.HashSet RelationTuple)
tuplesHashSet s =
  Stream.foldr (\t acc -> HS.insert t acc) mempty (Stream.fromAsync s)  

test1 :: IO ()
test1 = do
  let attrs1 = attributesFromList [Attribute "x" IntegerAtomType]
      gfPlan = ReadTuplesFromMemoryPlan attrs1 (RelationTupleSet [RelationTuple attrs1 (V.singleton (IntegerAtom 1))])
  let Right (StreamRelation attrsOut tupStream) = executePlan gfPlan
      
  _ <- Stream.toList $ Stream.mapM print $ Stream.fromAsync tupStream
  print attrsOut

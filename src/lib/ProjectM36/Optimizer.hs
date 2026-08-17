{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Tracks hotspots in the database usage and suggests database reorganization and caching optimizations. The first half of this optimizer tracks hotspots in the database by analyzing which data is most heavily accessed (read/written). The second half of the module uses this statistical data to suggest how to rearrange the database to better serve the hotspots. This can include planning and\/or running queries to determine if the suggestions are useful.
module ProjectM36.Optimizer where
import qualified ProjectM36.StaticOptimizer as StaticOpt
import qualified ProjectM36.CostBasedOptimizer as CostBasedOpt
import ProjectM36.Cache.RelationalExprCache as RelExprCache
import ProjectM36.RelationalExpression
import ProjectM36.Base
import ProjectM36.TransactionGraph.Types
import ProjectM36.TransGraphRelationalExpression as TGRE
import ProjectM36.Error
import ProjectM36.NormalizeExpr
import ProjectM36.Streaming.RelationalExpression
import ProjectM36.PinnedRelationalExpr
import ProjectM36.Relation
import ProjectM36.SystemMemory

import Control.Monad.STM
import System.Random
import Data.Time.Clock
import Control.Exception
import Control.DeepSeq
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except

-- Rationale: heuristics are quickly out-of-date and never hardware-specific enough. When we start using the database, we can rely on obvious trade-offs (such as creating a oft-used btree representation), but we should rely on experiments and projections from those experiments for future optimization decision-making, thus reducing reliance on heuristics, knobs to adjust heuristics, and statistics. This optimizer is largely based on trying different query execution strategies whenever possible. Then, we want to record the results of the experiments as our source of performance truths.

-- Logic: execute with what is available now, add experiments with cost comparisons to run in the background. B-tree indexes are often a no-brainer on expensive parts of a query, just as humans add them willy-nilly, but we track when they're no longer worth keeping around.

{-
Phase I:
The optimizer makes a plan based on available representations. In memory representations only.

Phase II:
The executor can switch the plan while executing if it finds a faster representation. This can happen if a background process has added a representation after the planning phase, but before execution.

Phase III:
The planner can now create new representations if its deems it worthwhile. For example, the planner can create a btree representation if the cost of doing so is amortized by the rest of the plan.

In the background, an experimenter thread receives suggestions from the planner for experiments that it should run, thereby creating new representations in the background.
-}

-- | Context needed to run optimizer.
data OptimizerEnv r =
  OptimizerEnv {
   optCache :: RelExprCache,
   optStats :: CostBasedOpt.OptimizerStats,
   optRandomGen :: r
   }
  
-- apply static optimizer first, then cost-based optimizer, finally execute the optimized query
optimizeAndEvalRelationalExpr :: RandomGen r => OptimizerEnv r -> RelationalExprEnv -> RelationalExpr -> IO (Either RelationalError Relation)
optimizeAndEvalRelationalExpr optEnv relExprEnv expr = do
  let gfExpr = runProcessExprM UncommittedContextMarker (processRelationalExpr expr) -- references parent tid instead of context! options- I could add the context to the graph with a new transid or implement an evalRelationalExpr in RE.hs to use the context (which is what I had previously)
      graph = re_graph relExprEnv
      ctx = re_context relExprEnv
      gfEnv = freshGraphRefRelationalExprEnv (Just ctx) graph
  --first, type check
  case runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr gfExpr) of
    Left err -> pure (Left err)
    Right _ -> do
      -- then, apply static optimizations
      case StaticOpt.runGraphRefSOptRelationalExprM (Just ctx) (re_graph relExprEnv) (StaticOpt.fullOptimizeGraphRefRelationalExpr gfExpr) of
        Left err -> pure (Left err)
        Right staticOptGfExpr -> do
          -- next, apply cost-based optimizations
          (optGfExpr, _suggestions) <- atomically $ CostBasedOpt.optimizeGraphRefRelationalExpr (optCache optEnv) (optStats optEnv) staticOptGfExpr
          -- note suggestions for background experiments
          evalGraphRefRelationalExprWithCache optEnv gfEnv optGfExpr

  
-- | For internal use- expression argument should pass through static optimizer beforehand.
evalGraphRefRelationalExprWithCache :: RandomGen r => OptimizerEnv r -> GraphRefRelationalExprEnv -> GraphRefRelationalExpr -> IO (Either RelationalError Relation)
evalGraphRefRelationalExprWithCache optEnv gfEnv gfExpr =
  case planGraphRefRelationalExpr gfExpr gfEnv of
    Left err -> pure (Left err)
    Right plan -> do
      startExecTime <- getCurrentTime
      exec <- executePlan plan mempty gfEnv mempty (optCache optEnv) -- try/catch to handle exceptions
      case exec of
        Left err -> pure (Left err)
        Right resultStream -> do
          --convert tuple stream into relation- we could push the results to the socket directly without materializing the entire relation
          relationResult <- streamRelationAsRelation resultStream
          relationResult' <- evaluate (force relationResult)
          endExecTime <- getCurrentTime
          let execDiffTime = endExecTime `diffUTCTime` startExecTime
          --add to the cache- we cannot add uncommitted data to the cache since uncommitted data does not have a unique key (transaction id) (should uncommitted data be able to be cached with a transaction id that has not been committed?)
              mCacheKey :: Maybe (RelationalExprBase TransactionId)
              mCacheKey = originalRelExpr plan >>= toPinnedRelationalExpr
              cacheValue = UnsortedTupleSetRep (attributes relationResult') (tupleSet relationResult')
              --cacheValue = PinnedExpressionRep (ExistingRelation relationResult') -- ideally, we would cache the expensive parts of the plan, not just the top-level result
          case mCacheKey of
            Nothing -> pure (Right relationResult')
            Just cacheKey -> do
              eMemStats <- getMemoryStats -- consider running mem stats less often if it's a bottleneck
              case eMemStats of
                Left err -> pure (Left (SystemError err))
                Right memStats -> do
                  void $ atomically $
                    let rando = optRandomGen optEnv in
                    RelExprCache.add rando cacheKey cacheValue execDiffTime False memStats (optCache optEnv)
                  pure (Right relationResult')

-- | Optimization can be disabled due to missing context in isomorphic transformations.
optimizeAndEvalDatabaseContextExpr :: Bool -> DatabaseContextExpr' -> DatabaseContextEvalMonad ()
optimizeAndEvalDatabaseContextExpr runOpt expr = do
  graph <- asks dce_graph
  transId <- asks dce_transId
  context <- getStateContext
  dbcfuncutils <- asks dce_dbcfuncutils
  let gfExpr = runProcessExprM UncommittedContextMarker (processDatabaseContextExpr expr)
      eOptExpr = if runOpt then
                   StaticOpt.runGraphRefSOptDatabaseContextExprM transId context graph dbcfuncutils (StaticOpt.optimizeGraphRefDatabaseContextExpr gfExpr)
                   else
                   pure gfExpr
  case eOptExpr of
    Left err -> throwError err
    Right optExpr -> evalGraphRefDatabaseContextExpr optExpr

optimizeAndEvalDatabaseContextIOExpr :: DatabaseContextIOExpr -> DatabaseContextIOEvalMonad ()
optimizeAndEvalDatabaseContextIOExpr expr = do
  transId <- asks dbcio_transId
  ctx <- getDBCIOContext
  graph <- asks dbcio_graph
  let gfExpr = runProcessExprM UncommittedContextMarker (processDatabaseContextIOExpr expr)
      eOptExpr = StaticOpt.runGraphRefSOptDatabaseContextIOExprM transId ctx graph (StaticOpt.optimizeDatabaseContextIOExpr gfExpr)
  case eOptExpr of
    Left err -> throwError err
    Right optExpr ->
      evalGraphRefDatabaseContextIOExpr optExpr

optimizeAndEvalTransGraphRelationalExprWithCache :: RandomGen r => OptimizerEnv r -> TransactionGraph -> TransGraphRelationalExpr -> IO (Either RelationalError Relation)
optimizeAndEvalTransGraphRelationalExprWithCache optEnv graph tgExpr = do
  let gfEnv = freshGraphRefRelationalExprEnv Nothing graph
      res = do
        gfExpr <- TGRE.process (TransGraphEvalEnv graph) tgExpr
        _typ <- runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr gfExpr)
        StaticOpt.runGraphRefSOptRelationalExprM Nothing graph (StaticOpt.fullOptimizeGraphRefRelationalExpr gfExpr)
  case res of
    Left err -> pure (Left err)
    Right optExpr ->
      evalGraphRefRelationalExprWithCache optEnv gfEnv optExpr


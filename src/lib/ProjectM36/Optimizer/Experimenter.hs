
-- | A background thread which receives suggestions from the optimizer as queries are run. The experimenter may add suggested representations to the relational expression cache and run queries against the cache to determine if the experiment results in query performance improvements.
module ProjectM36.Optimizer.Experimenter where

import ProjectM36.Cache.RelationalExprCache as RelExprCache (RelExprCache, add)
import ProjectM36.CostBasedOptimizer (ReorgSuggestion(..), SuggestionReason(..), OptimizerStats)
import ProjectM36.SystemMemory
import ProjectM36.Base
import ProjectM36.Optimizer as Opt
import ProjectM36.PinnedRelationalExpr (toGraphRefRelationalExpr)
import ProjectM36.RelationalExpression (GraphRefRelationalExprEnv(..), runGraphRefRelationalExprM, typeForGraphRefRelationalExpr)
import ProjectM36.Relation (attributes)
import qualified ProjectM36.Relation.Representation.BTree as BT
import ProjectM36.TransactionGraph.Types
import ProjectM36.Relation.Representation

import Control.Concurrent 
import Control.DeepSeq (force)
import Control.Concurrent.STM.TBQueue
import Control.Monad.STM
import Control.Exception (evaluate)
import Control.Monad (foldM, void)
import System.Random (RandomGen, initStdGen)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

data ExperimenterThread =
  ExperimenterThread {
  _experimenterRelExprCache :: RelExprCache,
  _experimenterQueue :: TBQueue ExperimenterAction,
  _experimenterThreadId :: ThreadId
  }

start :: TransactionGraph -> RelExprCache -> OptimizerStats -> IO ExperimenterThread
start graph cache optStats' = do
  queue <- newTBQueueIO 100
  rando <- initStdGen
  tid <- forkIO (experimenterWorker rando graph cache optStats' queue)
  pure (ExperimenterThread {
           _experimenterRelExprCache = cache,
           _experimenterQueue = queue,
           _experimenterThreadId = tid
           })

data ExperimenterAction =
  SuggestExperiment [ReorgSuggestion] |
  ShutdownExperimenterThread

experimenterWorker :: RandomGen r => r -> TransactionGraph -> RelExprCache -> OptimizerStats ->  TBQueue ExperimenterAction -> IO ()
experimenterWorker rando graph cache optStats' queue =
  let loop :: RandomGen g => g -> IO g
      loop rando' = do
        action <- atomically $ readTBQueue queue
        randonext <- case action of
          ShutdownExperimenterThread -> pure rando'
          SuggestExperiment reorgsuggestions -> do
              -- run the experiment and tell relexprcache to retain the data if the suggestion makes a performance improvement
              -- we add the representation to the global relexprcache as an experimental entry which may be evicted immediately under memory pressure
            let suggestionFolder rando'' (AddBtreeSuggestion pinnedExpr reason) = addBTreeExperiment rando'' pinnedExpr graph cache reason optStats'
            foldM suggestionFolder rando' reorgsuggestions
        loop randonext
  in void (loop rando)

addBTreeExperiment :: RandomGen g => g -> PinnedRelationalExpr -> TransactionGraph -> RelExprCache -> SuggestionReason -> OptimizerStats -> IO g
addBTreeExperiment rando pinnedExpr graph cache reason optStats' = do
  memStats <- getMemoryStats >>=
              \ems -> case ems of
                        Left _err -> error "failed to get memory stats"
                        Right ms -> pure ms
              
  -- read the pinned expr and convert it into a btree
  let gfExpr = toGraphRefRelationalExpr pinnedExpr
      optimizerEnv = OptimizerEnv { optCache = cache,
                                    optStats = optStats',
                                    optRandomGen = rando }
      gfEnv = GraphRefRelationalExprEnv { gre_context = Nothing,
                                          gre_graph = graph,
                                          gre_extra = Nothing }
  case runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr gfExpr) of
    Left _err -> pure rando -- TODO: log type error
    Right gfExprType -> do
      -- ensure that the type is suitable for our unboxed vector btree
      if BT.suitable (attributes gfExprType) then do
        -- if we recently executed this exact expression, then is it likely to be in the relexpr cache
        startTime <- getCurrentTime
        eGfExprResult <- Opt.optimizeAndEvalGraphRefRelationalExpr optimizerEnv gfEnv gfExpr
        case eGfExprResult of
          Left _err -> pure rando -- TODO: log result error
          Right gfExprResult ->
            case BT.construct gfExprResult of
              Nothing -> error "bad index creation" -- pure ()
              Just bt -> do
                bt' <- evaluate (force bt)
                endTime <- getCurrentTime
                let relRepresentation = BTreeRep pinnedExpr (attributes gfExprType) bt'
                    calcTime = endTime `diffUTCTime` startTime
                    isRegQuery = False -- TODO: registered queries are RelationalExpr- can we extract that from a pinned relational expr and some context or is it a lost cause
                    shouldCache = case reason of
                                    ObviousImprovementReason -> True
                                    TestIfFasterThanMsReason _queryToService _maxtime -> error "unimplemented" -- to implement: run the queryToService against the cached version- if the query runs faster than maxtime, then retain it, otherwise toss it
                if shouldCache then
                  fst <$> atomically (RelExprCache.add rando pinnedExpr relRepresentation calcTime isRegQuery memStats True cache)
                  else
                  pure rando
        else
        pure rando -- TODO: log bad suggestion
  

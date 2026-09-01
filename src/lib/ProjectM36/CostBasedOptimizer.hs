{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ProjectM36.CostBasedOptimizer where
import ProjectM36.Base (PinnedRelationalExpr, RelationalExprBase(..))
import ProjectM36.AttributeNamesBase (GraphRefRelationalExpr)
import ProjectM36.Cache.RelationalExprCache as RelExprCache
import qualified Data.HashPSQ as Q
import Data.Time.Clock (NominalDiffTime)
import Data.Hashable
import Control.Monad.STM

type Cost = NominalDiffTime

-- | Stores hashes of relational algebra snippets from real queries alongwith their costs. This allows us to capture high-level requests resulting in low-level results (time cost). The optimizer then suggests to the cache what and how to improve. In the future, we could track IO and CPU cost, but streamly does not make that easy now.
newtype OrdGraphRefRelationalExpr = OrdGraphRefRelationalExpr { _gfExpr :: PinnedRelationalExpr }
 deriving (Eq, Hashable, Show)

instance Ord OrdGraphRefRelationalExpr where
  compare a b = show a `compare` show b -- this will only be called rarely on a hash collision, so this cheap implementation should be good enough for now

-- The maximum size of the queue should be guided by the amount of time it takes to insert a new value. If the value is too high relative to the rest of the query planner, then reduce the max size.
data OptimizerStats = OptimizerStats {
  queue :: Q.HashPSQ OrdGraphRefRelationalExpr Cost (),
  maxSize :: Int
  }

emptyStats :: OptimizerStats
emptyStats =
  OptimizerStats {
  queue = Q.empty,
  maxSize = 100
  }

mostExpensiveItem :: OptimizerStats -> Maybe PinnedRelationalExpr
mostExpensiveItem stats =
  case Q.findMin (queue stats) of
    Nothing -> Nothing
    Just (k, _p, ()) -> Just (_gfExpr k)

recordCost :: OptimizerStats -> Cost -> PinnedRelationalExpr -> OptimizerStats
recordCost stats currentCost gfExpr =
  stats { queue = truncateQ $ Q.insert (OrdGraphRefRelationalExpr gfExpr) (negate currentCost) () (queue stats)
        }
  where
    truncateQ q = if Q.size q > maxSize stats then
      Q.fromList $ take (maxSize stats) (Q.toList q)
      else
      q


-- should this examine the existing relexprcache state?
suggestOptimization :: RelExprCache -> OptimizerStats -> STM [OptimizerSuggestion]
suggestOptimization cache stats =
  case mostExpensiveItem stats of
    Nothing -> pure [] 
    Just expr ->
      case expr of
        Project{} -> do
          --check that the btree representation is not already in the cache
          mCacheInfos <- RelExprCache.lookup expr cache
          case mCacheInfos of
            Nothing ->
              pure [AddBtreeSuggestion expr ObviousImprovementReason]
            Just _ -> pure []
        _ -> pure []

-- | Create the suggested optimization and test that the cost is reduced.
--runExperiment :: ReorgSuggestion -> GraphRefRelationalExpr -> Cost -> IO Cost
--runExperiment suggestion gfExpr unoptimizedCost = postOptCost
-- the relational expression can be used as a reason for creating the suggested btree representation
data OptimizerSuggestion = AddBtreeSuggestion PinnedRelationalExpr SuggestionReason  
                       -- RemoveBtreeSuggestion PinnedRelationalExpr SuggestionReason

data SuggestionReason = TestIfFasterThanMsReason PinnedRelationalExpr NominalDiffTime |
                        -- ^ Test if creation of the representation serves this pinned query faster than x. If it does, retain it.
                        ObviousImprovementReason

-- PDF: f(x) = (alpha * xm^alpha) / x^(alpha+1)   for x >= xm
paretoProbabilityDistributionF :: Double -> Double -> Double -> Double
paretoProbabilityDistributionF xm alpha x
  | xm <= 0 || alpha <= 0 = error "xm and alpha must be > 0"
  | x < xm                = 0.0
  | otherwise             = (alpha * xm ** alpha) / (x ** (alpha + 1))

optimizeGraphRefRelationalExpr ::
  RelExprCache ->
  OptimizerStats ->
  GraphRefRelationalExpr ->
  STM (GraphRefRelationalExpr, [OptimizerSuggestion])
optimizeGraphRefRelationalExpr cache stats relExpr = do
  -- note that we cannot rely on any cache entries hanging around- this is intentional so as not to hold locks on the cache for too long. We can, however, predict which cache entries may hang around long enough for us to use.
  -- scan for existence (projection on empty attributes) expressions that could benefit from btree 
  suggestions <- suggestOptimization cache stats
  pure (relExpr, suggestions)

  

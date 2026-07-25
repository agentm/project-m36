{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Tracks hotspots in the database usage and suggests database reorganization and caching optimizations. The first half of this optimizer tracks hotspots in the database by analyzing which data is most heavily accessed (read/written). The second half of the module uses this statistical data to suggest how to rearrange the database to better serve the hotspots. This can include planning and\/or running queries to determine if the suggestions are useful.
module ProjectM36.Optimizer where
import ProjectM36.Base
import ProjectM36.Cache.RelationalExprCache
import qualified Data.HashPSQ as Q
import Data.Time.Clock (NominalDiffTime)
import Data.Hashable
import Control.Monad.STM

-- | Stores hashes of relational algebra snippets from real queries alongwith their costs. This allows us to capture high-level requests resulting in low-level results (time cost). The optimizer then suggests to the cache what and how to improve. In the future, we could track IO and CPU cost, but streamly does not make that easy now.

type Cost = NominalDiffTime

newtype OrdGraphRefRelationalExpr = OrdGraphRefRelationalExpr { _gfExpr :: GraphRefRelationalExpr }
 deriving (Eq, Hashable, Show)

instance Ord OrdGraphRefRelationalExpr where
  compare a b = show a `compare` show b -- this will only be called rarely on a hash collision, so this cheap implementation should be good enough for now

{-
data QItem = QItem GraphRefRelationalExpr Cost
 deriving (Eq)

instance Ord QItem where
  compare (QItem _ a) (QItem _ b) = a `compare` b
-}

-- The maximum size of the queue should be guided by the amount of time it takes to insert a new value. If the value is too high relative to the rest of the query planner, then reduce the max size.
data OptimizerStats = OptimizerStats {
  queue :: Q.HashPSQ OrdGraphRefRelationalExpr Cost (),
  maxSize :: Int
  }

mostExpensiveItem :: OptimizerStats -> Maybe GraphRefRelationalExpr
mostExpensiveItem stats =
  case Q.findMin (queue stats) of
    Nothing -> Nothing
    Just (k, _p, ()) -> Just (_gfExpr k)

recordCost :: OptimizerStats -> Cost -> GraphRefRelationalExpr -> OptimizerStats
recordCost stats currentCost gfExpr =
  stats { queue = truncateQ $ Q.insert (OrdGraphRefRelationalExpr gfExpr) (negate currentCost) () (queue stats)
        }
  where
    truncateQ q = if Q.size q > maxSize stats then
      Q.fromList $ take (maxSize stats) (Q.toList q)
      else
      q


-- should this examine the existing relexprcache state?
suggestOptimization :: RelExprCache -> OptimizerStats -> STM [ReorgSuggestion]
suggestOptimization _cache stats =
  case mostExpensiveItem stats of
    Nothing -> pure [] 
    Just expr ->
      case expr of
        Project{} -> pure [AddBtreeSuggestion expr]
        _ -> pure []

-- | Create the suggested optimization and test that the cost is reduced.
--runExperiment :: ReorgSuggestion -> GraphRefRelationalExpr -> Cost -> IO Cost
--runExperiment suggestion gfExpr unoptimizedCost = postOptCost
-- the relational expression can be used as a reason for creating the suggested btree representation
data ReorgSuggestion = AddBtreeSuggestion GraphRefRelationalExpr | 
                       RemoveBtreeSuggestion GraphRefRelationalExpr 

-- PDF: f(x) = (alpha * xm^alpha) / x^(alpha+1)   for x >= xm
paretoProbabilityDistributionF :: Double -> Double -> Double -> Double
paretoProbabilityDistributionF xm alpha x
  | xm <= 0 || alpha <= 0 = error "xm and alpha must be > 0"
  | x < xm                = 0.0
  | otherwise             = (alpha * xm ** alpha) / (x ** (alpha + 1))

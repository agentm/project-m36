-- | Tracks hotspots in the database usage and suggests database reorganization and caching optimizations. The first half of this optimizer tracks hotspots in the database by analyzing which data is most heavily accessed (read/written). The second half of the module uses this statistical data to suggest how to rearrange the database to better serve the hotspots. This can include planning and\/or running queries to determine if the suggestions are useful.
module ProjectM36.Optimizer where
import ProjectM36.Base
import qualified Data.PQueue.Max as Q

-- | Stores hashes of relational algebra snippets from real queries alongwith their costs. This allows us to capture high-level requests resulting in low-level results (time cost). The optimizer then suggests to the cache what and how to improve. In the future, we could track IO and CPU cost, but streamly does not make that easy now.

type Cost = NominalDiffTime

data QItem = QItem GraphRefRelationalExpr Cost

instance Ord QItem where
  compare (QItem _ a) (QItem _ b) = a `compare` b

-- | Estimate
data OptimizerStats = OptimizerStats {
  meanCost ::
  queue :: Q.MaxQueue QItem
  }

mostExpensiveItem :: OptimizerStats -> Maybe QItem
mostExpensiveItem (OptimizerStats q) = Q.getMax q

recordCost :: OptimizerStats -> Cost -> GraphRefRelationalExpr -> OptimizerStats
recordCost stats currentCost gfExpr = do
  

-- should this examine the existing relexprcache state?
suggestOptimization :: OptimizerStats -> STM [ReorgSuggestion]
suggestOptimization = undefined

-- | Create the suggested optimization and test that the cost is reduced.
--runExperiment :: ReorgSuggestion -> GraphRefRelationalExpr -> Cost -> IO Cost
--runExperiment suggestion gfExpr unoptimizedCost = postOptCost

data ReorgSuggestion = AddBtreeSuggestion GraphRefRelationalExpr |
                       RemoveBtreeSuggestion GraphRefRelationalExpr 

-- PDF: f(x) = (alpha * xm^alpha) / x^(alpha+1)   for x >= xm
paretoProbabilityDistributionF :: Double -> Double -> Double -> Double
paretoProbabilityDistributionF xm alpha x
  | xm <= 0 || alpha <= 0 = error "xm and alpha must be > 0"
  | x < xm                = 0.0
  | otherwise             = (alpha * xm ** alpha) / (x ** (alpha + 1))

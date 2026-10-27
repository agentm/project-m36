module ProjectM36.TransactionGraph.FindTransaction where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Transaction.Types
import ProjectM36.TransactionGraph.Types
import ProjectM36.DatabaseContext.Types
import ProjectM36.RelationalExpression.Types
import Control.Monad.Trans.Except (except)
import Control.Monad.Trans.Class (lift)

import qualified Data.UUID as U
import qualified Data.Set as S

dbContextForTransId :: TransactionId -> TransactionGraph -> Either RelationalError DatabaseContext
dbContextForTransId tid graph = do
  trans <- transactionForId tid graph
  pure (concreteDatabaseContext trans)

transactionForId :: TransactionId -> TransactionGraph -> Either RelationalError Transaction
transactionForId tid graph 
  | tid == U.nil =
    Left RootTransactionTraversalError
  | otherwise =
      let sameTID (Transaction idMatch _ _) = idMatch == tid
          matchingTrans = S.filter sameTID $ transactionsForGraph graph
      in  case S.toList matchingTrans of
        [] -> Left $ NoSuchTransactionError tid
        x : _ -> Right x

gfTransForId :: TransactionId -> GraphRefRelationalExprM Transaction
gfTransForId tid = do
  graph <- gfGraph
  lift $ except $ transactionForId tid graph

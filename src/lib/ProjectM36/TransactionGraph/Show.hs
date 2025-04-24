module ProjectM36.TransactionGraph.Show where
import ProjectM36.TransactionGraph
import ProjectM36.TransactionGraph.Types
import ProjectM36.Transaction.Types
import qualified Data.Set as S

showTransactionStructure :: Transaction -> TransactionGraph -> String
showTransactionStructure trans graph = headInfo ++ " " ++ show (transactionId trans) ++ " p" ++ parentTransactionsInfo
  where
    headInfo = show (headNamesForTransaction trans graph)
    parentTransactionsInfo = if isRootTransaction trans then "root" else case parentTransactions trans graph of
      Left err -> show err
      Right parentTransSet -> concat $ S.toList $ S.map (show . transactionId) parentTransSet

  
showGraphStructure :: TransactionGraph -> String
showGraphStructure graph@(TransactionGraph _ transSet) = S.foldr folder "" transSet
  where
    folder trans acc = acc ++ showTransactionStructure trans graph ++ "\n"
    
                             

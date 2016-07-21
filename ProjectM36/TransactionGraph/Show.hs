{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.TransactionGraph.Show where
import ProjectM36.Base
import ProjectM36.TransactionGraph
import ProjectM36.Transaction
import qualified Data.Set as S

showTransactionStructure :: Transaction -> TransactionGraph -> String
showTransactionStructure trans graph = headInfo ++ " " ++ show (transactionId trans) ++ " p" ++ parentTransactionsInfo ++ " c" ++ childTransactionsInfo
  where
    headInfo = maybe "" show (headNameForTransaction trans graph)
    parentTransactionsInfo = if isRootTransaction trans graph then "root" else case parentTransactions trans graph of
      Left err -> show err
      Right parentTransSet -> concat $ S.toList $ S.map (show . transactionId) parentTransSet
    childTransactionsInfo = show (S.toList (transactionChildIds trans))
  
showGraphStructure :: TransactionGraph -> String
showGraphStructure graph@(TransactionGraph _ transSet) = S.foldr folder "" transSet
  where
    folder trans acc = acc ++ showTransactionStructure trans graph ++ "\n"
    
                             
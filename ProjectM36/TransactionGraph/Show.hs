{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.TransactionGraph.Show where
import ProjectM36.Base
import ProjectM36.TransactionGraph
import ProjectM36.Transaction
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T

showTransactionStructure :: Transaction -> TransactionGraph -> String
showTransactionStructure trans graph = headInfo ++ " " ++ show (transactionUUID trans) ++ " p" ++ parentTransactionsInfo ++ " c" ++ childTransactionsInfo
  where
    headInfo = maybe "" show (headNameForTransaction trans graph)
    parentTransactionsInfo = if isRootTransaction trans graph then "root" else case parentTransactions trans graph of
      Left err -> show err
      Right parentTransSet -> concat $ S.toList $ S.map (show . transactionUUID) parentTransSet
    childTransactionsInfo = show (S.toList (transactionChildren trans))
  
showGraphStructure :: TransactionGraph -> String
showGraphStructure graph@(TransactionGraph _ transSet) = S.foldr folder "" transSet
  where
    folder trans acc = acc ++ showTransactionStructure trans graph ++ "\n"
    
graphAsDot :: TransactionGraph -> String                              
graphAsDot (TransactionGraph heads transSet) = "digraph {" ++ dot ++ headInfo ++ "}"
  where
    dot = S.foldr transactionAsDot "" transSet
    transactionAsDot trans acc = acc ++ childArrows trans ++ parentArrows trans
    arrows trans idSet = S.foldr (\c acc' -> acc' ++ oneArrow trans c) "" idSet
    childArrows trans = arrows trans (transactionChildren trans)
    parentArrows trans = arrows trans (transactionParentUUIDs trans)
    transLabel t = tidLabel (transactionUUID t)
    tidLabel l = "\"" ++ show l ++ "\""
    oneArrow trans tid = transLabel trans ++ " -> " ++ tidLabel tid ++ ";\n"
    headInfo = M.foldrWithKey (\headName t acc -> transLabel t ++ " [label=\"" ++ (show . transactionUUID) t ++ ":" ++ T.unpack headName ++ "\"];\n" ++ acc) "" heads



                             
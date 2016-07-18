module ProjectM36.TransactionGraph.Show.Dot where
import ProjectM36.Transaction
import ProjectM36.Base
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M

graphAsDot :: TransactionGraph -> String                              
graphAsDot (TransactionGraph heads transSet) = "digraph {" ++ dot ++ headInfo ++ "}"
  where
    dot = S.foldr transactionAsDot "" transSet
    transactionAsDot trans acc = acc ++ childArrows trans ++ parentArrows trans
    arrows trans idSet = S.foldr (\c acc' -> acc' ++ oneArrow trans c) "" idSet
    childArrows trans = arrows trans (transactionChildIds trans)
    parentArrows trans = arrows trans (transactionParentIds trans)
    transLabel t = tidLabel (transactionUUID t)
    tidLabel l = "\"" ++ show l ++ "\""
    oneArrow trans tid = transLabel trans ++ " -> " ++ tidLabel tid ++ ";"
    headInfo = M.foldrWithKey (\headName t acc -> transLabel t ++ " [label=\"" ++ (show . transactionUUID) t ++ ":" ++ T.unpack headName ++ "\"];" ++ acc) "" heads




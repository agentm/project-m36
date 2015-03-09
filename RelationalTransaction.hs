module RelationalTransaction where
import Data.UUID (UUID, nil)
import RelationType
import RelationalError
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Applicative ((<$>))

bootstrapTransactionGraph :: UUID -> DatabaseContext -> TransactionGraph
bootstrapTransactionGraph freshUUID context = TransactionGraph bootstrapHeads bootstrapTransactions
  where
    bootstrapHeads = M.singleton "master" freshTransaction
    freshTransaction = Transaction freshUUID (TransactionInfo nil S.empty) context
    bootstrapTransactions = S.singleton freshTransaction
    
transactionForHead :: String -> TransactionGraph -> Maybe Transaction
transactionForHead headName (TransactionGraph heads _) = M.lookup headName heads

headNameForTransaction :: Transaction -> TransactionGraph -> Maybe String
headNameForTransaction transaction (TransactionGraph heads _) = if M.null matchingTrans then
                                                                  Nothing
                                                                else
                                                                  Just $ (head . M.keys) matchingTrans
  where
    matchingTrans = M.filter (transaction ==) heads
    
transactionForUUID :: UUID -> TransactionGraph -> Either RelationalError Transaction
transactionForUUID uuid graph = if S.null matchingTrans then
                                  Left $ NoSuchTransactionError uuid
                                else
                                  Right $ head (S.toList matchingTrans)
  where
    matchingTrans = S.filter (\(Transaction uuidMatch _ _) -> uuidMatch == uuid) (transactionsForGraph graph)
    
transactionsForUUIDs :: S.Set UUID -> TransactionGraph -> Either RelationalError (S.Set Transaction)    
transactionsForUUIDs uuidSet graph = S.map transactionForUUID uuidSet
  
-- the first transaction has no parent - all other do have parents- merges have two parents
parentTransactions :: Transaction -> TransactionGraph -> Either RelationalError (S.Set Transaction)
parentTransactions (Transaction _ (TransactionInfo pUUID _)) = case transactionForUUID pUUID of
  Just trans -> S.singleton trans
  Nothing -> S.empty
parentTransactions (Transaction _ (MergeTransactionInfo pUUID1 pUUID2 _)) = transactionsForUUIDs (S.fromList [pUUID1 pUUID2])
  
  
childTransactions :: Transaction -> TransactionGraph -> S.Set Transaction
childTransactions (Transaction _ _ (TransactionInfo _ children _)) = undefined
childTransactions (Transaction _ _ (MergeTransactionInfo _ _ children _)) = undefined

    
--create a disconnected transaction with a parent  
newDisconnectedTransaction :: UUID -> DatabaseContext -> DisconnectedTransaction
newDisconnectedTransaction parentUUID context = DisconnectedTransaction parentUUID context

{-
addBranch :: HeadName -> Transaction -> TransactionGraph -> Maybe TransactionGraph
addBranch newBranchName branchPoint graph@(TransactionGraph heads transSet) = if not (S.member branchPoint transSet) then Nothing else Just $ TransactionGraph newHeads transSet
 where
  newHeads = M.insert newBranchName branchPoint heads
-}

-- create a new commit and add it to the heads
-- technically, the new head could be added to an existing commit, but by adding a new commit, the new head is unambiguously linked to a new commit (with a context indentical to its parent)
addBranch :: UUID -> HeadName -> Transaction -> TransactionGraph -> Maybe (Transaction, TransactionGraph)
addBranch newUUID newBranchName branchPoint graph@(TransactionGraph heads transSet) = addTransactionToGraph newBranchName branchPoint newUUID (transactionContext branchPoint) graph

--adds a disconnected transaction to a transaction graph at some head
addDisconnectedTransaction :: UUID -> HeadName -> DisconnectedTransaction -> TransactionGraph -> Maybe (Transaction, TransactionGraph)
addDisconnectedTransaction newUUID headName dTrans@(DisconnectedTransaction parentUUID context) graph = case transactionForUUID parentUUID graph of 
  Nothing -> Nothing
  Just parentTrans ->
    addTransactionToGraph headName parentTrans newUUID context graph

-- create a new transaction on "newHeadName" with the branchPointTrans
addTransactionToGraph :: HeadName -> Transaction -> UUID -> DatabaseContext -> TransactionGraph -> Maybe (Transaction, TransactionGraph)
addTransactionToGraph newHeadName branchPointTrans@(Transaction parentUUID _ parentInfo) newUUID newContext graph@(TransactionGraph heads transSet) = do
  let freshTransaction = Transaction newUUID (transactionUUID branchPointTrans) (TransactionInfo S.empty newContext)
  -- there are two parents for MergeTransactions! not implemented
  --update parentTransaction to add child
  let parentTransaction = branchPointTrans
  let updatedTransSet = (S.insert freshTransaction <$> S.insert parentTransaction <$> S.delete parentTransaction) transSet
  let updatedGraph = TransactionGraph (M.insert newHeadName freshTransaction heads) updatedTransSet
  return (freshTransaction, updatedGraph)

--check that all forward and backward links are in place
validateGraph :: TransactionGraph -> Maybe RelationalError 
validateGraph (TransactionGraph heads transSet) = undefined
  
validateTransaction :: Transaction -> TransactionGraph -> Maybe RelationalError
validateTransaction trans@(Transaction sUUID pUUID info) = undefined
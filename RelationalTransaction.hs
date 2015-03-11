module RelationalTransaction where
import Data.UUID (UUID, nil)
import RelationType
import RelationalError
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Applicative ((<$>))
import Control.Monad

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
transactionsForUUIDs uuidSet graph = do
  transList <- forM (S.toList uuidSet) ((flip transactionForUUID) graph)
  return (S.fromList transList)

  
-- the first transaction has no parent - all other do have parents- merges have two parents
parentTransactions :: Transaction -> TransactionGraph -> Either RelationalError (S.Set Transaction)
parentTransactions (Transaction _ (TransactionInfo pUUID _) _) graph = do
  trans <- transactionForUUID pUUID graph
  return (S.singleton trans)

parentTransactions (Transaction _ (MergeTransactionInfo pUUID1 pUUID2 _) _ ) graph = transactionsForUUIDs (S.fromList [pUUID1, pUUID2]) graph
  
  
childTransactions :: Transaction -> TransactionGraph -> Either RelationalError (S.Set Transaction)
childTransactions (Transaction _ (TransactionInfo _ children) _) = transactionsForUUIDs children
childTransactions (Transaction _ (MergeTransactionInfo _ _ children) _) = transactionsForUUIDs children

    
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
addBranch :: UUID -> HeadName -> Transaction -> TransactionGraph -> Either RelationalError (Transaction, TransactionGraph)
addBranch newUUID newBranchName branchPoint graph@(TransactionGraph heads transSet) = addTransactionToGraph newBranchName branchPoint newUUID (transactionContext branchPoint) graph

--adds a disconnected transaction to a transaction graph at some head
addDisconnectedTransaction :: UUID -> HeadName -> DisconnectedTransaction -> TransactionGraph -> Either RelationalError (Transaction, TransactionGraph)
addDisconnectedTransaction newUUID headName dTrans@(DisconnectedTransaction parentUUID context) graph = do
  parentTrans <- transactionForUUID parentUUID graph                              
  addTransactionToGraph headName parentTrans newUUID context graph

-- create a new transaction on "newHeadName" with the branchPointTrans
addTransactionToGraph :: HeadName -> Transaction -> UUID -> DatabaseContext -> TransactionGraph -> Either RelationalError (Transaction, TransactionGraph)
addTransactionToGraph newHeadName branchPointTrans@(Transaction parentUUID _ parentInfo) newUUID newContext graph@(TransactionGraph heads transSet) = do
  let freshTransaction = Transaction newUUID (TransactionInfo (transactionUUID branchPointTrans) S.empty) newContext
  -- there are two parents for MergeTransactions! not implemented
  --update parentTransaction to add child
  let parentTransaction = branchPointTrans
  let updatedTransSet = (S.insert freshTransaction <$> S.insert parentTransaction <$> S.delete parentTransaction) transSet
  let updatedGraph = TransactionGraph (M.insert newHeadName freshTransaction heads) updatedTransSet
  return (freshTransaction, updatedGraph)

--check that all forward and backward links are in place
validateGraph :: TransactionGraph -> Maybe [RelationalError]
validateGraph graph@(TransactionGraph heads transSet) = do
  errors <- mapM (walkParentTransactions S.empty graph) (S.toList transSet)
  mapM (walkChildTransactions S.empty graph) (S.toList transSet)
  
--verify that all parent links exist and that all children exist
--maybe verify that all parents end at UUID nil and all children end at leaves
walkParentTransactions :: S.Set UUID -> TransactionGraph -> Transaction -> Maybe RelationalError
walkParentTransactions seenTransSet graph trans =
  let transUUID = transactionUUID trans in 
  if transUUID == nil then 
    Nothing
  else if S.member transUUID seenTransSet then
    Just $ TransactionGraphCycleError transUUID
    else 
      let parentTransSetOrError = parentTransactions trans graph in
      case parentTransSetOrError of 
        Left err -> Just err
        Right parentTransSet -> do 
          walk <- mapM (walkParentTransactions (S.insert transUUID seenTransSet) graph) (S.toList parentTransSet)
          case walk of
            err:xs -> Just err
            _ -> Nothing
  
--refactor: needless duplication in these two functions
walkChildTransactions :: S.Set UUID -> TransactionGraph -> Transaction -> Maybe RelationalError
walkChildTransactions seenTransSet graph trans = 
  let transUUID = transactionUUID trans in 
  if childTransactions trans graph == Right S.empty then
    Nothing
  else if S.member transUUID seenTransSet then
    Just $ TransactionGraphCycleError transUUID
    else 
     let childTransSetOrError = childTransactions trans graph in
     case childTransSetOrError of
       Left err -> Just err
       Right childTransSet -> do
         walk <- mapM (walkChildTransactions (S.insert transUUID seenTransSet) graph) (S.toList childTransSet)
         case walk of
           err:xs -> Just err
           _ -> Nothing
  
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module ProjectM36.TransactionGraph where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Transaction
import ProjectM36.Relation
import ProjectM36.TupleSet
import ProjectM36.Tuple
import qualified Data.Vector as V
import qualified ProjectM36.Attribute as A
import qualified Data.UUID as U
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import qualified Data.Text as T
import GHC.Generics
import Data.Binary
import ProjectM36.TransactionGraph.Merge
import Data.Either (lefts, rights, isRight)
--import Debug.Trace

-- | Record a lookup for a specific transaction in the graph.
data TransactionIdLookup = TransactionIdLookup TransactionId |
                           TransactionIdHeadNameLookup HeadName [TransactionIdHeadBacktrack]
                           deriving (Show, Eq, Binary, Generic)
                           
-- topic~3^2
data TransactionIdHeadBacktrack = TransactionIdHeadParentBacktrack Int | -- git ~: walk back n parents, arbitrarily choosing a parent when a choice must be made
                                  TransactionIdHeadBranchBacktrack Int -- git ^: walk back one parent level to the nth arbitrarily-chosen parent
                                  deriving (Show, Eq, Binary, Generic)

--operators which manipulate a transaction graph
data TransactionGraphOperator = JumpToHead HeadName  |
                                JumpToTransaction TransactionId |
                                Branch HeadName |
                                MergeTransactions MergeStrategy HeadName HeadName |
                                Commit |
                                Rollback
                              deriving (Eq, Show, Binary, Generic)

data ROTransactionGraphOperator = ShowGraph


bootstrapTransactionGraph :: TransactionId -> DatabaseContext -> TransactionGraph
bootstrapTransactionGraph freshId context = TransactionGraph bootstrapHeads bootstrapTransactions
  where
    bootstrapHeads = M.singleton "master" freshTransaction
    newSchemas = Schemas context M.empty
    freshTransaction = Transaction freshId (TransactionInfo U.nil S.empty) newSchemas
    bootstrapTransactions = S.singleton freshTransaction

emptyTransactionGraph :: TransactionGraph
emptyTransactionGraph = TransactionGraph M.empty S.empty

transactionForHead :: HeadName -> TransactionGraph -> Maybe Transaction
transactionForHead headName (TransactionGraph heads _) = M.lookup headName heads

headNameForTransaction :: Transaction -> TransactionGraph -> Maybe HeadName
headNameForTransaction transaction (TransactionGraph heads _) = if M.null matchingTrans then
                                                                  Nothing
                                                                else
                                                                  Just $ (head . M.keys) matchingTrans
  where
    matchingTrans = M.filter (transaction ==) heads

transactionForId :: TransactionId -> TransactionGraph -> Either RelationalError Transaction
transactionForId tid graph = if tid == U.nil then
                                  Left RootTransactionTraversalError
                                else if S.null matchingTrans then
                                  Left $ NoSuchTransactionError tid
                                else
                                  Right $ head (S.toList matchingTrans)
  where
    matchingTrans = S.filter (\(Transaction idMatch _ _) -> idMatch == tid) (transactionsForGraph graph)

transactionsForIds :: S.Set TransactionId -> TransactionGraph -> Either RelationalError (S.Set Transaction)
transactionsForIds idSet graph = do
  transList <- forM (S.toList idSet) ((flip transactionForId) graph)
  return (S.fromList transList)

isRootTransaction :: Transaction -> TransactionGraph -> Bool
isRootTransaction (Transaction _ (TransactionInfo pId _) _) _ = U.null pId
isRootTransaction (Transaction _ (MergeTransactionInfo _ _ _) _) _  = False

-- the first transaction has no parent - all other do have parents- merges have two parents
parentTransactions :: Transaction -> TransactionGraph -> Either RelationalError (S.Set Transaction)
parentTransactions (Transaction _ (TransactionInfo pId _) _) graph = do
  trans <- transactionForId pId graph
  return (S.singleton trans)

parentTransactions (Transaction _ (MergeTransactionInfo pId1 pId2 _) _ ) graph = transactionsForIds (S.fromList [pId1, pId2]) graph


childTransactions :: Transaction -> TransactionGraph -> Either RelationalError (S.Set Transaction)
childTransactions (Transaction _ (TransactionInfo _ children) _) = transactionsForIds children
childTransactions (Transaction _ (MergeTransactionInfo _ _ children) _) = transactionsForIds children

-- create a new commit and add it to the heads
-- technically, the new head could be added to an existing commit, but by adding a new commit, the new head is unambiguously linked to a new commit (with a context indentical to its parent)
addBranch :: TransactionId -> HeadName -> TransactionId -> TransactionGraph -> Either RelationalError (Transaction, TransactionGraph)
addBranch newId newBranchName branchPointId graph = do
  parentTrans <- transactionForId branchPointId graph
  let newTrans = Transaction newId (TransactionInfo branchPointId S.empty) (schemas parentTrans)
  addTransactionToGraph newBranchName newTrans graph

--adds a disconnected transaction to a transaction graph at some head
addDisconnectedTransaction :: TransactionId -> HeadName -> DisconnectedTransaction -> TransactionGraph -> Either RelationalError (Transaction, TransactionGraph)
addDisconnectedTransaction newId headName (DisconnectedTransaction parentId schemas') graph = addTransactionToGraph headName (Transaction newId (TransactionInfo parentId S.empty) schemas') graph


addTransactionToGraph :: HeadName -> Transaction -> TransactionGraph -> Either RelationalError (Transaction, TransactionGraph)
addTransactionToGraph headName newTrans graph = do
  let parentIds = transactionParentIds newTrans
      childIds = transactionChildIds newTrans
      newId = transactionId newTrans
      validateIds ids = mapM (\i -> transactionForId i graph) (S.toList ids)
      addChildTransaction trans = transactionSetChildren trans (S.insert newId (transactionChildIds trans))
  --validate that the parent transactions are in the graph
  _ <- validateIds parentIds
  when (S.size parentIds < 1) (Left $ NewTransactionMissingParentError newId)
  --if the headName already exists, ensure that it refers to a parent
  case transactionForHead headName graph of
    Nothing -> pure () -- any headName is OK 
    Just trans -> when (S.notMember (transactionId trans) parentIds) (Left (HeadNameSwitchingHeadProhibitedError headName))
  --validate that the transaction has no children
  when (not (S.null childIds)) (Left $ NewTransactionMayNotHaveChildrenError newId)
  --validate that the trasaction's id is unique
  when (isRight (transactionForId newId graph)) (Left (TransactionIdInUseError newId))
  --update the parent transactions to point to the new transaction
  parents <- mapM (\tid -> transactionForId tid graph) (S.toList parentIds)
  let updatedParents = S.map addChildTransaction (S.fromList parents)
      updatedTransSet = S.insert newTrans (S.union updatedParents (transactionsForGraph graph))
      updatedHeads = M.insert headName newTrans (transactionHeadsForGraph graph)
  pure (newTrans, (TransactionGraph updatedHeads updatedTransSet))

validateGraph :: TransactionGraph -> Maybe [RelationalError]
validateGraph graph@(TransactionGraph _ transSet) = do
  --check that all transaction ids are unique in the graph
  --FINISH ME!
  --uuids = map transactionId transSet
  --check that all heads appear in the transSet
  --check that all forward and backward links are in place
  _ <- mapM (walkParentTransactions S.empty graph) (S.toList transSet)
  mapM (walkChildTransactions S.empty graph) (S.toList transSet)

--verify that all parent links exist and that all children exist
--maybe verify that all parents end at transaction id nil and all children end at leaves
walkParentTransactions :: S.Set TransactionId -> TransactionGraph -> Transaction -> Maybe RelationalError
walkParentTransactions seenTransSet graph trans =
  let transId = transactionId trans in
  if transId == U.nil then
    Nothing
  else if S.member transId seenTransSet then
    Just $ TransactionGraphCycleError transId
    else
      let parentTransSetOrError = parentTransactions trans graph in
      case parentTransSetOrError of
        Left err -> Just err
        Right parentTransSet -> do
          walk <- mapM (walkParentTransactions (S.insert transId seenTransSet) graph) (S.toList parentTransSet)
          case walk of
            err:_ -> Just err
            _ -> Nothing

--refactor: needless duplication in these two functions
walkChildTransactions :: S.Set TransactionId -> TransactionGraph -> Transaction -> Maybe RelationalError
walkChildTransactions seenTransSet graph trans =
  let transId = transactionId trans in
  if childTransactions trans graph == Right S.empty then
    Nothing
  else if S.member transId seenTransSet then
    Just $ TransactionGraphCycleError transId
    else
     let childTransSetOrError = childTransactions trans graph in
     case childTransSetOrError of
       Left err -> Just err
       Right childTransSet -> do
         walk <- mapM (walkChildTransactions (S.insert transId seenTransSet) graph) (S.toList childTransSet)
         case walk of
           err:_ -> Just err
           _ -> Nothing

-- returns the new "current" transaction, updated graph, and tutorial d result
-- the current transaction is not part of the transaction graph until it is committed
evalGraphOp :: TransactionId -> DisconnectedTransaction -> TransactionGraph -> TransactionGraphOperator -> Either RelationalError (DisconnectedTransaction, TransactionGraph)

--affects only disconncted transaction
evalGraphOp _ _ graph (JumpToTransaction jumpId) = case transactionForId jumpId graph of
  Left err -> Left err
  Right parentTrans -> Right (newTrans, graph)
    where
      newTrans = DisconnectedTransaction jumpId (schemas parentTrans)

-- switch from one head to another
-- affects only disconnectedtransaction
evalGraphOp _ _ graph (JumpToHead headName) =
  case transactionForHead headName graph of
    Just newHeadTransaction -> let disconnectedTrans = DisconnectedTransaction (transactionId newHeadTransaction) (schemas newHeadTransaction) in
      Right (disconnectedTrans, graph)
    Nothing -> Left $ NoSuchHeadNameError headName
-- add new head pointing to branchPoint
-- repoint the disconnected transaction to the new branch commit (with a potentially different disconnected context)
-- affects transactiongraph and the disconnectedtransaction is recreated based off the branch
    {-
evalGraphOp newId discon@(DisconnectedTransaction parentId disconContext) graph (Branch newBranchName) = case transactionForId parentId graph of
  Nothing -> (discon, graph, DisplayErrorResult "Failed to find parent transaction.")
  Just parentTrans -> case addBranch newBranchName parentTrans graph of
    Nothing -> (discon, graph, DisplayErrorResult "Failed to add branch.")
    Just newGraph -> (newDiscon, newGraph, DisplayResult "Branched.")
     where
       newDiscon = DisconnectedTransaction (transactionId parentTrans) disconContext
-}

-- create a new commit and add it to the heads
-- technically, the new head could be added to an existing commit, but by adding a new commit, the new head is unambiguously linked to a new commit (with a context indentical to its parent)
evalGraphOp newId (DisconnectedTransaction parentId schemas') graph (Branch newBranchName) = case addBranch newId newBranchName parentId graph of
  Left err -> Left err
  Right (_, newGraph) -> Right (newDiscon, newGraph)
    where
      newDiscon = DisconnectedTransaction newId schemas'

-- add the disconnected transaction to the graph
-- affects graph and disconnectedtransaction- the new disconnectedtransaction's parent is the freshly committed transaction
evalGraphOp newTransId discon@(DisconnectedTransaction parentId schemas') graph Commit = case transactionForId parentId graph of
  Left err -> Left err
  Right parentTransaction -> case headNameForTransaction parentTransaction graph of
    Nothing -> Left $ TransactionIsNotAHeadError parentId
    Just headName -> case maybeUpdatedGraph of
      Left err-> Left err
      Right (_, updatedGraph) -> Right (newDisconnectedTrans, updatedGraph)
      where
        newDisconnectedTrans = DisconnectedTransaction newTransId schemas'
        maybeUpdatedGraph = addDisconnectedTransaction newTransId headName discon graph

-- refresh the disconnected transaction, return the same graph
evalGraphOp _ (DisconnectedTransaction parentId _) graph Rollback = case transactionForId parentId graph of
  Left err -> Left err
  Right parentTransaction -> Right (newDiscon, graph)
    where
      newDiscon = DisconnectedTransaction parentId (schemas parentTransaction)
      
-- a successful merge should remove a head
evalGraphOp newId (DisconnectedTransaction parentId _) graph (MergeTransactions mergeStrategy headNameA headNameB) = mergeTransactions newId parentId mergeStrategy (headNameA, headNameB) graph

--present a transaction graph as a relation showing the uuids, parentuuids, and flag for the current location of the disconnected transaction
graphAsRelation :: DisconnectedTransaction -> TransactionGraph -> Either RelationalError Relation
graphAsRelation (DisconnectedTransaction parentId _) graph@(TransactionGraph _ transSet) = do
  tupleMatrix <- mapM tupleGenerator (S.toList transSet)
  mkRelationFromList attrs tupleMatrix
  where
    attrs = A.attributesFromList [Attribute "id" TextAtomType,
                                  Attribute "parents" (RelationAtomType parentAttributes),
                                  Attribute "current" BoolAtomType,
                                  Attribute "head" TextAtomType
                                 ]
    parentAttributes = A.attributesFromList [Attribute "id" TextAtomType]
    tupleGenerator transaction = case transactionParentsRelation transaction graph of
      Left err -> Left err
      Right parentTransRel -> Right [TextAtom $ T.pack $ show (transactionId transaction),
                                     RelationAtom parentTransRel,
                                     BoolAtom $ parentId == transactionId transaction,
                                     TextAtom $ case headNameForTransaction transaction graph of
                                       Just headName -> headName
                                       Nothing -> ""
                                      ]

transactionParentsRelation :: Transaction -> TransactionGraph -> Either RelationalError Relation
transactionParentsRelation trans graph = do
  if isRootTransaction trans graph then do
    mkRelation attrs emptyTupleSet
    else do
      parentTransSet <- parentTransactions trans graph
      let tuples = map trans2tuple (S.toList parentTransSet)
      mkRelationFromTuples attrs tuples
  where
    attrs = A.attributesFromList [Attribute "id" TextAtomType]
    trans2tuple trans2 = mkRelationTuple attrs $ V.singleton (TextAtom (T.pack (show $ transactionId trans2)))

{-
--display transaction graph as relation
evalROGraphOp :: DisconnectedTransaction -> TransactionGraph -> ROTransactionGraphOperator -> Either RelationalError Relation
evalROGraphOp discon graph ShowGraph = do
  graphRel <- graphAsRelation discon graph
  return graphRel
-}

-- | Execute the merge strategy against the transactions, returning a new transaction which can be then added to the transaction graph
createMergeTransaction :: TransactionId -> MergeStrategy -> TransactionGraph -> (Transaction, Transaction) -> Either MergeError Transaction
createMergeTransaction newId (SelectedBranchMergeStrategy selectedBranch) graph t2@(trans1, trans2) = do
  selectedTrans <- validateHeadName selectedBranch graph t2
  pure $ Transaction newId (MergeTransactionInfo (transactionId trans1) (transactionId trans2) S.empty) (schemas selectedTrans)
                       
-- merge functions, relvars, individually
createMergeTransaction newId strat@UnionMergeStrategy graph t2 = createUnionMergeTransaction newId strat graph t2

-- merge function, relvars, but, on error, just take the component from the preferred branch
createMergeTransaction newId strat@(UnionPreferMergeStrategy _) graph t2 = createUnionMergeTransaction newId strat graph t2

-- | Returns the correct Transaction for the branch name in the graph and ensures that it is one of the two transaction arguments in the tuple.
validateHeadName :: HeadName -> TransactionGraph -> (Transaction, Transaction) -> Either MergeError Transaction
validateHeadName headName graph (t1, t2) = do
  case transactionForHead headName graph of
    Nothing -> Left SelectedHeadMismatchMergeError
    Just trans -> if trans /= t1 && trans /= t2 then 
                    Left SelectedHeadMismatchMergeError 
                  else
                    Right trans
  
-- Algorithm: start at one transaction and work backwards up the parents. If there is a node we have not yet visited as a child, then walk that up to its head. If that branch contains the goal transaction, then we have completed a valid subgraph traversal.
subGraphOfFirstCommonAncestor :: TransactionGraph -> TransactionHeads -> Transaction -> Transaction -> S.Set Transaction -> Either RelationalError TransactionGraph
subGraphOfFirstCommonAncestor origGraph resultHeads currentTrans goalTrans traverseSet = do
  let currentid = transactionId currentTrans
      goalid = transactionId goalTrans
  if currentTrans == goalTrans then
    Right (TransactionGraph resultHeads traverseSet) -- add filter
    --catch root transaction to improve error?
    else do
    currentTransChildren <- liftM S.fromList $ mapM (flip transactionForId origGraph) (S.toList (transactionChildIds currentTrans))            
    let searchChildren = S.difference (S.insert currentTrans traverseSet) currentTransChildren
        searchChild start = pathToTransaction origGraph start goalTrans (S.insert currentTrans traverseSet)
        childSearches = map searchChild (S.toList searchChildren)
        errors = lefts childSearches
        pathsFound = rights childSearches
        realErrors = filter (/= (FailedToFindTransactionError goalid)) errors
    -- report any non-search-related errors        
    when (not (null realErrors)) (Left (head realErrors))
    -- if no paths found, search the parent
    if null pathsFound then
      case oneParent currentTrans of
        Left RootTransactionTraversalError -> Left (NoCommonTransactionAncestorError currentid goalid)
        Left err -> Left err
        Right currentTransParent -> do      
          subGraphOfFirstCommonAncestor origGraph resultHeads currentTransParent goalTrans (S.insert currentTrans traverseSet)
      else -- we found a path
      Right (TransactionGraph resultHeads (S.unions (traverseSet : pathsFound)))
  where
    oneParent (Transaction _ (TransactionInfo parentId _) _) = transactionForId parentId origGraph
    oneParent (Transaction _ (MergeTransactionInfo parentId _ _) _) = transactionForId parentId origGraph

-- | Search from a past graph point to all following heads for a specific transaction. If found, return the transaction path, otherwise a RelationalError.
pathToTransaction :: TransactionGraph -> Transaction -> Transaction -> S.Set Transaction -> Either RelationalError (S.Set Transaction)
pathToTransaction graph currentTransaction targetTransaction accumTransSet = do
  let targetId = transactionId targetTransaction
  if transactionId targetTransaction == transactionId currentTransaction then do
    Right accumTransSet
    else do
    currentTransChildren <- mapM (flip transactionForId graph) (S.toList (transactionChildIds currentTransaction))        
    if length currentTransChildren == 0 then
      Left (FailedToFindTransactionError targetId)
      else do
      let searches = map (\t -> pathToTransaction graph t targetTransaction (S.insert t accumTransSet)) currentTransChildren
      let realErrors = filter (/= FailedToFindTransactionError targetId) (lefts searches)
          paths = rights searches
      if length realErrors > 0 then -- found some real errors
        Left (head realErrors)
      else if length paths == 0 then -- failed to find transaction in all children
             Left (FailedToFindTransactionError targetId)
           else --we have some paths!
             Right (S.unions paths)

mergeTransactions :: TransactionId -> TransactionId -> MergeStrategy -> (HeadName, HeadName) -> TransactionGraph -> Either RelationalError (DisconnectedTransaction, TransactionGraph)
mergeTransactions newId parentId mergeStrategy (headNameA, headNameB) graph = do
  let transactionForHeadErr name = case transactionForHead name graph of
        Nothing -> Left (NoSuchHeadNameError name)
        Just t -> Right t
  transA <- transactionForHeadErr headNameA 
  transB <- transactionForHeadErr headNameB 
  disconParent <- transactionForId parentId graph
  let subHeads = M.filterWithKey (\k _ -> elem k [headNameA, headNameB]) (transactionHeadsForGraph graph)
  subGraph <- subGraphOfFirstCommonAncestor graph subHeads transA transB S.empty
  subGraph' <- filterSubGraph subGraph subHeads
  case createMergeTransaction newId mergeStrategy subGraph' (transA, transB) of
    Left err -> Left (MergeTransactionError err)
    Right mergedTrans -> case headNameForTransaction disconParent graph of
      Nothing -> Left (TransactionIsNotAHeadError parentId)
      Just headName -> do 
        (newTrans, newGraph) <- addTransactionToGraph headName mergedTrans graph
        let newGraph' = TransactionGraph (transactionHeadsForGraph newGraph) (transactionsForGraph newGraph)
            newDiscon = DisconnectedTransaction newId (schemas newTrans)
        pure (newDiscon, newGraph')
  
--TEMPORARY COPY/PASTE  
showTransactionStructureX :: Transaction -> TransactionGraph -> String
showTransactionStructureX trans graph = headInfo ++ " " ++ show (transactionId trans) ++ " " ++ parentTransactionsInfo
  where
    headInfo = maybe "" show (headNameForTransaction trans graph)
    parentTransactionsInfo = if isRootTransaction trans graph then "root" else case parentTransactions trans graph of
      Left err -> show err
      Right parentTransSet -> concat $ S.toList $ S.map (show . transactionId) parentTransSet
  
showGraphStructureX :: TransactionGraph -> String
showGraphStructureX graph@(TransactionGraph heads transSet) = headsInfo ++ S.foldr folder "" transSet
  where
    folder trans acc = acc ++ showTransactionStructureX trans graph ++ "\n"
    headsInfo = show $ M.map transactionId heads
    
-- | After splicing out a subgraph, run it through this function to remove references to transactions which are not in the subgraph.
filterSubGraph :: TransactionGraph -> TransactionHeads -> Either RelationalError TransactionGraph
filterSubGraph graph heads = Right $ TransactionGraph newHeads newTransSet
  where
    validIds = S.map transactionId (transactionsForGraph graph)
    newTransSet = S.map (filterTransaction validIds) (transactionsForGraph graph)
    newHeads = M.map (filterTransaction validIds) heads
    
--helper function for commonalities in union merge
createUnionMergeTransaction :: TransactionId -> MergeStrategy -> TransactionGraph -> (Transaction, Transaction) -> Either MergeError Transaction
createUnionMergeTransaction newId strategy graph (t1,t2) = do
  let contextA = concreteDatabaseContext t1
      contextB = concreteDatabaseContext t2
  
  preference <- case strategy of 
    UnionMergeStrategy -> pure PreferNeither
    UnionPreferMergeStrategy preferBranch -> do
      case transactionForHead preferBranch graph of
        Nothing -> Left (PreferredHeadMissingMergeError preferBranch)
        Just preferredTrans -> pure $ if t1 == preferredTrans then PreferFirst else PreferSecond
    badStrat -> Left (InvalidMergeStrategyError badStrat)
          
  incDeps <- unionMergeMaps preference (inclusionDependencies contextA) (inclusionDependencies contextB)
  relVars <- unionMergeRelVars preference (relationVariables contextA) (relationVariables contextB)
  atomFuncs <- unionMergeAtomFunctions preference (atomFunctions contextA) (atomFunctions contextB)
  notifs <- unionMergeMaps preference (notifications contextA) (notifications contextB)
  types <- unionMergeTypeConstructorMapping preference (typeConstructorMapping contextA) (typeConstructorMapping contextB)
  -- TODO: add merge of subschemas
  let newContext = DatabaseContext incDeps relVars atomFuncs notifs types
      newSchemas = Schemas newContext (subschemas t1)
  pure (Transaction newId (MergeTransactionInfo (transactionId t1) (transactionId t2) S.empty) newSchemas)

lookupTransaction :: TransactionGraph -> TransactionIdLookup -> Either RelationalError Transaction
lookupTransaction graph (TransactionIdLookup tid) = transactionForId tid graph
lookupTransaction graph (TransactionIdHeadNameLookup headName backtracks) = case transactionForHead headName graph of 
  Nothing -> Left (NoSuchHeadNameError headName)
  Just headTrans -> do
    traversedId <- traverseGraph graph (transactionId headTrans) backtracks
    transactionForId traversedId graph
    
traverseGraph :: TransactionGraph -> TransactionId -> [TransactionIdHeadBacktrack] -> Either RelationalError TransactionId
traverseGraph graph currentTid backtrackSteps = foldM (backtrackGraph graph) currentTid backtrackSteps
             
backtrackGraph :: TransactionGraph -> TransactionId -> TransactionIdHeadBacktrack -> Either RelationalError TransactionId
-- tilde, step back one parent link- if a choice must be made, choose the "first" link arbitrarily
backtrackGraph graph currentTid (TransactionIdHeadParentBacktrack steps) = do
  trans <- transactionForId currentTid graph
  let parents = S.toAscList (transactionParentIds trans)
  if length parents < 1 then
    Left RootTransactionTraversalError
    else do
    parentTrans <- transactionForId (head parents) graph
    if steps == 1 then do
      pure (transactionId parentTrans)
      else
      backtrackGraph graph (transactionId parentTrans) (TransactionIdHeadParentBacktrack (steps - 1))
  
backtrackGraph graph currentTid (TransactionIdHeadBranchBacktrack steps) = do
  trans <- transactionForId currentTid graph
  let parents = transactionParentIds trans
  if S.size parents < 1 then
    Left RootTransactionTraversalError    
    else if S.size parents < steps then
           Left (ParentCountTraversalError (S.size parents) steps)
         else
           pure (S.elemAt (steps - 1) parents)
    
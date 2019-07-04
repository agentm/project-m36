{-# LANGUAGE DeriveAnyClass, DeriveGeneric, CPP #-}
module ProjectM36.TransactionGraph where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Transaction
import ProjectM36.Relation
import ProjectM36.TupleSet
import ProjectM36.Tuple
import ProjectM36.RelationalExpression
import ProjectM36.TransactionGraph.Merge
import qualified ProjectM36.DisconnectedTransaction as Discon

import qualified Data.Vector as V
import qualified ProjectM36.Attribute as A
import qualified Data.UUID as U
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import Data.Time.Clock
import qualified Data.Text as T
import GHC.Generics
import Data.Binary
import Data.Either (lefts, rights, isRight)
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif
import Control.Arrow
import Data.Maybe

{-
--import Debug.Trace
import Control.Monad.Reader
import qualified ProjectM36.DisconnectedTransaction as D
-}

-- | Record a lookup for a specific transaction in the graph.
data TransactionIdLookup = TransactionIdLookup TransactionId |
                           TransactionIdHeadNameLookup HeadName [TransactionIdHeadBacktrack]
                           deriving (Show, Eq, Binary, Generic)
                           
-- | Used for git-style head backtracking such as topic~3^2.
data TransactionIdHeadBacktrack = TransactionIdHeadParentBacktrack Int | -- ^ git equivalent of ~: walk back n parents, arbitrarily choosing a parent when a choice must be made
                                  TransactionIdHeadBranchBacktrack Int | -- ^ git equivalent of ^: walk back one parent level to the nth arbitrarily-chosen parent 
                                  TransactionStampHeadBacktrack UTCTime -- ^ git equivalent of 'git-rev-list -n 1 --before X' find the first transaction which was created before the timestamp
                                  deriving (Show, Eq, Binary, Generic)

  
-- | Operators which manipulate a transaction graph and which transaction the current 'Session' is based upon.
data TransactionGraphOperator = JumpToHead HeadName  |
                                JumpToTransaction TransactionId |
                                WalkBackToTime UTCTime |
                                Branch HeadName |
                                DeleteBranch HeadName |
                                MergeTransactions MergeStrategy HeadName HeadName |
                                Commit |
                                Rollback
                              deriving (Eq, Show, Binary, Generic)
                                       
isCommit :: TransactionGraphOperator -> Bool                                       
isCommit Commit = True
isCommit _ = False
                                       
data ROTransactionGraphOperator = ShowGraph
                                  deriving Show

bootstrapTransactionGraph :: UTCTime -> TransactionId -> DatabaseContext -> TransactionGraph
bootstrapTransactionGraph stamp freshId context = TransactionGraph bootstrapHeads bootstrapTransactions
  where
    bootstrapHeads = M.singleton "master" freshTransaction
    newSchemas = Schemas context M.empty
    freshTransaction = Transaction freshId (TransactionInfo U.nil S.empty stamp) newSchemas
    bootstrapTransactions = S.singleton freshTransaction

emptyTransactionGraph :: TransactionGraph
emptyTransactionGraph = TransactionGraph M.empty S.empty

transactionForHead :: HeadName -> TransactionGraph -> Maybe Transaction
transactionForHead headName graph = M.lookup headName (transactionHeadsForGraph graph)

headList :: TransactionGraph -> [(HeadName, TransactionId)]
headList graph = map (second transactionId) (M.assocs (transactionHeadsForGraph graph))

headNameForTransaction :: Transaction -> TransactionGraph -> Maybe HeadName
headNameForTransaction transaction (TransactionGraph heads _) = if M.null matchingTrans then
                                                                  Nothing
                                                                else
                                                                  Just $ (head . M.keys) matchingTrans
  where
    matchingTrans = M.filter (transaction ==) heads

transactionForId :: TransactionId -> TransactionGraph -> Either RelationalError Transaction
transactionForId tid graph 
  | tid == U.nil =
    Left RootTransactionTraversalError
  | S.null matchingTrans =
    Left $ NoSuchTransactionError tid
  | otherwise =
    Right $ head (S.toList matchingTrans)
  where
    matchingTrans = S.filter (\(Transaction idMatch _ _) -> idMatch == tid) (transactionsForGraph graph)

transactionsForIds :: S.Set TransactionId -> TransactionGraph -> Either RelationalError (S.Set Transaction)
transactionsForIds idSet graph =
  S.fromList <$> forM (S.toList idSet) (`transactionForId` graph)

isRootTransaction :: Transaction -> TransactionGraph -> Bool
isRootTransaction (Transaction _ (TransactionInfo pId _ _) _) _ = U.null pId
isRootTransaction (Transaction _ MergeTransactionInfo{} _) _  = False

-- the first transaction has no parent - all other do have parents- merges have two parents
parentTransactions :: Transaction -> TransactionGraph -> Either RelationalError (S.Set Transaction)
parentTransactions (Transaction _ (TransactionInfo pId _ _) _) graph = 
  S.singleton <$> transactionForId pId graph


parentTransactions (Transaction _ (MergeTransactionInfo pId1 pId2 _ _) _ ) graph = transactionsForIds (S.fromList [pId1, pId2]) graph


childTransactions :: Transaction -> TransactionGraph -> Either RelationalError (S.Set Transaction)
childTransactions (Transaction _ (TransactionInfo _ children _) _) = transactionsForIds children
childTransactions (Transaction _ (MergeTransactionInfo _ _ children _) _) = transactionsForIds children

-- create a new commit and add it to the heads
-- technically, the new head could be added to an existing commit, but by adding a new commit, the new head is unambiguously linked to a new commit (with a context indentical to its parent)
addBranch :: UTCTime -> TransactionId -> HeadName -> TransactionId -> TransactionGraph -> Either RelationalError (Transaction, TransactionGraph)
addBranch stamp newId newBranchName branchPointId graph = do
  parentTrans <- transactionForId branchPointId graph
  let newTrans = Transaction newId (TransactionInfo branchPointId S.empty stamp) (schemas parentTrans)
  addTransactionToGraph newBranchName newTrans graph

--adds a disconnected transaction to a transaction graph at some head
addDisconnectedTransaction :: UTCTime -> TransactionId -> HeadName -> DisconnectedTransaction -> TransactionGraph -> Either RelationalError (Transaction, TransactionGraph)
addDisconnectedTransaction stamp newId headName (DisconnectedTransaction parentId schemas' _) = addTransactionToGraph headName (Transaction newId (TransactionInfo parentId S.empty stamp) schemas')


addTransactionToGraph :: HeadName -> Transaction -> TransactionGraph -> Either RelationalError (Transaction, TransactionGraph)
addTransactionToGraph headName newTrans graph = do
  let parentIds = transactionParentIds newTrans
      childIds = transactionChildIds newTrans
      newId = transactionId newTrans
      validateIds ids = mapM (`transactionForId` graph) (S.toList ids)
      addChildTransaction trans = transactionSetChildren trans (S.insert newId (transactionChildIds trans))
  --validate that the parent transactions are in the graph
  _ <- validateIds parentIds
  when (S.size parentIds < 1) (Left $ NewTransactionMissingParentError newId)
  --if the headName already exists, ensure that it refers to a parent
  case transactionForHead headName graph of
    Nothing -> pure () -- any headName is OK 
    Just trans -> when (S.notMember (transactionId trans) parentIds) (Left (HeadNameSwitchingHeadProhibitedError headName))
  --validate that the transaction has no children
  unless (S.null childIds) (Left $ NewTransactionMayNotHaveChildrenError newId)
  --validate that the trasaction's id is unique
  when (isRight (transactionForId newId graph)) (Left (TransactionIdInUseError newId))
  --update the parent transactions to point to the new transaction
  parents <- mapM (`transactionForId` graph) (S.toList parentIds)
  let updatedParents = S.map addChildTransaction (S.fromList parents)
      updatedTransSet = S.insert newTrans (S.union updatedParents (transactionsForGraph graph))
      updatedHeads = M.insert headName newTrans (transactionHeadsForGraph graph)
  pure (newTrans, TransactionGraph updatedHeads updatedTransSet)

validateGraph :: TransactionGraph -> Maybe [RelationalError]
validateGraph graph@(TransactionGraph _ transSet) = do
  --check that all transaction ids are unique in the graph
  --FINISH ME!
  --uuids = map transactionId transSet
  --check that all heads appear in the transSet
  --check that all forward and backward links are in place
  mapM_ (walkParentTransactions S.empty graph) (S.toList transSet)
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
evalGraphOp :: UTCTime -> TransactionId -> DisconnectedTransaction -> TransactionGraph -> TransactionGraphOperator -> Either RelationalError (DisconnectedTransaction, TransactionGraph)

evalGraphOp _ _ _ graph (JumpToTransaction jumpId) = case transactionForId jumpId graph of
  Left err -> Left err
  Right parentTrans -> Right (newTrans, graph)
    where
      newTrans = DisconnectedTransaction jumpId (schemas parentTrans) False

-- switch from one head to another
evalGraphOp _ _ _ graph (JumpToHead headName) =
  case transactionForHead headName graph of
    Just newHeadTransaction -> let disconnectedTrans = DisconnectedTransaction (transactionId newHeadTransaction) (schemas newHeadTransaction) False in
      Right (disconnectedTrans, graph)
    Nothing -> Left $ NoSuchHeadNameError headName
    
evalGraphOp _ _ discon graph (WalkBackToTime backTime) = do
  let startTransId = Discon.parentId discon
  jumpDest <- backtrackGraph graph startTransId (TransactionStampHeadBacktrack backTime) 
  case transactionForId jumpDest graph of
    Left err -> Left err
    Right trans -> do
      let disconnectedTrans = DisconnectedTransaction (transactionId trans) (schemas trans) False
      Right (disconnectedTrans, graph)
              
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
evalGraphOp stamp newId (DisconnectedTransaction parentId schemas' _) graph (Branch newBranchName) = do
  let newDiscon = DisconnectedTransaction newId schemas' False
  case addBranch stamp newId newBranchName parentId graph of
    Left err -> Left err
    Right (_, newGraph) -> Right (newDiscon, newGraph)
  
-- add the disconnected transaction to the graph
-- affects graph and disconnectedtransaction- the new disconnectedtransaction's parent is the freshly committed transaction
evalGraphOp stamp newTransId discon@(DisconnectedTransaction parentId schemas' _) graph Commit = case transactionForId parentId graph of
  Left err -> Left err
  Right parentTransaction -> case headNameForTransaction parentTransaction graph of
    Nothing -> Left $ TransactionIsNotAHeadError parentId
    Just headName -> case maybeUpdatedGraph of
      Left err-> Left err
      Right (_, updatedGraph) -> Right (newDisconnectedTrans, updatedGraph)
      where
        newDisconnectedTrans = DisconnectedTransaction newTransId schemas' False
        maybeUpdatedGraph = addDisconnectedTransaction stamp newTransId headName discon graph

-- refresh the disconnected transaction, return the same graph
evalGraphOp _ _ (DisconnectedTransaction parentId _ _) graph Rollback = case transactionForId parentId graph of
  Left err -> Left err
  Right parentTransaction -> Right (newDiscon, graph)
    where
      newDiscon = DisconnectedTransaction parentId (schemas parentTransaction) False
      
evalGraphOp stamp newId (DisconnectedTransaction parentId _ _) graph (MergeTransactions mergeStrategy headNameA headNameB) = mergeTransactions stamp newId parentId mergeStrategy (headNameA, headNameB) graph

evalGraphOp _ _ discon graph@(TransactionGraph graphHeads transSet) (DeleteBranch branchName) = case transactionForHead branchName graph of
  Nothing -> Left (NoSuchHeadNameError branchName)
  Just _ -> Right (discon, TransactionGraph (M.delete branchName graphHeads) transSet)

--present a transaction graph as a relation showing the uuids, parentuuids, and flag for the current location of the disconnected transaction
graphAsRelation :: DisconnectedTransaction -> TransactionGraph -> Either RelationalError Relation
graphAsRelation (DisconnectedTransaction parentId _ _) graph@(TransactionGraph _ transSet) = do
  tupleMatrix <- mapM tupleGenerator (S.toList transSet)
  mkRelationFromList attrs tupleMatrix
  where
    attrs = A.attributesFromList [Attribute "id" TextAtomType,
                                  Attribute "stamp" DateTimeAtomType,
                                  Attribute "parents" (RelationAtomType parentAttributes),
                                  Attribute "current" BoolAtomType,
                                  Attribute "head" TextAtomType
                                 ]
    parentAttributes = A.attributesFromList [Attribute "id" TextAtomType]
    tupleGenerator transaction = case transactionParentsRelation transaction graph of
      Left err -> Left err
      Right parentTransRel -> Right [TextAtom $ T.pack $ show (transactionId transaction),
                                     DateTimeAtom (transactionTimestamp transaction),
                                     RelationAtom parentTransRel,
                                     BoolAtom $ parentId == transactionId transaction,
                                     TextAtom $ fromMaybe "" (headNameForTransaction transaction graph)
                                      ]

transactionParentsRelation :: Transaction -> TransactionGraph -> Either RelationalError Relation
transactionParentsRelation trans graph = 
  if isRootTransaction trans graph then    
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
createMergeTransaction :: UTCTime -> TransactionId -> MergeStrategy -> TransactionGraph -> (Transaction, Transaction) -> Either MergeError Transaction
createMergeTransaction stamp newId (SelectedBranchMergeStrategy selectedBranch) graph t2@(trans1, trans2) = do
  let selectedTrans = validateHeadName selectedBranch graph t2
  Transaction newId (MergeTransactionInfo (transactionId trans1) (transactionId trans2) S.empty stamp) . schemas <$> selectedTrans
                       
-- merge functions, relvars, individually
createMergeTransaction stamp newId strat@UnionMergeStrategy graph t2 = createUnionMergeTransaction stamp newId strat graph t2

-- merge function, relvars, but, on error, just take the component from the preferred branch
createMergeTransaction stamp newId strat@(UnionPreferMergeStrategy _) graph t2 = createUnionMergeTransaction stamp newId strat graph t2

-- | Returns the correct Transaction for the branch name in the graph and ensures that it is one of the two transaction arguments in the tuple.
validateHeadName :: HeadName -> TransactionGraph -> (Transaction, Transaction) -> Either MergeError Transaction
validateHeadName headName graph (t1, t2) =
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
    currentTransChildren <- S.fromList <$> mapM (`transactionForId` origGraph) (S.toList (transactionChildIds currentTrans))            
    let searchChildren = S.difference (S.insert currentTrans traverseSet) currentTransChildren
        searchChild start = pathToTransaction origGraph start goalTrans (S.insert currentTrans traverseSet)
        childSearches = map searchChild (S.toList searchChildren)
        errors = lefts childSearches
        pathsFound = rights childSearches
        realErrors = filter (/= FailedToFindTransactionError goalid) errors
    -- report any non-search-related errors        
    unless (null realErrors) (Left (head realErrors))
    -- if no paths found, search the parent
    if null pathsFound then
      case oneParent currentTrans of
        Left RootTransactionTraversalError -> Left (NoCommonTransactionAncestorError currentid goalid)
        Left err -> Left err
        Right currentTransParent ->
          subGraphOfFirstCommonAncestor origGraph resultHeads currentTransParent goalTrans (S.insert currentTrans traverseSet)
      else -- we found a path
      Right (TransactionGraph resultHeads (S.unions (traverseSet : pathsFound)))
  where
    oneParent (Transaction _ (TransactionInfo parentId _ _) _) = transactionForId parentId origGraph
    oneParent (Transaction _ (MergeTransactionInfo parentId _ _ _) _) = transactionForId parentId origGraph

-- | Search from a past graph point to all following heads for a specific transaction. If found, return the transaction path, otherwise a RelationalError.
pathToTransaction :: TransactionGraph -> Transaction -> Transaction -> S.Set Transaction -> Either RelationalError (S.Set Transaction)
pathToTransaction graph currentTransaction targetTransaction accumTransSet = do
  let targetId = transactionId targetTransaction
  if transactionId targetTransaction == transactionId currentTransaction then
    Right accumTransSet
    else do
    currentTransChildren <- mapM (`transactionForId` graph) (S.toList (transactionChildIds currentTransaction))        
    if null currentTransChildren then
      Left (FailedToFindTransactionError targetId)
      else do
      let searches = map (\t -> pathToTransaction graph t targetTransaction (S.insert t accumTransSet)) currentTransChildren
      let realErrors = filter (/= FailedToFindTransactionError targetId) (lefts searches)
          paths = rights searches
      if not (null realErrors) then -- found some real errors
        Left (head realErrors)
      else if null paths then -- failed to find transaction in all children
             Left (FailedToFindTransactionError targetId)
           else --we have some paths!
             Right (S.unions paths)

mergeTransactions :: UTCTime -> TransactionId -> TransactionId -> MergeStrategy -> (HeadName, HeadName) -> TransactionGraph -> Either RelationalError (DisconnectedTransaction, TransactionGraph)
mergeTransactions stamp newId parentId mergeStrategy (headNameA, headNameB) graph = do
  let transactionForHeadErr name = case transactionForHead name graph of
        Nothing -> Left (NoSuchHeadNameError name)
        Just t -> Right t
  transA <- transactionForHeadErr headNameA 
  transB <- transactionForHeadErr headNameB 
  disconParent <- transactionForId parentId graph
  let subHeads = M.filterWithKey (\k _ -> k `elem` [headNameA, headNameB]) (transactionHeadsForGraph graph)
  subGraph <- subGraphOfFirstCommonAncestor graph subHeads transA transB S.empty
  subGraph' <- filterSubGraph subGraph subHeads
  case createMergeTransaction stamp newId mergeStrategy subGraph' (transA, transB) of
    Left err -> Left (MergeTransactionError err)
    Right mergedTrans -> case checkConstraints (concreteDatabaseContext mergedTrans) of
      Left err -> Left err
      Right _ -> case headNameForTransaction disconParent graph of
        Nothing -> Left (TransactionIsNotAHeadError parentId)
        Just headName -> do
          (newTrans, newGraph) <- addTransactionToGraph headName mergedTrans graph
          let newGraph' = TransactionGraph (transactionHeadsForGraph newGraph) (transactionsForGraph newGraph)
              newDiscon = DisconnectedTransaction newId (schemas newTrans) False
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
createUnionMergeTransaction :: UTCTime -> TransactionId -> MergeStrategy -> TransactionGraph -> (Transaction, Transaction) -> Either MergeError Transaction
createUnionMergeTransaction stamp newId strategy graph (t1,t2) = do
  let contextA = concreteDatabaseContext t1
      contextB = concreteDatabaseContext t2
  
  preference <- case strategy of 
    UnionMergeStrategy -> pure PreferNeither
    UnionPreferMergeStrategy preferBranch ->
      case transactionForHead preferBranch graph of
        Nothing -> Left (PreferredHeadMissingMergeError preferBranch)
        Just preferredTrans -> pure $ if t1 == preferredTrans then PreferFirst else PreferSecond
    badStrat -> Left (InvalidMergeStrategyError badStrat)
          
  incDeps <- unionMergeMaps preference (inclusionDependencies contextA) (inclusionDependencies contextB)
  relVars <- unionMergeRelVars preference (relationVariables contextA) (relationVariables contextB)
  atomFuncs <- unionMergeAtomFunctions preference (atomFunctions contextA) (atomFunctions contextB)
  notifs <- unionMergeMaps preference (notifications contextA) (notifications contextB)
  types <- unionMergeTypeConstructorMapping preference (typeConstructorMapping contextA) (typeConstructorMapping contextB)
  dbcFuncs <- unionMergeDatabaseContextFunctions preference (dbcFunctions contextA) (dbcFunctions contextB)
  -- TODO: add merge of subschemas
  let newContext = DatabaseContext {
        inclusionDependencies = incDeps, 
        relationVariables = relVars, 
        atomFunctions = atomFuncs, 
        dbcFunctions = dbcFuncs,
        notifications = notifs,
        typeConstructorMapping = types
        }
      newSchemas = Schemas newContext (subschemas t1)
  pure (Transaction newId (MergeTransactionInfo (transactionId t1) (transactionId t2) S.empty stamp) newSchemas)

lookupTransaction :: TransactionGraph -> TransactionIdLookup -> Either RelationalError Transaction
lookupTransaction graph (TransactionIdLookup tid) = transactionForId tid graph
lookupTransaction graph (TransactionIdHeadNameLookup headName backtracks) = case transactionForHead headName graph of 
  Nothing -> Left (NoSuchHeadNameError headName)
  Just headTrans -> do
    traversedId <- traverseGraph graph (transactionId headTrans) backtracks
    transactionForId traversedId graph
    
traverseGraph :: TransactionGraph -> TransactionId -> [TransactionIdHeadBacktrack] -> Either RelationalError TransactionId
traverseGraph graph = foldM (backtrackGraph graph)
             
backtrackGraph :: TransactionGraph -> TransactionId -> TransactionIdHeadBacktrack -> Either RelationalError TransactionId
-- tilde, step back one parent link- if a choice must be made, choose the "first" link arbitrarily
backtrackGraph graph currentTid (TransactionIdHeadParentBacktrack steps) = do
  trans <- transactionForId currentTid graph
  let parents = S.toAscList (transactionParentIds trans)
  if null parents then
    Left RootTransactionTraversalError
    else do
    parentTrans <- transactionForId (head parents) graph
    if steps == 1 then
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
           
backtrackGraph graph currentTid btrack@(TransactionStampHeadBacktrack stamp) = do           
  trans <- transactionForId currentTid graph
  let parents = transactionParentIds trans
  if transactionTimestamp trans <= stamp then
    pure currentTid
    else if S.null parents then
           Left RootTransactionTraversalError
         else
           let arbitraryParent = head (S.toList parents) in
           backtrackGraph graph arbitraryParent btrack
    
-- | Create a temporary branch for commit, merge the result to head, delete the temporary branch. This is useful to atomically commit a transaction, avoiding a TransactionIsNotHeadError but trading it for a potential MergeError.
--this is not a GraphOp because it combines multiple graph operations
autoMergeToHead :: UTCTime -> (TransactionId, TransactionId, TransactionId) -> DisconnectedTransaction -> HeadName -> MergeStrategy -> TransactionGraph -> Either RelationalError (DisconnectedTransaction, TransactionGraph)
autoMergeToHead stamp (tempBranchTransId, tempCommitTransId, mergeTransId) discon mergeToHeadName strat graph = do
  let tempBranchName = "mergebranch_" <> U.toText tempBranchTransId
  --create the temp branch
  (discon', graph') <- evalGraphOp stamp tempBranchTransId discon graph (Branch tempBranchName)
  
  --commit to the new branch- possible future optimization: don't require fsync for this- create a temp commit type
  (discon'', graph'') <- evalGraphOp stamp tempCommitTransId discon' graph' Commit
 
  --jump to merge head
  (discon''', graph''') <- evalGraphOp stamp tempBranchTransId discon'' graph'' (JumpToHead mergeToHeadName)
  
  --create the merge
  (discon'''', graph'''') <- evalGraphOp stamp mergeTransId discon''' graph''' (MergeTransactions strat tempBranchName mergeToHeadName)
  
  --delete the temp branch
  (discon''''', graph''''') <- evalGraphOp stamp tempBranchTransId discon'''' graph'''' (DeleteBranch tempBranchName)
  {-
  let rel = runReader (evalRelationalExpr (RelationVariable "s" ())) (mkRelationalExprState $ D.concreteDatabaseContext discon'''')
  traceShowM rel
-}
  
  pure (discon''''', graph''''')

  

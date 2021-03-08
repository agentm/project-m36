{-# LANGUAGE DeriveGeneric, CPP, FlexibleContexts, DerivingVia #-}
module ProjectM36.TransactionGraph where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Transaction
import ProjectM36.TransactionInfo as TI
import ProjectM36.Relation
import ProjectM36.TupleSet
import ProjectM36.Tuple
import ProjectM36.RelationalExpression
import ProjectM36.TransactionGraph.Merge
import ProjectM36.MerkleHash
import qualified ProjectM36.DisconnectedTransaction as Discon
import qualified ProjectM36.Attribute as A

import Codec.Winery
import Control.Monad.Except hiding (join)
import Control.Monad.Reader hiding (join)
import qualified Data.Vector as V
import qualified Data.UUID as U
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import Data.Time.Clock
import qualified Data.Text as T
import GHC.Generics
import Data.Either (lefts, rights, isRight)
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif
import Control.Arrow
import Data.Maybe
import Data.UUID.V4
import qualified Data.ByteString.Lazy as BL
import ProjectM36.DatabaseContext as DBC
import Crypto.Hash.SHA256

-- | Record a lookup for a specific transaction in the graph.
data TransactionIdLookup = TransactionIdLookup TransactionId |
                           TransactionIdHeadNameLookup HeadName [TransactionIdHeadBacktrack]
                           deriving (Show, Eq, Generic)
                           deriving Serialise via WineryVariant TransactionIdLookup
                           
-- | Used for git-style head backtracking such as topic~3^2.
data TransactionIdHeadBacktrack = TransactionIdHeadParentBacktrack Int | -- ^ git equivalent of ~v: walk back n parents, arbitrarily choosing a parent when a choice must be made
                                  TransactionIdHeadBranchBacktrack Int | -- ^ git equivalent of ^: walk back one parent level to the nth arbitrarily-chosen parent 
                                  TransactionStampHeadBacktrack UTCTime -- ^ git equivalent of 'git-rev-list -n 1 --before X' find the first transaction which was created before the timestamp
                                  deriving (Show, Eq, Generic)
                                  deriving Serialise via WineryVariant TransactionIdHeadBacktrack

  
-- | Operators which manipulate a transaction graph and which transaction the current 'Session' is based upon.
data TransactionGraphOperator = JumpToHead HeadName  |
                                JumpToTransaction TransactionId |
                                WalkBackToTime UTCTime |
                                Branch HeadName |
                                DeleteBranch HeadName |
                                MergeTransactions MergeStrategy HeadName HeadName |
                                Commit |
                                Rollback
                              deriving (Eq, Show, Generic)
                              deriving Serialise via WineryVariant TransactionGraphOperator
                                       
isCommit :: TransactionGraphOperator -> Bool                                       
isCommit Commit = True
isCommit _ = False
                                       
data ROTransactionGraphOperator = ShowGraph | ValidateMerkleHashes
                                  deriving Show

bootstrapTransactionGraph :: UTCTime -> TransactionId -> DatabaseContext -> TransactionGraph
bootstrapTransactionGraph stamp' freshId context = TransactionGraph bootstrapHeads bootstrapTransactions
  where
    bootstrapHeads = M.singleton "master" freshTransaction
    newSchemas = Schemas context M.empty
    freshTransaction = fresh freshId stamp' newSchemas
    hashedTransaction = Transaction freshId ((transactionInfo freshTransaction) { merkleHash = calculateMerkleHash freshTransaction emptyTransactionGraph }) newSchemas
    bootstrapTransactions = S.singleton hashedTransaction

-- | Create a transaction graph from a context.
freshTransactionGraph :: DatabaseContext -> IO (TransactionGraph, TransactionId)
freshTransactionGraph ctx = do
  now <- getCurrentTime
  freshId <- nextRandom
  pure (bootstrapTransactionGraph now freshId ctx, freshId)


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

transactionsForIds :: S.Set TransactionId -> TransactionGraph -> Either RelationalError (S.Set Transaction)
transactionsForIds idSet graph =
  S.fromList <$> forM (S.toList idSet) (`transactionForId` graph)

-- | A root transaction terminates a graph and has no parents.
isRootTransaction :: Transaction -> Bool
isRootTransaction trans = parentIds trans == S.singleton U.nil

rootTransactions :: TransactionGraph -> S.Set Transaction
rootTransactions graph = S.filter isRootTransaction (transactionsForGraph graph)

-- the first transaction has no parent - all other do have parents- merges have two parents
parentTransactions :: Transaction -> TransactionGraph -> Either RelationalError (S.Set Transaction)
parentTransactions trans = transactionsForIds (parentIds trans)

childTransactions :: Transaction -> TransactionGraph -> Either RelationalError (S.Set Transaction)
childTransactions trans graph = transactionsForIds childIds graph
  where
    childIds = S.map transactionId (S.filter filt (transactionsForGraph graph))
    filt trans' = S.member (transactionId trans) (parentIds trans')

-- create a new commit and add it to the heads
-- technically, the new head could be added to an existing commit, but by adding a new commit, the new head is unambiguously linked to a new commit (with a context indentical to its parent)
addBranch :: UTCTime -> TransactionId -> HeadName -> TransactionId -> TransactionGraph -> Either RelationalError (Transaction, TransactionGraph)
addBranch stamp' newId newBranchName branchPointId graph = do
  parentTrans <- transactionForId branchPointId graph
  let newTrans = Transaction newId (TI.singleParent branchPointId stamp') (schemas parentTrans)
  addTransactionToGraph newBranchName newTrans graph

--adds a disconnected transaction to a transaction graph at some head
addDisconnectedTransaction :: UTCTime -> TransactionId -> HeadName -> DisconnectedTransaction -> TransactionGraph -> Either RelationalError (Transaction, TransactionGraph)
addDisconnectedTransaction stamp' newId headName (DisconnectedTransaction parentId schemas' _) graph = addTransactionToGraph headName newTrans' graph
  where
    newTrans = Transaction newId newTInfo schemas'
    newTInfo = TI.singleParent parentId stamp'
    newTrans' = Transaction newId (newTInfo { merkleHash = newHash }) schemas'
    newHash = calculateMerkleHash newTrans graph--lookup parents in graph to calculate
--LOOP
addTransactionToGraph :: HeadName -> Transaction -> TransactionGraph -> Either RelationalError (Transaction, TransactionGraph)
addTransactionToGraph headName newTrans graph = do
  let parentIds' = parentIds newTrans
      newId = transactionId newTrans
      validateIds ids = mapM (`transactionForId` graph) (S.toList ids)
  childTs <- childTransactions newTrans graph
  --validate that the parent transactions are in the graph
  _ <- validateIds parentIds'
  when (S.size parentIds' < 1) (Left $ NewTransactionMissingParentError newId)
  --if the headName already exists, ensure that it refers to a parent
  case transactionForHead headName graph of
    Nothing -> pure () -- any headName is OK 
    Just trans -> when (S.notMember (transactionId trans) parentIds') (Left (HeadNameSwitchingHeadProhibitedError headName))
  --validate that the transaction has no children
  unless (S.null childTs) (Left $ NewTransactionMayNotHaveChildrenError newId)
  --validate that the trasaction's id is unique
  when (isRight (transactionForId newId graph)) (Left (TransactionIdInUseError newId))
  --replace all references to UncommittedTransactionMarker to new transaction id
  let newTrans' = newTransUncommittedReplace newTrans
      updatedTransSet = S.insert newTrans' (transactionsForGraph graph)
      updatedHeads = M.insert headName newTrans' (transactionHeadsForGraph graph)
  pure (newTrans', TransactionGraph updatedHeads updatedTransSet)

--replace all occurrences of the uncommitted context marker
newTransUncommittedReplace :: Transaction -> Transaction
newTransUncommittedReplace trans@(Transaction tid tinfo (Schemas ctx sschemas)) =
  Transaction tid tinfo (Schemas fixedContext sschemas)
  where
  uncommittedReplace UncommittedContextMarker = TransactionMarker tid
  uncommittedReplace marker = marker
  relvars = relationVariables (concreteDatabaseContext trans)  
  fixedRelvars = M.map (fmap uncommittedReplace) relvars
  fixedContext = ctx { relationVariables = fixedRelvars }
  


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
      let disconnectedTrans = Discon.freshTransaction (transactionId trans) (schemas trans)
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
evalGraphOp stamp' newId (DisconnectedTransaction parentId schemas' _) graph (Branch newBranchName) = do
  let newDiscon = Discon.freshTransaction newId schemas'
  case addBranch stamp' newId newBranchName parentId graph of
    Left err -> Left err
    Right (_, newGraph) -> Right (newDiscon, newGraph)
  
-- add the disconnected transaction to the graph
-- affects graph and disconnectedtransaction- the new disconnectedtransaction's parent is the freshly committed transaction
evalGraphOp stamp' newTransId discon@(DisconnectedTransaction parentId schemas' _) graph Commit = case transactionForId parentId graph of
  Left err -> Left err
  Right parentTransaction -> case headNameForTransaction parentTransaction graph of
    Nothing -> Left $ TransactionIsNotAHeadError parentId
    Just headName -> case maybeUpdatedGraph of
      Left err-> Left err
      Right (_, updatedGraph) -> Right (newDisconnectedTrans, updatedGraph)
      where
        newDisconnectedTrans = Discon.freshTransaction newTransId schemas'
        maybeUpdatedGraph = addDisconnectedTransaction stamp' newTransId headName discon graph

-- refresh the disconnected transaction, return the same graph
evalGraphOp _ _ (DisconnectedTransaction parentId _ _) graph Rollback = case transactionForId parentId graph of
  Left err -> Left err
  Right parentTransaction -> Right (newDiscon, graph)
    where
      newDiscon = Discon.freshTransaction parentId (schemas parentTransaction)
      
evalGraphOp stamp' newId (DisconnectedTransaction parentId _ _) graph (MergeTransactions mergeStrategy headNameA headNameB) = 
  runGraphRefRelationalExprM env $ mergeTransactions stamp' newId parentId mergeStrategy (headNameA, headNameB)
  where
    env = freshGraphRefRelationalExprEnv Nothing graph

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
                                  Attribute "hash" ByteStringAtomType,
                                  Attribute "stamp" DateTimeAtomType,
                                  Attribute "parents" (RelationAtomType parentAttributes),
                                  Attribute "current" BoolAtomType,
                                  Attribute "head" TextAtomType
                                 ]
    parentAttributes = A.attributesFromList [Attribute "id" TextAtomType]
    tupleGenerator transaction = case transactionParentsRelation transaction graph of
      Left err -> Left err
      Right parentTransRel -> Right [TextAtom $ T.pack $ show (transactionId transaction),
                                     ByteStringAtom $ _unMerkleHash (merkleHash (transactionInfo transaction)),
                                     DateTimeAtom (timestamp transaction),
                                     RelationAtom parentTransRel,
                                     BoolAtom $ parentId == transactionId transaction,
                                     TextAtom $ fromMaybe "" (headNameForTransaction transaction graph)
                                      ]

transactionParentsRelation :: Transaction -> TransactionGraph -> Either RelationalError Relation
transactionParentsRelation trans graph = 
  if isRootTransaction trans then    
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
createMergeTransaction :: UTCTime -> TransactionId -> MergeStrategy -> (Transaction, Transaction) -> GraphRefRelationalExprM Transaction
createMergeTransaction stamp' newId (SelectedBranchMergeStrategy selectedBranch) t2@(trans1, trans2) = do
  graph <- gfGraph
  selectedTrans <- validateHeadName selectedBranch graph t2
  pure $ addMerkleHash graph $
    Transaction newId (TransactionInfo {
                          parents = NE.fromList [transactionId trans1,
                                                 transactionId trans2],
                          stamp = stamp',
                          merkleHash = mempty }) (schemas selectedTrans)
                       
-- merge functions, relvars, individually
createMergeTransaction stamp' newId strat@UnionMergeStrategy t2 =
  createUnionMergeTransaction stamp' newId strat t2

-- merge function, relvars, but, on error, just take the component from the preferred branch
createMergeTransaction stamp' newId strat@(UnionPreferMergeStrategy _) t2 =
  createUnionMergeTransaction stamp' newId strat t2

-- | Returns the correct Transaction for the branch name in the graph and ensures that it is one of the two transaction arguments in the tuple.
validateHeadName :: HeadName -> TransactionGraph -> (Transaction, Transaction) -> GraphRefRelationalExprM Transaction
validateHeadName headName graph (t1, t2) =
  case transactionForHead headName graph of
    Nothing -> throwError (MergeTransactionError SelectedHeadMismatchMergeError)
    Just trans -> if trans /= t1 && trans /= t2 then 
                    throwError (MergeTransactionError SelectedHeadMismatchMergeError)
                  else
                    pure trans
  
-- Algorithm: start at one transaction and work backwards up the parents. If there is a node we have not yet visited as a child, then walk that up to its head. If that branch contains the goal transaction, then we have completed a valid subgraph traversal.
subGraphOfFirstCommonAncestor :: TransactionGraph -> TransactionHeads -> Transaction -> Transaction -> S.Set Transaction -> Either RelationalError TransactionGraph
subGraphOfFirstCommonAncestor origGraph resultHeads currentTrans' goalTrans traverseSet = do
  let currentid = transactionId currentTrans'
      goalid = transactionId goalTrans
  if currentTrans' == goalTrans then
    Right (TransactionGraph resultHeads traverseSet) -- add filter
    --catch root transaction to improve error?
    else do
    currentTransChildren <- childTransactions currentTrans' origGraph
    let searchChildren = S.difference (S.insert currentTrans' traverseSet) currentTransChildren
        searchChild start' = pathToTransaction origGraph start' goalTrans (S.insert currentTrans' traverseSet)
        childSearches = map searchChild (S.toList searchChildren)
        errors = lefts childSearches
        pathsFound = rights childSearches
        realErrors = filter (/= FailedToFindTransactionError goalid) errors
    -- report any non-search-related errors        
    unless (null realErrors) (Left (head realErrors))
    -- if no paths found, search the parent
    if null pathsFound then
      case oneParent currentTrans' of
        Left RootTransactionTraversalError -> Left (NoCommonTransactionAncestorError currentid goalid)
        Left err -> Left err
        Right currentTransParent ->
          subGraphOfFirstCommonAncestor origGraph resultHeads currentTransParent goalTrans (S.insert currentTrans' traverseSet)
      else -- we found a path
      Right (TransactionGraph resultHeads (S.unions (traverseSet : pathsFound)))
  where
    oneParent (Transaction _ tinfo _) = transactionForId (NE.head (parents tinfo)) origGraph
    
-- | Search from a past graph point to all following heads for a specific transaction. If found, return the transaction path, otherwise a RelationalError.
pathToTransaction :: TransactionGraph -> Transaction -> Transaction -> S.Set Transaction -> Either RelationalError (S.Set Transaction)
pathToTransaction graph currentTransaction targetTransaction accumTransSet = do
  let targetId = transactionId targetTransaction
  if transactionId targetTransaction == transactionId currentTransaction then
    Right accumTransSet
    else do
    currentTransChildren <- childTransactions currentTransaction graph
    if null currentTransChildren then
      Left (FailedToFindTransactionError targetId)
      else do
      let searches = map (\t -> pathToTransaction graph t targetTransaction (S.insert t accumTransSet)) (S.toList currentTransChildren)
      let realErrors = filter (/= FailedToFindTransactionError targetId) (lefts searches)
          paths = rights searches
      if not (null realErrors) then -- found some real errors
        Left (head realErrors)
      else if null paths then -- failed to find transaction in all children
             Left (FailedToFindTransactionError targetId)
           else --we have some paths!
             Right (S.unions paths)

mergeTransactions :: UTCTime -> TransactionId -> TransactionId -> MergeStrategy -> (HeadName, HeadName) -> GraphRefRelationalExprM (DisconnectedTransaction, TransactionGraph)
mergeTransactions stamp' newId parentId mergeStrategy (headNameA, headNameB) = do
  graph <- gfGraph
  let transactionForHeadErr name = case transactionForHead name graph of
        Nothing -> throwError (NoSuchHeadNameError name)
        Just t -> pure t
      runE e = case e of
        Left e' -> throwError e'
        Right v -> pure v
  transA <- transactionForHeadErr headNameA
  transB <- transactionForHeadErr headNameB
  disconParent <- gfTransForId parentId
  let subHeads = M.filterWithKey (\k _ -> k `elem` [headNameA, headNameB]) (transactionHeadsForGraph graph)
  subGraph <- runE $ subGraphOfFirstCommonAncestor graph subHeads transA transB S.empty
  subGraph' <- runE $ filterSubGraph subGraph subHeads
  mergedTrans <- local (const (freshGraphRefRelationalExprEnv Nothing subGraph')) $ createMergeTransaction stamp' newId mergeStrategy (transA, transB)
  case headNameForTransaction disconParent graph of
        Nothing -> throwError (TransactionIsNotAHeadError parentId)
        Just headName -> do
          (newTrans, newGraph) <- runE $ addTransactionToGraph headName mergedTrans graph
          case checkConstraints (concreteDatabaseContext mergedTrans) newId graph of
            Left err -> throwError err
            Right _ -> do
              let newGraph' = TransactionGraph (transactionHeadsForGraph newGraph) (transactionsForGraph newGraph)
                  newDiscon = Discon.freshTransaction newId (schemas newTrans)
              pure (newDiscon, newGraph')
  
--TEMPORARY COPY/PASTE  
showTransactionStructureX :: Transaction -> TransactionGraph -> String
showTransactionStructureX trans graph = headInfo ++ " " ++ show (transactionId trans) ++ " " ++ parentTransactionsInfo
  where
    headInfo = maybe "" show (headNameForTransaction trans graph)
    parentTransactionsInfo = if isRootTransaction trans then "root" else case parentTransactions trans graph of
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
createUnionMergeTransaction :: UTCTime -> TransactionId -> MergeStrategy -> (Transaction, Transaction) -> GraphRefRelationalExprM Transaction
createUnionMergeTransaction stamp' newId strategy (t1,t2) = do
  let contextA = concreteDatabaseContext t1
      contextB = concreteDatabaseContext t2
      liftMergeE x = case x of
        Left e -> throwError (MergeTransactionError e)
        Right t -> pure t
        
  graph <- gfGraph
  preference <- case strategy of 
    UnionMergeStrategy -> pure PreferNeither
    UnionPreferMergeStrategy preferBranch ->
      case transactionForHead preferBranch graph of
        Nothing -> throwError (MergeTransactionError (PreferredHeadMissingMergeError preferBranch))
        Just preferredTrans -> pure $ if t1 == preferredTrans then PreferFirst else PreferSecond
    badStrat -> throwError (MergeTransactionError (InvalidMergeStrategyError badStrat))
          
  incDeps <- liftMergeE $ unionMergeMaps preference (inclusionDependencies contextA) (inclusionDependencies contextB)
  relVars <- unionMergeRelVars preference (relationVariables contextA) (relationVariables contextB)
  atomFuncs <- liftMergeE $ unionMergeAtomFunctions preference (atomFunctions contextA) (atomFunctions contextB)
  notifs <- liftMergeE $ unionMergeMaps preference (notifications contextA) (notifications contextB)
  types <- liftMergeE $ unionMergeTypeConstructorMapping preference (typeConstructorMapping contextA) (typeConstructorMapping contextB)
  dbcFuncs <- liftMergeE $ unionMergeDatabaseContextFunctions preference (dbcFunctions contextA) (dbcFunctions contextB)
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
  pure $ addMerkleHash graph $
    Transaction newId (TransactionInfo {
                          parents = NE.fromList [transactionId t1,
                                                  transactionId t2],
                            stamp = stamp',
                            merkleHash = mempty }) newSchemas

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

  let parentIds' = S.toAscList (parentIds trans)
  case parentIds' of
    [] -> Left RootTransactionTraversalError
    firstParentId:_ -> do
      parentTrans <- transactionForId firstParentId graph
      if steps == 1 then
        pure (transactionId parentTrans)
        else
        backtrackGraph graph (transactionId parentTrans) (TransactionIdHeadParentBacktrack (steps - 1))
  
backtrackGraph graph currentTid (TransactionIdHeadBranchBacktrack steps) = do
  trans <- transactionForId currentTid graph
  let parentIds' = parentIds trans
  if S.size parentIds' < 1 then
    Left RootTransactionTraversalError    
    else if S.size parentIds' < steps then
           Left (ParentCountTraversalError (S.size parentIds') steps)
         else
           pure (S.elemAt (steps - 1) parentIds')
           
backtrackGraph graph currentTid btrack@(TransactionStampHeadBacktrack stamp') = do           
  trans <- transactionForId currentTid graph
  let parentIds' = parentIds trans  
  if timestamp trans <= stamp' then
    pure currentTid
    else if S.null parentIds' then
           Left RootTransactionTraversalError
         else
           let arbitraryParent = head (S.toList parentIds') in
           backtrackGraph graph arbitraryParent btrack
    
-- | Create a temporary branch for commit, merge the result to head, delete the temporary branch. This is useful to atomically commit a transaction, avoiding a TransactionIsNotHeadError but trading it for a potential MergeError.
--this is not a GraphOp because it combines multiple graph operations
autoMergeToHead :: UTCTime -> (TransactionId, TransactionId, TransactionId) -> DisconnectedTransaction -> HeadName -> MergeStrategy -> TransactionGraph -> Either RelationalError (DisconnectedTransaction, TransactionGraph)
autoMergeToHead stamp' (tempBranchTransId, tempCommitTransId, mergeTransId) discon mergeToHeadName strat graph = do
  let tempBranchName = "mergebranch_" <> U.toText tempBranchTransId
  --create the temp branch
  (discon', graph') <- evalGraphOp stamp' tempBranchTransId discon graph (Branch tempBranchName)
  
  --commit to the new branch- possible future optimization: don't require fsync for this- create a temp commit type
  (discon'', graph'') <- evalGraphOp stamp' tempCommitTransId discon' graph' Commit
 
  --jump to merge head
  (discon''', graph''') <- evalGraphOp stamp' tempBranchTransId discon'' graph'' (JumpToHead mergeToHeadName)
  
  --create the merge
  (discon'''', graph'''') <- evalGraphOp stamp' mergeTransId discon''' graph''' (MergeTransactions strat tempBranchName mergeToHeadName)
  
  --delete the temp branch
  (discon''''', graph''''') <- evalGraphOp stamp' tempBranchTransId discon'''' graph'''' (DeleteBranch tempBranchName)
  {-
  let rel = runReader (evalRelationalExpr (RelationVariable "s" ())) (mkRelationalExprState $ D.concreteDatabaseContext discon'''')
  traceShowM rel
-}
  
  pure (discon''''', graph''''')


addMerkleHash :: TransactionGraph -> Transaction -> Transaction
addMerkleHash graph trans = Transaction (transactionId trans) newInfo (schemas trans)
  where
    newInfo = (transactionInfo trans) { merkleHash = calculateMerkleHash trans graph }
  
-- the new hash includes the parents' ids, the current id, and the hash of the context, and the merkle hashes of the parent transactions
calculateMerkleHash :: Transaction -> TransactionGraph -> MerkleHash
calculateMerkleHash trans graph = MerkleHash $ hashlazy (BL.fromChunks [transIds,
                                                                        schemasBytes
                                                                       ] <>                                                      dbcBytes <> parentMerkleHashes)
  where
    parentMerkleHashes = BL.fromChunks $ map (_unMerkleHash . getMerkleHash) parentTranses
    parentTranses =
      case transactionsForIds (parentIds trans) graph of
        Left RootTransactionTraversalError -> []
        Left e -> error ("failed to find transaction in Merkle hash construction: " ++ show e)
        Right t -> S.toList t
    getMerkleHash t = merkleHash (transactionInfo t)
    transIds = serialise (transactionId trans : S.toList (parentIds trans))
    dbcBytes = DBC.hashBytes (concreteDatabaseContext trans)
    schemasBytes = serialise (subschemas trans)

validateMerkleHash :: Transaction -> TransactionGraph -> Either MerkleValidationError ()
validateMerkleHash trans graph = 
  if expectedHash /= actualHash  then
    Left (MerkleValidationError (transactionId trans) expectedHash actualHash)
  else
    pure ()
  where
    expectedHash = merkleHash (transactionInfo trans)
    actualHash = calculateMerkleHash trans graph

data MerkleValidationError = MerkleValidationError TransactionId MerkleHash MerkleHash
  deriving (Show, Eq, Generic)

validateMerkleHashes :: TransactionGraph -> Either [MerkleValidationError] ()
validateMerkleHashes graph =
  if null errs then pure () else Left errs
  where
    errs = S.foldr validateTrans [] (transactionsForGraph graph)    
    validateTrans trans acc =
      case validateMerkleHash trans graph of
        Left err -> err : acc
        _ -> acc

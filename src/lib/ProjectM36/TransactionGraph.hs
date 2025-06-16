{-# LANGUAGE DeriveGeneric, CPP, FlexibleContexts, DerivingVia #-}
module ProjectM36.TransactionGraph where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Transaction
import ProjectM36.TransactionInfo as TI
import ProjectM36.Relation
import ProjectM36.ValueMarker
import ProjectM36.DatabaseContext.Types
import ProjectM36.DatabaseContext
import ProjectM36.TransactionGraph.Types
import ProjectM36.Transaction.Types
import ProjectM36.IsomorphicSchema.Types hiding (concreteDatabaseContext, subschemas)
import qualified ProjectM36.IsomorphicSchema.Types as Schema
import qualified ProjectM36.TupleSet as TS
import ProjectM36.Tuple
import ProjectM36.RelationalExpression
import ProjectM36.TransactionGraph.Merge
import ProjectM36.MerkleHash
import ProjectM36.DisconnectedTransaction (DisconnectedTransaction(..), CurrentHead(..))
import qualified ProjectM36.DisconnectedTransaction as Discon
import qualified ProjectM36.Attribute as A
import ProjectM36.HashSecurely
import ProjectM36.ReferencedTransactionIds

import Codec.Winery
#if MIN_VERSION_base(4,18,0)
import Control.Monad (foldM, forM, unless, when)
#endif
import Control.Monad.Except
import Control.Monad.Reader
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


data TransactionGraphExpr = JumpToHead HeadName  |
                            JumpToTransaction TransactionId |
                            WalkBackToTime UTCTime
                          deriving (Eq, Show, Generic)
  deriving Serialise via WineryVariant TransactionGraphExpr
                                
-- | Operators which manipulate a transaction graph and which transaction the current 'Session' is based upon.
data AlterTransactionGraphExpr = 
                                Branch HeadName |
                                DeleteBranch HeadName |
                                MergeTransactions MergeStrategy HeadName HeadName |
                                Commit |
                                Rollback
                              deriving (Eq, Show, Generic)
                              deriving Serialise via WineryVariant AlterTransactionGraphExpr

isCommit :: AlterTransactionGraphExpr -> Bool
isCommit Commit = True
isCommit _ = False

data ROTransactionGraphOperator = ShowGraph | ValidateMerkleHashes
                                  deriving Show

bootstrapTransactionGraph :: UTCTime -> TransactionId -> DatabaseContext -> TransactionGraph
bootstrapTransactionGraph stamp' freshId context = TransactionGraph bootstrapHeads bootstrapTransactions
  where
    bootstrapHeads = M.singleton "master" freshTransaction
    newSchemas = Schemas context emptyValue
    freshTransaction = fresh freshId stamp' newSchemas
    hashedTransaction = Transaction freshId ((transactionInfo freshTransaction) { merkleHash = calculateMerkleHash freshTransaction emptyTransactionGraph }) newSchemas
    bootstrapTransactions = S.singleton hashedTransaction

freshTransactionGraph' :: ResolvedDatabaseContext -> IO (TransactionGraph, TransactionId)
freshTransactionGraph' = freshTransactionGraph . toDatabaseContext
  
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

headNamesForTransaction :: Transaction -> TransactionGraph -> [HeadName]
headNamesForTransaction transaction (TransactionGraph heads _) =
  M.keys $ M.filter (transaction ==) heads

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

--adds a disconnected transaction to a transaction graph at some head
addDisconnectedTransaction :: UTCTime -> TransactionId -> DisconnectedTransaction -> TransactionGraph -> Either RelationalError (UncommittedTransaction, TransactionGraph)
addDisconnectedTransaction stamp' newId discon graph = do
  headName <- case disconCurrentHead discon of
                   CurrentHeadBranch hname -> pure hname
                   CurrentHeadTransactionId tid -> Left $ TransactionIsNotAHeadError tid
  let newTrans = UncommittedTransaction $ Transaction newId newTInfo schemas'
      schemas' = Schemas (Discon.concreteDatabaseContext discon) (Schema.subschemas (disconSchemas discon))
      newTInfo = TI.singleParent (Discon.parentId discon) stamp'
  addTransactionToGraph headName newTrans graph

addTransactionToGraph :: HeadName -> UncommittedTransaction -> TransactionGraph -> Either RelationalError (UncommittedTransaction, TransactionGraph)
addTransactionToGraph headName (UncommittedTransaction newTrans) graph = do
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
      --add merkle hash to all new transactions
      hashedTransactionInfo = (transactionInfo newTrans')
                              { merkleHash = calculateMerkleHash newTrans' graph }
      hashedTrans = Transaction (transactionId newTrans') hashedTransactionInfo (schemas newTrans')
      updatedTransSet = S.insert hashedTrans (transactionsForGraph graph)
      updatedHeads = M.insert headName hashedTrans (transactionHeadsForGraph graph)
  pure (UncommittedTransaction hashedTrans, TransactionGraph updatedHeads updatedTransSet)

--replace all occurrences of the uncommitted context marker
newTransUncommittedReplace :: Transaction -> Transaction
newTransUncommittedReplace trans@(Transaction tid tinfo (Schemas ctx sschemas)) =
  Transaction tid tinfo (Schemas fixedContext sschemas)
  where
  uncommittedReplaceRelVars (ValueMarker rvs) =
    ValueMarker $ M.map (fmap uncommittedReplaceMarker) rvs
  uncommittedReplaceRelVars e@NotChangedSinceMarker{} = e
  
  uncommittedReplaceMarker UncommittedContextMarker = TransactionMarker tid
  uncommittedReplaceMarker marker = marker
  
  relvars = relationVariables (concreteDatabaseContext trans)
  fixedRelvars = uncommittedReplaceRelVars relvars
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

data ROTransactionGraphCreateDisconnectedTransaction =
  -- | Lookup disconnected transaction info from persistent storage or graph.
  MakeDisconnectedTransactionFromTransaction TransactionId |
  -- | No further lookup required.
  PureDisconnectedTransaction DisconnectedTransaction 
  
-- | Evaluate the graph-changing operator, reading graph info from disk, if necessary. Returns the new disconnected transaction and new graph. When jumping to a new part of the graph, we may need to read from disk to update the diconnected transaction.
evalTransactionGraphExpr :: DisconnectedTransaction -> TransactionGraph -> TransactionGraphExpr -> Either RelationalError DisconnectedTransaction
evalTransactionGraphExpr _discon graph (JumpToHead headName) =
    case transactionForHead headName graph of
      Just newHeadTransaction -> do
        let discon' = DisconnectedTransaction {
              disconTransactionId = transactionId newHeadTransaction,
              disconSchemas = schemas newHeadTransaction,
              disconCurrentHead = Discon.CurrentHeadBranch headName
              }
        pure discon'
      Nothing -> Left $ NoSuchHeadNameError headName
evalTransactionGraphExpr _discon graph (JumpToTransaction jumpId) =
  case transactionForId jumpId graph of
    Left err -> Left err
    Right parentTrans -> do
      pure (DisconnectedTransaction {
               disconTransactionId = transactionId parentTrans,
               disconSchemas = schemas parentTrans,
               disconCurrentHead = CurrentHeadTransactionId jumpId
               })
evalTransactionGraphExpr discon graph (WalkBackToTime backTime) = do
  let startTransId = Discon.parentId discon
  jumpDest <- backtrackGraph graph startTransId (TransactionStampHeadBacktrack backTime)
  case transactionForId jumpDest graph of
    Left err -> Left err
    Right trans -> do
      pure (DisconnectedTransaction {
               disconTransactionId = transactionId trans,
               disconSchemas = schemas trans,
               disconCurrentHead = CurrentHeadTransactionId jumpDest
               })

evalAlterTransactionGraphExpr :: UTCTime -> TransactionId -> DisconnectedTransaction -> TransactionGraph -> AlterTransactionGraphExpr -> Either RelationalError (DisconnectedTransaction, Maybe UncommittedTransaction, TransactionGraph)
evalAlterTransactionGraphExpr _stamp' _newId discon graph@(TransactionGraph heads transSet) (Branch newBranchName) =
  if M.member newBranchName heads then
    Left (HeadNameAlreadyInUseError newBranchName)
    else do
    trans <- transactionForId (Discon.parentId discon) graph
    let discon' = Discon.freshTransaction (CurrentHeadBranch newBranchName) (transactionId trans) (schemas trans)
    pure (discon', Nothing, TransactionGraph (M.insert newBranchName trans heads) transSet)
evalAlterTransactionGraphExpr _stamp' _newId discon graph@(TransactionGraph graphHeads transSet) (DeleteBranch branchName) =
  case transactionForHead branchName graph of
    Nothing -> Left (NoSuchHeadNameError branchName)
    Just _ -> Right (discon, Nothing, TransactionGraph (M.delete branchName graphHeads) transSet)
evalAlterTransactionGraphExpr stamp' newId discon graph (MergeTransactions mergeStrategy headNameA headNameB) = do
  let env = freshGraphRefRelationalExprEnv Nothing graph
  (uTrans@(UncommittedTransaction trans), graph') <- runGraphRefRelationalExprM env $ mergeTransactions stamp' newId (Discon.parentId discon) mergeStrategy (MergeHeadNames { sourceHead = headNameA,
        targetHead = headNameB })
  let discon' = discon { disconSchemas = schemas trans,
                         disconCurrentHead = CurrentHeadBranch headNameB
                       }
  pure (discon', Just uTrans, graph')

evalAlterTransactionGraphExpr stamp' newTransId discon graph Commit =
  case transactionForId (Discon.parentId discon) graph of
  Left err -> Left err
  Right parentTransaction ->
    case headNamesForTransaction parentTransaction graph of
      [] -> Left $ TransactionIsNotAHeadError (Discon.parentId discon)
      _ -> do
        let maybeUpdatedGraph = addDisconnectedTransaction stamp' newTransId discon graph
        case maybeUpdatedGraph of
          Left err -> Left err
          Right (uncommittedTrans, updatedGraph) -> do
                let newDisconnectedTrans = Discon.freshTransaction (disconCurrentHead discon) newTransId (disconSchemas discon)
                Right (newDisconnectedTrans, Just uncommittedTrans, updatedGraph)

evalAlterTransactionGraphExpr _stamp' _newId discon graph Rollback =
  case transactionForId (disconTransactionId discon) graph of
    Left err -> Left err
    Right parentTransaction -> Right (newDiscon, Nothing, graph)
      where
        newDiscon = Discon.freshTransaction (disconCurrentHead discon) (disconTransactionId discon) (schemas parentTransaction)

--present a transaction graph as a relation showing the uuids, parentuuids, and flag for the current location of the disconnected transaction
graphAsRelation :: DisconnectedTransaction -> TransactionGraph -> Either RelationalError Relation
graphAsRelation discon graph@(TransactionGraph _ transSet) = do
  tupleMatrix <- mapM tupleGenerator (S.toList transSet)
  mkRelationFromList attrs tupleMatrix
  where
    attrs = A.attributesFromList [Attribute "id" TextAtomType,
                                  Attribute "hash" ByteStringAtomType,
                                  Attribute "stamp" DateTimeAtomType,
                                  Attribute "parents" (RelationAtomType parentAttributes),
                                  Attribute "current" BoolAtomType,
                                  Attribute "heads" (RelationAtomType headAttributes)
                                 ]
    parentAttributes = A.attributesFromList [Attribute "id" TextAtomType]
    headAttributes = A.attributesFromList [Attribute "name" TextAtomType]
    tupleGenerator transaction = case transactionParentsRelation transaction graph of
      Left err -> Left err
      Right parentTransRel -> do
        let headNames = headNamesForTransaction transaction graph
        headsRel <- mkRelationFromList headAttributes (map (\hname -> [TextAtom hname]) headNames)
        Right [TextAtom $ T.pack $ show (transactionId transaction),
                                     ByteStringAtom $ _unMerkleHash (merkleHash (transactionInfo transaction)),
                                     DateTimeAtom (timestamp transaction),
                                     RelationAtom parentTransRel,
                                     BoolAtom $ Discon.parentId discon == transactionId transaction,
                                     RelationAtom headsRel
              ]

transactionParentsRelation :: Transaction -> TransactionGraph -> Either RelationalError Relation
transactionParentsRelation trans graph =
  if isRootTransaction trans then
    mkRelation attrs TS.empty
    else do
      parentTransSet <- parentTransactions trans graph
      let tuples = map trans2tuple (S.toList parentTransSet)
      mkRelationFromTuples attrs tuples
  where
    attrs = A.attributesFromList [Attribute "id" TextAtomType]
    trans2tuple trans2 = mkRelationTuple attrs $ V.singleton (TextAtom (T.pack (show $ transactionId trans2)))

-- | Execute the merge strategy against the transactions, returning a new transaction which can be then added to the transaction graph
createMergeTransaction :: UTCTime -> TransactionId -> MergeStrategy -> (Transaction, Transaction) -> GraphRefRelationalExprM UncommittedTransaction
createMergeTransaction stamp' newId (SelectedBranchMergeStrategy selectedBranch) t2@(trans1, trans2) = do
  graph <- gfGraph
  selectedTrans <- validateHeadName selectedBranch graph t2
  pure $ UncommittedTransaction $ addMerkleHash graph $
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

-- Algorithm: start at one transaction and work backwards up the parents. If there is a node we have not yet visited as a child, then walk that up to its head. If that branch contains the goal transaction, then we have completed a valid subgraph traversal. The subgraph must also include any transactions which are referenced by other transactions.
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
    case realErrors of
      [] -> pure ()
      err : _ -> Left err
    -- if no paths found, search the parent
    if null pathsFound then
      case oneParent currentTrans' of
        Left RootTransactionTraversalError -> Left (NoCommonTransactionAncestorError currentid goalid)
        Left err -> Left err
        Right currentTransParent ->
          subGraphOfFirstCommonAncestor origGraph resultHeads currentTransParent goalTrans (S.insert currentTrans' traverseSet)
      else do -- we found a path
        -- we union all the relevant path transactions together, but we are missing any transactions which these transaction may reference. To make a valid transaction graph, we must include these referenced transactions.
        let openSet = S.unions (traverseSet : pathsFound)
            transactionIncluder acc trans = do
              allTrans <- referencedTransactionIdsForTransaction trans origGraph
              pure $ S.union allTrans acc
        closedTransactionSet <- foldM transactionIncluder mempty (S.toList openSet)
        Right (TransactionGraph resultHeads closedTransactionSet)
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
      case realErrors of
        -- found some real errors
        err : _ -> Left err
        [] -> case paths of
          -- failed to find transaction in all children
          [] -> Left $ FailedToFindTransactionError targetId
          -- we have some paths!
          _ -> Right $ S.unions paths

mergeTransactions :: UTCTime -> TransactionId -> TransactionId -> MergeStrategy -> MergeHeadNames -> GraphRefRelationalExprM (UncommittedTransaction, TransactionGraph)
mergeTransactions stamp' newId parentId mergeStrategy mergeHeadNames = do
  graph <- gfGraph
  let transactionForHeadErr name = case transactionForHead name graph of
        Nothing -> throwError (NoSuchHeadNameError name)
        Just t -> pure t
      runE e = case e of
        Left e' -> throwError e'
        Right v -> pure v
  transA <- transactionForHeadErr (sourceHead mergeHeadNames)
  transB <- transactionForHeadErr (targetHead mergeHeadNames)
  disconParent <- gfTransForId parentId
  let subHeads = M.filterWithKey (\k _ -> k `elem` [sourceHead mergeHeadNames, targetHead mergeHeadNames]) (transactionHeadsForGraph graph)
  -- is this an optimization???
  subGraph <- runE $ subGraphOfFirstCommonAncestor graph subHeads transA transB S.empty
  _ <- runE $ validateConnectivity subGraph

  subGraph' <- runE $ filterSubGraph subGraph subHeads
  -- we cannot cut the transaction graph away only to "relevant" transactions because transactions can reference other transactions via relvar expressions
  mergedTrans <- local (const (freshGraphRefRelationalExprEnv Nothing subGraph')) $
                 createMergeTransaction stamp' newId mergeStrategy (transA, transB)
  case headNamesForTransaction disconParent graph of
        [] -> throwError (TransactionIsNotAHeadError parentId)
        _ -> do
          (uTrans, newGraph') <- runE $ addTransactionToGraph (targetHead mergeHeadNames) mergedTrans graph
          case checkConstraints (concreteDatabaseContext (_uncommittedTransaction mergedTrans)) newId graph of
            Left err -> throwError err
            Right _ -> do
              let newGraph'' = TransactionGraph (transactionHeadsForGraph newGraph') (transactionsForGraph newGraph')
              pure (uTrans, newGraph'')

--TEMPORARY COPY/PASTE
showTransactionStructureX :: Bool -> Transaction -> TransactionGraph -> String
showTransactionStructureX showRelVars trans graph = headInfo ++ " " ++ show (transactionId trans) ++ " " ++ parentTransactionsInfo ++ relVarsInfo
  where
    relVars = relationVariables (concreteDatabaseContext trans)
    relVarsAsString = case relVars of
                    NotChangedSinceMarker tid -> "TransactionId " <> show tid
                    ValueMarker m -> concatMap show (M.toList m)
    relVarsInfo | not showRelVars = ""
                | otherwise = "\n" <> relVarsAsString
    headInfo = show (headNamesForTransaction trans graph)
    parentTransactionsInfo = if isRootTransaction trans then "root" else case parentTransactions trans graph of
      Left err -> show err
      Right parentTransSet -> concat $ S.toList $ S.map (show . transactionId) parentTransSet

showGraphStructureX :: Bool -> TransactionGraph -> String
showGraphStructureX showRelVars graph@(TransactionGraph heads transSet) = headsInfo ++ S.foldr folder "" transSet
  where
    folder trans acc = acc ++ showTransactionStructureX showRelVars trans graph ++ "\n"
    headsInfo = show $ M.map transactionId heads

-- | After splicing out a subgraph, run it through this function to remove references to transactions which are not in the subgraph.
filterSubGraph :: TransactionGraph -> TransactionHeads -> Either RelationalError TransactionGraph
filterSubGraph graph heads = Right $ TransactionGraph newHeads newTransSet
  where
    validIds = S.map transactionId (transactionsForGraph graph)
    newTransSet = S.map (filterTransaction validIds) (transactionsForGraph graph)
    newHeads = M.map (filterTransaction validIds) heads

--helper function for commonalities in union merge
createUnionMergeTransaction :: UTCTime -> TransactionId -> MergeStrategy -> (Transaction, Transaction) -> GraphRefRelationalExprM UncommittedTransaction
createUnionMergeTransaction stamp' newId strategy (t1,t2) = do
  let contextA = concreteDatabaseContext t1
      contextB = concreteDatabaseContext t2
      liftE x = case x of
        Left e -> throwError e
        Right t -> pure t

  graph <- gfGraph
  preference <- case strategy of
    UnionMergeStrategy -> pure PreferNeither
    UnionPreferMergeStrategy preferBranch ->
      case transactionForHead preferBranch graph of
        Nothing -> throwError (MergeTransactionError (PreferredHeadMissingMergeError preferBranch))
        Just preferredTrans -> pure $ if t1 == preferredTrans then PreferFirst else PreferSecond
    badStrat -> throwError (MergeTransactionError (InvalidMergeStrategyError badStrat))

  incDeps <- liftE $ unionMergeMaps InclusionDependenciesField preference graph inclusionDependencies (inclusionDependencies contextA) (inclusionDependencies contextB)
  relVars <- unionMergeRelVars preference graph (relationVariables contextA) (relationVariables contextB)
  atomFuncs <- liftE $ unionMergeAtomFunctions preference graph (atomFunctions contextA) (atomFunctions contextB)
  notifs <- liftE $ unionMergeMaps NotificationsField preference graph notifications (notifications contextA) (notifications contextB)
  types <- liftE $ unionMergeTypeConstructorMapping preference graph (typeConstructorMapping contextA) (typeConstructorMapping contextB)
  dbcFuncs <- liftE $ unionMergeDatabaseContextFunctions preference graph (dbcFunctions contextA) (dbcFunctions contextB)
  registeredQs <- liftE $ unionMergeRegisteredQueries preference graph (registeredQueries contextA) (registeredQueries contextB)
  -- TODO: add merge of subschemas
  let newContext = DatabaseContext {
        inclusionDependencies = incDeps,
        relationVariables = relVars,
        atomFunctions = atomFuncs,
        dbcFunctions = dbcFuncs,
        notifications = notifs,
        typeConstructorMapping = types,
        registeredQueries = registeredQs
        }
      newSchemas = Schemas newContext (subschemas t1)
  pure $ UncommittedTransaction $ addMerkleHash graph $
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
    else case S.toList parentIds' of
      [] -> Left RootTransactionTraversalError
      arbitraryParent : _ -> backtrackGraph graph arbitraryParent btrack

-- | Create a temporary branch for commit, merge the result to head, delete the temporary branch. This is useful to atomically commit a transaction, avoiding a TransactionIsNotHeadError but trading it for a potential MergeError.
--this is not a GraphOp because it combines multiple graph operations
autoMergeToHead :: UTCTime -> (TransactionId, TransactionId, TransactionId) -> DisconnectedTransaction -> HeadName -> MergeStrategy -> TransactionGraph -> Either RelationalError (DisconnectedTransaction, TransactionGraphIncrementalWriteInfo)
autoMergeToHead stamp' (tempBranchTransId, tempCommitTransId, mergeTransId) discon mergeToHeadName strat graph = do
  let tempBranchName = "mergebranch_" <> U.toText tempBranchTransId
  --create the temp branch
  (discon', mtrans', graph') <- evalAlterTransactionGraphExpr stamp' tempBranchTransId discon graph (Branch tempBranchName)

  --commit to the new branch- possible future optimization: don't require fsync for this- create a temp commit type
  (discon'', mtrans'', graph'') <- evalAlterTransactionGraphExpr stamp' tempCommitTransId discon' graph' Commit

  --jump to merge head
  discon''' <- evalTransactionGraphExpr discon'' graph'' (JumpToHead mergeToHeadName)

  --create the merge
  (discon'''', mtrans'''', graph'''') <- evalAlterTransactionGraphExpr stamp' mergeTransId discon''' graph'' (MergeTransactions strat tempBranchName mergeToHeadName)

  --delete the temp branch
  (discon''''', mtrans''''', graph''''') <- evalAlterTransactionGraphExpr stamp' tempBranchTransId discon'''' graph'''' (DeleteBranch tempBranchName)
  {-
  let rel = runReader (evalRelationalExpr (RelationVariable "s" ())) (mkRelationalExprState $ D.concreteDatabaseContext discon'''')
  traceShowM rel
-}
  pure (discon''''',
        TransactionGraphIncrementalWriteInfo {
           uncommittedTransactions = S.fromList (catMaybes [mtrans',
                                                            mtrans'',
                                                            mtrans'''',
                                                            mtrans'''''
                                                           ]),
           newGraph = graph'''''
           })

addMerkleHash :: TransactionGraph -> Transaction -> Transaction
addMerkleHash graph trans = Transaction (transactionId trans) newInfo (schemas trans)
  where
    newInfo = (transactionInfo trans) { merkleHash = calculateMerkleHash trans graph }
  -- the new hash includes the parents' ids, the current id, and the hash of the context, and the merkle hashes of the parent transactions
calculateMerkleHash :: Transaction -> TransactionGraph -> MerkleHash
calculateMerkleHash trans graph = hashTransaction trans parentTranses
  where
    parentTranses =
      case transactionsForIds (parentIds trans) graph of
        Left RootTransactionTraversalError -> mempty
        Left e -> error ("failed to find transaction in Merkle hash construction: " ++ show e)
        Right t -> t

validateMerkleHash :: Transaction -> TransactionGraph -> Either MerkleValidationError ()
validateMerkleHash trans graph =
  when (expectedHash /= actualHash) $
    Left (MerkleValidationError (transactionId trans) expectedHash actualHash)
  where
    expectedHash = merkleHash (transactionInfo trans)
    actualHash = calculateMerkleHash trans graph

data MerkleValidationError = MerkleValidationError TransactionId MerkleHash MerkleHash
  deriving (Show,Eq, Generic)

validateMerkleHashes :: TransactionGraph -> Either [MerkleValidationError] ()
validateMerkleHashes graph =
  if null errs then pure () else Left errs
  where
    errs = S.foldr validateTrans [] (transactionsForGraph graph)
    validateTrans trans acc =
      case validateMerkleHash trans graph of
        Left err -> err : acc
        _ -> acc

-- | Ensure that referenced transactions remain in the graph.
validateConnectivity :: TransactionGraph -> Either RelationalError TransactionGraph
validateConnectivity graph = do
  let validateTrans trans =
        mapM_ (`transactionForId` graph) (referencedTransactionIds (concreteDatabaseContext trans))
  mapM_ validateTrans (transactionsForGraph graph)
  pure graph


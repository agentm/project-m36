{-# LANGUAGE CPP, FlexibleContexts #-}
--Transaction Merge Engines
module ProjectM36.TransactionGraph.Merge where
import ProjectM36.Base
import ProjectM36.AttributeNamesBase (GraphRefRelationalExpr)
import ProjectM36.Error
import ProjectM36.ValueMarker
import ProjectM36.TransactionGraph.Types
import ProjectM36.TransactionGraph
import ProjectM36.TransactionGraph.FindTransaction
import ProjectM36.NormalizeExpr
import ProjectM36.RelationalExpression
import ProjectM36.RelationalExpression.Types
import ProjectM36.DatabaseContext.Types
import ProjectM36.AccessControlList hiding (normalize)
import ProjectM36.DatabaseContext.Fields
import ProjectM36.Transaction.Types
import qualified ProjectM36.IsomorphicSchema.Types as S

#if MIN_VERSION_base(4,18,0)
import Control.Monad (foldM)
import Control.Monad.Except
#else
import Control.Monad.Except hiding (join)
#endif
import Data.Time.Clock (UTCTime)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified ProjectM36.TypeConstructorDef as TCD
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Control.Monad.Reader

data MergePreference = PreferFirst | PreferSecond | PreferNeither

-- Check for overlapping keys. If the values differ, try a preference resolution
unionMergeMaps :: Eq a => DatabaseContextField -> MergePreference -> TransactionGraph -> (DatabaseContext -> ValueMarker (M.Map StringType a)) -> ValueMarker (M.Map StringType a) -> ValueMarker (M.Map StringType a) -> Either RelationalError (ValueMarker (M.Map StringType a))
unionMergeMaps _field _prefer _graph _f a@(NotChangedSinceMarker tidA) (NotChangedSinceMarker tidB)
  | tidA == tidB =
      pure a
unionMergeMaps field prefer graph f v_mapA v_mapB = do
  mapA <- resolveValueMarker graph f v_mapA
  mapB <- resolveValueMarker graph f v_mapB
  let diff = foldr folder Nothing overlappingKeys
      overlappingKeys = S.intersection (M.keysSet mapA) (M.keysSet mapB)
      folder _key (Just ret) = Just ret
      folder key acc =
        if mapA M.! key /= mapB M.! key then
          Just key
          else
          acc
  case prefer of
    PreferFirst -> pure (ValueMarker $ M.union mapA mapB)
    PreferSecond -> pure $ ValueMarker $ M.union mapB mapA
    PreferNeither -> case diff of
                       Nothing -> pure $ ValueMarker $ M.union mapA mapB
                       Just violatingKey ->
                         Left (MergeTransactionError (StrategyViolatesComponentMergeError field violatingKey))
                     
-- perform the merge even if the attributes are different- is this what we want? Obviously, we need finer-grained merge options.
unionMergeRelation :: MergePreference -> GraphRefRelationalExpr -> GraphRefRelationalExpr -> GraphRefRelationalExprM GraphRefRelationalExpr
unionMergeRelation prefer relA relB = do
  let unioned = Union relA relB
      mergeErr e = MergeTransactionError (StrategyViolatesRelationVariableMergeError e)
      preferredRelVar =
        case prefer of
          PreferFirst -> pure relA
          PreferSecond -> pure relB
          PreferNeither -> throwError (MergeTransactionError StrategyWithoutPreferredBranchResolutionMergeError)
      handler AttributeNamesMismatchError{} = preferredRelVar
      handler err' = throwError (mergeErr err')
  --typecheck first?
  (evalGraphRefRelationalExpr unioned >> pure (Union relA relB)) `catchError` handler

--try to execute unions against the relvars contents -- if a relvar only appears in one context, include it
unionMergeRelVars ::
  MergePreference ->
  TransactionGraph ->
  ValueMarker RelationVariables ->
  ValueMarker RelationVariables ->
  GraphRefRelationalExprM (ValueMarker RelationVariables)
unionMergeRelVars _prefer _graph v@(NotChangedSinceMarker tidA) (NotChangedSinceMarker tidB)
  | tidA == tidB =
      pure v
unionMergeRelVars prefer graph v_relvarsA v_relvarsB = do
  relvarsA <- case resolveValueMarker graph relationVariables v_relvarsA of
       Left err -> throwError err
       Right val -> pure val
  relvarsB <- case resolveValueMarker graph relationVariables v_relvarsB of
       Left err -> throwError err
       Right val -> pure val
  let allNames = S.toList (S.union (M.keysSet relvarsA) (M.keysSet relvarsB))
  ValueMarker <$> foldM (\acc name -> do
            mergedRel <- do
              let findRel = M.lookup name
                  lookupA = findRel relvarsA
                  lookupB = findRel relvarsB
              case (lookupA, lookupB) of
                (Just relA, Just relB) -> do
                  unionMergeRelation prefer relA relB
                (Nothing, Just relB) -> pure relB 
                (Just relA, Nothing) -> pure relA 
                (Nothing, Nothing) -> error "impossible relvar naming lookup"
            pure $ M.insert name mergedRel acc
            ) M.empty allNames

-- if two functions have the same name, ensure that the functions are identical, otherwise, conflict or prefer
--because we don't have a bytecode, there is no way to verify that function bodies are equal, so if the types match up, just choose the first function. This is a serious bug, but intractable until we have a function bytecode.
unionMergeAtomFunctions :: MergePreference -> TransactionGraph -> ValueMarker AtomFunctions -> ValueMarker AtomFunctions -> Either RelationalError (ValueMarker AtomFunctions)
unionMergeAtomFunctions _prefer _g v@(NotChangedSinceMarker tidA) (NotChangedSinceMarker tidB)
  | tidA == tidB =
    pure v
unionMergeAtomFunctions prefer graph v_funcsA v_funcsB = do
  funcsA <- resolveValueMarker graph atomFunctions v_funcsA
  funcsB <- resolveValueMarker graph atomFunctions v_funcsB  
  pure $ ValueMarker $
    case prefer of
      PreferFirst -> HS.union funcsA funcsB
      PreferSecond -> HS.union funcsB funcsA
      PreferNeither -> HS.union funcsA funcsB

unionMergeTypeConstructorMapping :: MergePreference -> TransactionGraph -> ValueMarker TypeConstructorMapping -> ValueMarker TypeConstructorMapping -> Either RelationalError (ValueMarker TypeConstructorMapping)
unionMergeTypeConstructorMapping _prefer _graph v@(NotChangedSinceMarker tidA) (NotChangedSinceMarker tidB)
  | tidA == tidB =
      pure v
unionMergeTypeConstructorMapping prefer graph v_typesA v_typesB = do
  typesA <- resolveValueMarker graph typeConstructorMapping v_typesA
  typesB <- resolveValueMarker graph typeConstructorMapping v_typesB  
  
  let allFuncNames = S.fromList $ map (\(tc,_) -> TCD.name tc) (typesA ++ typesB)
  val <- foldM (\acc name -> do
            let findType tcm = case filter (\(t,_) -> TCD.name t == name) tcm of
                  [] -> Nothing
                  [x] -> Just x
                  _ -> error $ "multiple names matching in TypeConstructorMapping for " <> T.unpack name
                lookupA = findType typesA
                lookupB = findType typesB
                cat t = pure (t : acc)
            case (lookupA, lookupB) of
               (Nothing, Nothing) -> error "type name lookup failure"
               (Just typeA, Nothing) -> cat typeA
               (Nothing, Just typeB) -> cat typeB
               (Just typeA, Just typeB) -> if typeA == typeB then
                                             cat typeA
                                           else --merge conflict
                                             case prefer of 
                                               PreferFirst -> cat typeA
                                               PreferSecond -> cat typeB
                                               PreferNeither -> Left (MergeTransactionError StrategyViolatesTypeConstructorMergeError)
            ) [] (S.toList allFuncNames)
  pure (ValueMarker val)

unionMergeDatabaseContextFunctions :: MergePreference -> TransactionGraph -> ValueMarker DatabaseContextFunctions -> ValueMarker DatabaseContextFunctions -> Either RelationalError (ValueMarker DatabaseContextFunctions)
unionMergeDatabaseContextFunctions _prefer _graph v@(NotChangedSinceMarker tidA) (NotChangedSinceMarker tidB)
  | tidA == tidB =
    pure v
unionMergeDatabaseContextFunctions prefer graph v_funcsA v_funcsB = do
  funcsA <- resolveValueMarker graph dbcFunctions v_funcsA
  funcsB <- resolveValueMarker graph dbcFunctions v_funcsB
  pure $ ValueMarker $
    case prefer of
      PreferFirst -> HS.union funcsA funcsB
      PreferSecond -> HS.union funcsB funcsA
      PreferNeither -> HS.union funcsA funcsB
  
unionMergeRegisteredQueries :: MergePreference -> TransactionGraph -> ValueMarker RegisteredQueries -> ValueMarker RegisteredQueries -> Either RelationalError (ValueMarker RegisteredQueries)
unionMergeRegisteredQueries _prefer _g v@(NotChangedSinceMarker tidA) (NotChangedSinceMarker tidB)
  | tidA == tidB = pure v
unionMergeRegisteredQueries prefer graph v_regQsA v_regQsB = do
  regQsA <- resolveValueMarker graph registeredQueries v_regQsA
  regQsB <- resolveValueMarker graph registeredQueries v_regQsB
  
  v <- case prefer of
         PreferFirst -> pure (M.union regQsA regQsB)
         PreferSecond -> pure (M.union regQsB regQsA)
         PreferNeither -> do
           let isect = M.filter id $ M.mapWithKey (\qname val -> M.lookup qname regQsB /= Just val) (M.intersection regQsA regQsB)
           --if the values in the intersection are the same, we can merge them      
           if M.null isect then
             pure (M.union regQsA regQsB)
             else
             Left (MergeTransactionError (StrategyViolatesRegisteredQueryMergeError (M.keys isect)))
  pure (ValueMarker v)
        
unionMergeACLs :: MergePreference -> TransactionGraph -> ValueMarker DatabaseContextACL -> ValueMarker DatabaseContextACL -> Either RelationalError (ValueMarker DatabaseContextACL)
unionMergeACLs _prefer _g v@(NotChangedSinceMarker tidA) (NotChangedSinceMarker tidB)
  | tidA == tidB = pure v
unionMergeACLs _prefer graph v_aclA v_aclB = do
  aclA <- resolveValueMarker graph acl v_aclA
  aclB <- resolveValueMarker graph acl v_aclB

  pure $ ValueMarker $ aclA <> aclB

mergeTransactions :: UTCTime -> TransactionId -> TransactionId -> CheckConstraints -> MergeStrategy -> MergeHeadNames -> GraphRefRelationalExprM (UncommittedTransaction, TransactionGraph)
mergeTransactions stamp' newId parentId checkConstraints' mergeStrategy mergeHeadNames = do
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
          let normalizerF ctx graph' relExpr = normalize (Just ctx) graph' relExpr
              dbctx = concreteDatabaseContext (_uncommittedTransaction mergedTrans)
          case checkConstraints' normalizerF dbctx newId graph of
            Left err -> throwError err
            Right _ -> do
              let newGraph'' = TransactionGraph (transactionHeadsForGraph newGraph') (transactionsForGraph newGraph')
              pure (uTrans, newGraph'')

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
  acls <- liftE $ unionMergeACLs preference graph (acl contextA) (acl contextB)
  -- TODO: add merge of subschemas
  let newContext = DatabaseContext {
        inclusionDependencies = incDeps,
        relationVariables = relVars,
        atomFunctions = atomFuncs,
        dbcFunctions = dbcFuncs,
        notifications = notifs,
        typeConstructorMapping = types,
        registeredQueries = registeredQs,
        acl = acls
        }
      newSchemas = S.Schemas newContext (subschemas t1)
  pure $ UncommittedTransaction $ addMerkleHash graph $
    Transaction newId (TransactionInfo {
                          parents = NE.fromList [transactionId t1,
                                                  transactionId t2],
                            stamp = stamp',
                            merkleHash = mempty }) newSchemas

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

-- tests for transaction merging
import Test.HUnit
import ProjectM36.Base
import ProjectM36.Attribute
import ProjectM36.Relation
import ProjectM36.ValueMarker
import ProjectM36.TransactionInfo as TI
import ProjectM36.DatabaseContext.Types
import ProjectM36.TransactionGraph
import ProjectM36.IsomorphicSchema.Types hiding (concreteDatabaseContext)
import ProjectM36.TransactionGraph.Types
import ProjectM36.Error
import ProjectM36.Key
import ProjectM36.DatabaseContext (toDatabaseContext)
import qualified ProjectM36.DatabaseContext.Basic as DBC
import ProjectM36.RelationalExpression
import ProjectM36.StaticOptimizer
import ProjectM36.Transaction.Types
import ProjectM36.DatabaseContext.Fields

import qualified Data.ByteString.Lazy as BS
import System.Exit
import Data.Word
import qualified Data.UUID as U
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.Time.Clock
import Data.Time.Calendar
import Data.Either

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

testTime :: UTCTime
testTime = UTCTime (fromGregorian 1980 01 01) (secondsToDiffTime 1000)
  
main :: IO ()           
main = do 
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testList :: Test
testList = TestList [
  testSubGraphToFirstAncestorBasic,
  testSubGraphToFirstAncestorSnipBranch,
  testSubGraphToFirstAncestorMoreTransactions,
  testSelectedBranchMerge,
  testUnionMergeStrategyOnRelVars,
  testUnionMergeStrategyOnInclusionDependencies,
  testUnionMergeStrategyOnInclusionDependenciesConflict,
  testUnionPreferMergeStrategy,
  testUnionMergeIncDepViolation
  ]

-- | Create a transaction graph with two branches and no changes between them.
{-
root 1
|--- branchA aaaa 
|--- branchB bbbb
-}

createTrans :: TransactionId -> TransactionInfo -> DatabaseContext -> UncommittedTransaction
createTrans tid info' ctx = UncommittedTransaction $ Transaction tid info' (Schemas ctx emptyValue)

basicTransactionGraph :: IO TransactionGraph
basicTransactionGraph = do
  let bsGraph = bootstrapTransactionGraph testTime uuidRoot (toDatabaseContext DBC.basicDatabaseContext)
      rootTrans = fromMaybe (error "bonk") (transactionForHead "master" bsGraph)
      uuidA = fakeUUID [10]
      uuidB = fakeUUID [11]
      uuidRoot = fakeUUID [1]
      rootContext = concreteDatabaseContext rootTrans
  (_, bsGraph') <- addTransaction "basicTransactionGraph" "branchA" (createTrans uuidA (TI.singleParent uuidRoot testTime) rootContext) bsGraph
  (_, bsGraph'') <- addTransaction "basicTransactionGraph2" "branchB" (createTrans uuidB (TI.singleParent uuidRoot testTime) rootContext) bsGraph'
  pure bsGraph''
  
addTransaction :: String -> HeadName -> UncommittedTransaction -> TransactionGraph -> IO (UncommittedTransaction, TransactionGraph)
addTransaction ctx headName transaction graph = case addTransactionToGraph headName transaction graph of
  Left err -> assertFailure (ctx <> ": " <> show err) >> error ""
  Right (t,g) -> pure (t,g)
              
fakeUUID :: [Word8] -> TransactionId
fakeUUID x = fromMaybe (error "impossible uuid") (U.fromByteString repbs)
  where repbs = BS.pack (take 16 (cycle x))

data TestBranchName = BranchA | BranchB
  deriving (Show, Eq)

-- makes a fake uuid based on the branch name Char and increment Word8
fakeUUID' :: TestBranchName -> Word8 -> TransactionId
fakeUUID' branchName version =
  fakeUUID [intBranch, version]
  where
    intBranch = case branchName of
                  BranchA -> 10
                  BranchB -> 11
  
assertEither :: (Show a) => String -> Either a b -> IO b
assertEither ctx x = case x of
  Left err -> assertFailure (ctx ++ ": " ++ show err) >> undefined
  Right val -> pure val
  
assertGraph :: TransactionGraph -> IO ()  
assertGraph graph = case validateGraph graph of
  Nothing -> pure ()
  Just errs -> assertFailure (show errs)
  
assertMaybe :: Maybe a -> String -> IO a
assertMaybe x msg = case x of
  Nothing -> assertFailure msg >> undefined
  Just x' -> pure x'
  
-- | Test that a subgraph of the basic graph is equal to the original graph (nothing to filter).
testSubGraphToFirstAncestorBasic :: Test  
testSubGraphToFirstAncestorBasic = TestCase $ do
  graph <- basicTransactionGraph 
  assertGraph graph
  transA <- assertMaybe (transactionForHead "branchA" graph) "failed to get branchA"
  transB <- assertMaybe (transactionForHead "branchB" graph) "failed to get branchB"
  subgraph <- assertEither "testSubGraphToFirstAncestorBasic" $ subGraphOfFirstCommonAncestor graph (transactionHeadsForGraph graph) transA transB S.empty
  let graphEq graphArg = S.map transactionId (transactionsForGraph graphArg)  
  assertEqual "no graph changes" (graphEq graph) (graphEq subgraph) 
  
-- | Test that a branch anchored at the root transaction is removed when using the first ancestor function.
testSubGraphToFirstAncestorSnipBranch :: Test  
testSubGraphToFirstAncestorSnipBranch = TestCase $ do
  baseGraph <- basicTransactionGraph  
  transA <- assertMaybe (transactionForHead "branchA" baseGraph) "failed to get branchA"
  transB <- assertMaybe (transactionForHead "branchB" baseGraph) "failed to get branchB"
  (_, graph) <- addTransaction "testSubGraphToFirstAncestorSnipBranch" "branchC" (createTrans (fakeUUID [12]) (TI.singleParent (fakeUUID [1]) testTime) (concreteDatabaseContext transA)) baseGraph
  subgraph <- assertEither "testSubGraphToFirstAncestorSnipBranch" $ subGraphOfFirstCommonAncestor graph (transactionHeadsForGraph baseGraph) transA transB S.empty
  assertGraph subgraph
  let graphEq graphArg = S.map transactionId (transactionsForGraph graphArg)  
  assertEqual "failed to snip branch" (graphEq baseGraph) (graphEq subgraph)
  
-- | Test that the subgraph function recurses properly through multiple parent transactions.  
testSubGraphToFirstAncestorMoreTransactions :: Test
testSubGraphToFirstAncestorMoreTransactions = TestCase $ do
  graph <- basicTransactionGraph
  assertGraph graph
  
-- add another relvar to branchB
  branchBTrans <- assertMaybe (transactionForHead "branchB" graph) "failed to get branchB head"
  let env = mkDatabaseContextEvalEnv (transactionId branchBTrans) graph dudFunctionUtils
      branchBContext = concreteDatabaseContext branchBTrans
  updatedBranchBContext <- case runDatabaseContextEvalMonad branchBContext env (optimizeAndEvalDatabaseContextExpr True (Assign "branchBOnlyRelvar" (ExistingRelation relationTrue))) of
    Left err -> assertFailure (show err) >> undefined
    Right st -> pure $ dbc_context st
  (_, graph') <- addTransaction "testSubGraphToFirstAncestorMoreTransactions" "branchB" (createTrans (fakeUUID [3]) (TI.singleParent (transactionId branchBTrans) testTime) updatedBranchBContext) graph
  branchBTrans' <- assertMaybe (transactionForHead "branchB" graph') "failed to get branchB head"  
  assertEqual "branchB id 3" (fakeUUID [3]) (transactionId branchBTrans')  
  
  -- add another transaction to branchA
  branchATrans <- assertMaybe (transactionForHead "branchA" graph) "failed to get branchA head"  
  (_, graph'') <- addTransaction "testSubGraphToFirstAncestorMoreTransactions2" "branchA" (createTrans (fakeUUID [4]) (TI.singleParent (transactionId branchATrans) testTime) (concreteDatabaseContext branchATrans)) graph'
  branchATrans' <- assertMaybe (transactionForHead "branchA" graph'') "failed to get branchA head"
  assertEqual "branchA id 4" (fakeUUID [4]) (transactionId branchATrans')
                                              
  --retrieve subgraph                                            
  let subGraphHeads = M.filter (\t -> transactionId t `elem` [fakeUUID [3], fakeUUID [4]]) (transactionHeadsForGraph graph'')
  subgraph <- assertEither "testSubGraphToFirstAncestorMoreTransactions" $ subGraphOfFirstCommonAncestor graph'' subGraphHeads branchATrans' branchBTrans' S.empty
  --verify that the subgraph includes both the heads and the common ancestor
  let expectedTransSet = S.fromList (map fakeUUID [[1],[3],[4]])
  assertBool "validate transactions in subgraph" (S.isProperSubsetOf expectedTransSet (S.map transactionId (transactionsForGraph subgraph)))

dudFunctionUtils :: DatabaseContextFunctionUtils
dudFunctionUtils = DatabaseContextFunctionUtils {
  executeDatabaseContextExpr = error "test executeDatabaseContextExpr",
  executeRelationalExpr = error "test executeRelationalExpr"
  }
  
testSelectedBranchMerge :: Test
testSelectedBranchMerge = TestCase $ do
  graph <- basicTransactionGraph
  assertGraph graph
  
  -- add a relvar "branchBOnlyRelvar" to branchB only
  branchBTrans <- assertMaybe (transactionForHead "branchB" graph) "failed to get branchB head"
  let env = mkDatabaseContextEvalEnv (transactionId branchBTrans) graph dudFunctionUtils
      branchBContext = concreteDatabaseContext branchBTrans  
  updatedBranchBContext <- case runDatabaseContextEvalMonad branchBContext env (optimizeAndEvalDatabaseContextExpr True (Assign "branchBOnlyRelvar" (ExistingRelation relationTrue))) of
    Left err -> assertFailure (show err) >> undefined
    Right st -> pure (dbc_context st)
    -- add the transaction with the "branchBOnlyRelvar" to the graph at transaction ID 3
  (_, graph') <- addTransaction "testSelectedBranchMerge" "branchB" (createTrans (fakeUUID' BranchB 2) (TI.singleParent (transactionId branchBTrans) testTime) updatedBranchBContext) graph
  --create the merge transaction in the graph
  let eGraph' = runGraphRefRelationalExprM gfEnv $ mergeTransactions testTime (fakeUUID' BranchB 3 {-new trans ID-}) (fakeUUID' BranchB 2 {-add transaction to parent ID-}) (SelectedBranchMergeStrategy "branchA") (MergeHeadNames { sourceHead = "branchA", targetHead = "branchB" })
      gfEnv = freshGraphRefRelationalExprEnv Nothing graph'
      
  (_, graph'') <- assertEither "testSelectedBranchMerge" eGraph'

--  putStrLn $ showGraphStructureX True graph''

  assertGraph graph''
  --validate that the branchB remains
  branchBTrans' <- assertMaybe (transactionForHead "branchB" graph'') "failed to find branchB head"
  assertEqual "head of merged transaction was removed" (fakeUUID' BranchB 3) (transactionId branchBTrans')

  --validate that the branchB relvar does *not* appear in the merge because branchA was selected
  mergeTrans <- assertEither "testSelectedBranchMerge2" (transactionForId (fakeUUID' BranchB 3) graph'')
  mergedRVs <- assertEither "testSelectedBranchMerge3" $ resolveDBC' graph'' (concreteDatabaseContext mergeTrans) relationVariables
  assertBool "branchOnlyRelvar is present in merge" (M.notMember "branchBOnlyRelvar" mergedRVs)

-- try various individual component conflicts and check for resolution
testUnionPreferMergeStrategy :: Test
testUnionPreferMergeStrategy = TestCase $ do
  -- create a graph with a relvar conflict
  graph <- basicTransactionGraph
  branchATrans <- assertMaybe (transactionForHead "branchA" graph) "branchATrans"
  branchBTrans <- assertMaybe (transactionForHead "branchB" graph) "branchBTrans"
  branchBRelVar <- assertEither "testUnionPreferMergeStrategy" $ mkRelationFromList (attributesFromList [Attribute "conflict" IntAtomType]) []
  branchARVs <- assertEither "testUnionPreferMergeStrategy2" $ resolveDBC' graph (concreteDatabaseContext branchATrans) relationVariables
  branchBRVs <- assertEither "testUnionPreferMergeStrategy3" $ resolveDBC' graph (concreteDatabaseContext branchBTrans) relationVariables
  let branchAContext = (concreteDatabaseContext branchATrans) {
        relationVariables = ValueMarker $ M.insert conflictRelVarName branchARelVar branchARVs
        }
      branchARelVar = ExistingRelation relationTrue 
      branchBContext = (concreteDatabaseContext branchBTrans) {
            relationVariables = ValueMarker $  M.insert conflictRelVarName (ExistingRelation branchBRelVar) branchBRVs
            }
      conflictRelVarName = "conflictRelVar" 

  (_, graph') <- addTransaction "testUnionPreferMergeStrategy" "branchA" (createTrans (fakeUUID [3]) (TI.singleParent (transactionId branchATrans) testTime) branchAContext) graph
  (_, graph'') <- addTransaction "testUnionPreferMergeStrategy2" "branchB" (createTrans (fakeUUID [4]) (TI.singleParent (transactionId branchBTrans) testTime) branchBContext) graph'
  -- validate that the conflict is hidden because we preferred a branch
  let merged = runGraphRefRelationalExprM env $ mergeTransactions testTime (fakeUUID [5]) (fakeUUID [3]) (UnionPreferMergeStrategy "branchB") (MergeHeadNames { sourceHead = "branchA", targetHead = "branchB" })
      env = freshGraphRefRelationalExprEnv Nothing graph''
  case merged of
    Left err -> assertFailure ("expected merge success: " ++ show err)
    Right (UncommittedTransaction trans, _) -> do
      let dbc = concreteDatabaseContext trans
      mergedRVs <- assertEither "testUnionPreferMergeStrategy" $ resolveDBC' graph'' dbc relationVariables
      let rvExpr = fromMaybe (error "conflictRelVarName") $ M.lookup conflictRelVarName mergedRVs
          reEnv = freshGraphRefRelationalExprEnv (Just dbc) graph
          eRvRel = runGraphRefRelationalExprM reEnv (evalGraphRefRelationalExpr rvExpr)
      assertEqual "branchB relvar preferred in conflict" (Right branchBRelVar) eRvRel
  
-- try various individual component conflicts and check for merge failure
testUnionMergeStrategyOnRelVars :: Test
testUnionMergeStrategyOnRelVars = TestCase $ do
  graph <- basicTransactionGraph
  assertGraph graph
  
  branchBTrans <- assertMaybe (transactionForHead "branchB" graph) "failed to get branchB head"
  -- add another relvar to branchB - branchBOnlyRelvar should appear in the merge  

  branchBRVs <- assertEither "testUnionMergeStrategy" $ resolveDBC' graph (concreteDatabaseContext branchBTrans) relationVariables
  let updatedBranchBContext = (concreteDatabaseContext branchBTrans) {
        relationVariables = ValueMarker $ M.insert branchBOnlyRelVarName branchBOnlyRelVar branchBRVs
        }
      branchBOnlyRelVar = ExistingRelation relationTrue
      branchBOnlyRelVarName = "branchBOnlyRelVar"
  (_, graph') <- addTransaction "testUnionMergeStrategy" "branchB" (createTrans (fakeUUID [3]) (TI.singleParent (transactionId branchBTrans) testTime) updatedBranchBContext) graph
  let env = freshGraphRefRelationalExprEnv Nothing graph'
  (UncommittedTransaction trans, graph'') <- assertEither "testUnionMergeStrategy" $ runGraphRefRelationalExprM env $ mergeTransactions testTime (fakeUUID [5]) (fakeUUID [10]) UnionMergeStrategy (MergeHeadNames {sourceHead = "branchA", targetHead = "branchB" })
  branchBRVs' <- assertEither "testUnionMergeStrategy3" $ resolveDBC' graph'' (concreteDatabaseContext trans) relationVariables
  let rvExpr = fromMaybe (error "branchOnlyBRelVarName") $ M.lookup branchBOnlyRelVarName branchBRVs'
      rvRel = fromRight (error "rvRel") $ runGraphRefRelationalExprM reEnv (evalGraphRefRelationalExpr rvExpr)
      reEnv = freshGraphRefRelationalExprEnv (Just (concreteDatabaseContext trans)) graph'
      
  assertEqual "branchBOnlyRelVar should appear in the merge" relationTrue rvRel

testUnionMergeStrategyOnInclusionDependencies :: Test
testUnionMergeStrategyOnInclusionDependencies = TestCase $ do
  graph <- basicTransactionGraph
  assertGraph graph

  branchBTrans <- assertMaybe (transactionForHead "branchB" graph) "failed to get branchB head"
  branchATrans <- assertMaybe (transactionForHead "branchA" graph) "failed to get branchA head"
  branchAIncDeps <- assertEither "testUnionMergeStrategy2" $ resolveDBC' graph (concreteDatabaseContext branchATrans) inclusionDependencies
  branchBIncDeps <- assertEither "testUnionMergeStrategy2" $ resolveDBC' graph (concreteDatabaseContext branchBTrans) inclusionDependencies  

  -- update branchA with exclusive incdep
  let updatedBranchAContext = (concreteDatabaseContext branchATrans) {
        inclusionDependencies = ValueMarker $ M.insert branchAOnlyIncDepName branchAOnlyIncDep branchAIncDeps
        }
      updatedBranchBContext = (concreteDatabaseContext branchBTrans) {
        inclusionDependencies = ValueMarker $ M.insert branchBOnlyIncDepName branchBOnlyIncDep branchBIncDeps
        }        
      branchAOnlyIncDep = InclusionDependency (ExistingRelation relationTrue) (ExistingRelation relationTrue)
      branchAOnlyIncDepName = "branchAOnlyIncDep"
      branchBOnlyIncDep = InclusionDependency (ExistingRelation relationTrue) (ExistingRelation relationTrue)
      branchBOnlyIncDepName = "branchBOnlyIncDep"

  (_, graph') <- addTransaction "testUnionMergeStrategy4" "branchA" (createTrans (fakeUUID' BranchA 2) (TI.singleParent (transactionId branchATrans) testTime) updatedBranchAContext) graph
  (_, graph'') <- addTransaction "testUnionMergeStrategy4" "branchB" (createTrans (fakeUUID' BranchB 2) (TI.singleParent (transactionId branchBTrans) testTime) updatedBranchBContext) graph'

  -- merge branchA to branchB
  let eMergeGraph = runGraphRefRelationalExprM gfEnv $ mergeTransactions testTime (fakeUUID' BranchB 3{- newid -}) (fakeUUID' BranchB 2{-parentid-}) UnionMergeStrategy (MergeHeadNames { sourceHead = "branchA", targetHead = "branchB" })
      gfEnv = freshGraphRefRelationalExprEnv Nothing graph''

--  putStrLn $ showGraphStructureX False graph'
      
  case eMergeGraph of
    Left err -> assertFailure ("expected merge success: " ++ show err)
    Right (_, mergeGraph) -> do
      -- check that the new merge transaction has the correct parents
      mergeTrans <- assertEither "testUnionMergeStrategy5" $ transactionForId (fakeUUID' BranchB 3) mergeGraph
      let mergeContext = concreteDatabaseContext mergeTrans
      --check merge transaction parent ids
      assertEqual "merge transaction parents" (parentIds mergeTrans) (S.fromList [fakeUUID' BranchA 2, fakeUUID' BranchB 2])
      -- check that the new merge tranasction has elements from both A and B branches
      mergeIncDeps <- assertEither "testUnionMergeStrategy7" $ resolveDBC' mergeGraph mergeContext inclusionDependencies
      -- check that inc deps from branchA and branchB are present (union)
      assertEqual "inclusion dependencies from branchA and branchB" (S.fromList [branchAOnlyIncDepName, branchBOnlyIncDepName]) (M.keysSet mergeIncDeps)

--          rvRel' = fromRight (error "rvRel'") $ runGraphRefRelationalExprM reEnv' (evalGraphRefRelationalExpr rvExpr')
--          reEnv' = freshGraphRefRelationalExprEnv (Just mergeContext) graph
--      assertEqual "merge transaction relvars" relationTrue rvRel'
      assertEqual "merge transaction incdeps" (Just branchAOnlyIncDep) (M.lookup branchAOnlyIncDepName mergeIncDeps)

-- test an expected conflict- add incdeps with same name but different attributes
testUnionMergeStrategyOnInclusionDependenciesConflict :: Test
testUnionMergeStrategyOnInclusionDependenciesConflict = TestCase $ do
  graph <- basicTransactionGraph
  assertGraph graph

  branchATrans <- assertMaybe (transactionForHead "branchA" graph) "retrieving head transaction for expected conflict"
  branchBTrans <- assertMaybe (transactionForHead "branchB" graph) "retrieving head transaction for expected conflict"
  let incDepName = "conflictIncDep"
      incDepA = InclusionDependency (ExistingRelation relationTrue) (ExistingRelation relationTrue)
      incDepB = InclusionDependency (ExistingRelation relationTrue) (ExistingRelation relationFalse)
      conflictContextA = (concreteDatabaseContext branchATrans) {
        inclusionDependencies = ValueMarker $ M.singleton incDepName incDepA
        }
      conflictContextB = (concreteDatabaseContext branchBTrans) {
        inclusionDependencies = ValueMarker $ M.singleton incDepName incDepB
        }
  -- add to branchA
  (_, graph') <- addTransaction "testUnionMergeStrategy10" "branchA" (createTrans (fakeUUID' BranchA 2) (TI.singleParent (transactionId branchATrans) testTime) conflictContextA) graph
  -- add to branchB
  (_, graph'') <- addTransaction "testUnionMergeStrategy10" "branchB" (createTrans (fakeUUID' BranchB 2) (TI.singleParent (transactionId branchBTrans) testTime) conflictContextB) graph'
  -- perform merge, expect conflict
  let failingMerge = runGraphRefRelationalExprM gfEnv $ mergeTransactions testTime (fakeUUID' BranchB 3) (fakeUUID' BranchA 2) UnionMergeStrategy (MergeHeadNames { sourceHead = "branchA", targetHead = "branchB" })
      gfEnv = freshGraphRefRelationalExprEnv Nothing graph''

  putStrLn $ showGraphStructureX False graph''
  case failingMerge of
    Right _ -> assertFailure "expected merge failure"
    Left err -> assertEqual "merge failure" err (MergeTransactionError (StrategyViolatesComponentMergeError InclusionDependenciesField incDepName))
    
-- test that a merge will fail if a constraint is violated
testUnionMergeIncDepViolation :: Test
testUnionMergeIncDepViolation = TestCase $ do
  graph <- basicTransactionGraph
  assertGraph graph
  
  branchBTrans <- assertMaybe (transactionForHead "branchB" graph) "failed to get branchB head"
  branchATrans <- assertMaybe (transactionForHead "branchA" graph) "failed to get branchA head"
    
  --add relvar and key constraint to both branches
  let eRel val = mkRelationFromList (attributesFromList [Attribute "x" IntAtomType, Attribute "y" IntAtomType]) [[IntAtom 1, IntAtom val]] 
      rvName = "x"
      branchArv = fromRight (error "branchArv") $ eRel 2
      branchBrv = fromRight (error "branchBrv") $ eRel 3
      branchAContext = (concreteDatabaseContext branchBTrans) {relationVariables = ValueMarker $ M.singleton rvName (ExistingRelation branchArv)}
      branchBContext = (concreteDatabaseContext branchBTrans) { relationVariables = ValueMarker $ M.singleton rvName (ExistingRelation branchBrv), 
                                                                inclusionDependencies = ValueMarker $ M.singleton incDepName incDep}      
      incDepName = "x_key"
      incDep = inclusionDependencyForKey (AttributeNames (S.singleton "x")) (RelationVariable "x" ())


   --add the rv in new commits to both branches
  (_, graph') <- addTransaction "testUnionMergeIncDepViolation" "branchB" (createTrans (fakeUUID [3]) (TI.singleParent (transactionId branchBTrans) testTime) branchBContext) graph
                  
  (_, graph'') <- addTransaction "testUnionMergeIncDepViolation2" "branchA" (createTrans (fakeUUID [4]) (TI.singleParent (transactionId branchATrans) testTime) branchAContext) graph'
  
  --check that the union merge fails due to a violated constraint
  let eMerge = runGraphRefRelationalExprM env $ mergeTransactions testTime (fakeUUID [5]) (fakeUUID [3]) UnionMergeStrategy (MergeHeadNames { sourceHead = "branchA", targetHead = "branchB" })
      env = freshGraphRefRelationalExprEnv Nothing graph''
  case eMerge of
    Left (InclusionDependencyCheckError incDepName' Nothing) -> assertEqual "incdep violation name" incDepName incDepName'
    Left err -> assertFailure ("other error: " ++ show err)
    Right _ -> assertFailure "constraint violation missing"


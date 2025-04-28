-- tests for transaction merging
import Test.HUnit
import ProjectM36.Base
import ProjectM36.Attribute
import ProjectM36.Relation
import ProjectM36.TransactionInfo as TI
import ProjectM36.DatabaseContext.Types
import ProjectM36.TransactionGraph
import ProjectM36.IsomorphicSchema.Types hiding (concreteDatabaseContext)
import ProjectM36.TransactionGraph.Types
import ProjectM36.Error
import ProjectM36.Key
import qualified ProjectM36.DisconnectedTransaction as Discon
import ProjectM36.DatabaseContext (toDatabaseContext)
import qualified ProjectM36.DatabaseContext.Basic as DBC
import ProjectM36.RelationalExpression
import ProjectM36.StaticOptimizer
import ProjectM36.Transaction.Types

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
  testUnionMergeStrategy,
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
createTrans tid info' ctx = UncommittedTransaction $ Transaction tid info' (Schemas ctx M.empty)

basicTransactionGraph :: IO TransactionGraph
basicTransactionGraph = do
  let bsGraph = bootstrapTransactionGraph testTime uuidRoot (toDatabaseContext DBC.basicDatabaseContext)
      rootTrans = fromMaybe (error "bonk") (transactionForHead "master" bsGraph)
      uuidA = fakeUUID 10
      uuidB = fakeUUID 11
      uuidRoot = fakeUUID 1
      rootContext = concreteDatabaseContext rootTrans
  (_, bsGraph') <- addTransaction "branchA" (createTrans uuidA (TI.singleParent uuidRoot testTime) rootContext) bsGraph
  (_, bsGraph'') <- addTransaction "branchB" (createTrans uuidB (TI.singleParent uuidRoot testTime) rootContext) bsGraph'
  pure bsGraph''
  
addTransaction :: HeadName -> UncommittedTransaction -> TransactionGraph -> IO (UncommittedTransaction, TransactionGraph)
addTransaction headName transaction graph = case addTransactionToGraph headName transaction graph of
  Left err -> assertFailure (show err) >> error ""
  Right (t,g) -> pure (t,g)
              
fakeUUID :: Word8 -> TransactionId
fakeUUID x = fromMaybe (error "impossible uuid") (U.fromByteString (BS.concat (replicate 4 w32)))
  where w32 = BS.pack (replicate 4 x)
  
assertEither :: (Show a) => Either a b -> IO b
assertEither x = case x of
  Left err -> assertFailure (show err) >> undefined
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
  subgraph <- assertEither $ subGraphOfFirstCommonAncestor graph (transactionHeadsForGraph graph) transA transB S.empty
  let graphEq graphArg = S.map transactionId (transactionsForGraph graphArg)  
  assertEqual "no graph changes" (graphEq graph) (graphEq subgraph) 
  
-- | Test that a branch anchored at the root transaction is removed when using the first ancestor function.
testSubGraphToFirstAncestorSnipBranch :: Test  
testSubGraphToFirstAncestorSnipBranch = TestCase $ do
  baseGraph <- basicTransactionGraph  
  transA <- assertMaybe (transactionForHead "branchA" baseGraph) "failed to get branchA"
  transB <- assertMaybe (transactionForHead "branchB" baseGraph) "failed to get branchB"
  (_, graph) <- addTransaction "branchC" (createTrans (fakeUUID 12) (TI.singleParent (fakeUUID 1) testTime) (concreteDatabaseContext transA)) baseGraph
  subgraph <- assertEither $ subGraphOfFirstCommonAncestor graph (transactionHeadsForGraph baseGraph) transA transB S.empty
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
  let env = mkDatabaseContextEvalEnv (transactionId branchBTrans) graph
      branchBContext = concreteDatabaseContext branchBTrans
  updatedBranchBContext <- case runDatabaseContextEvalMonad branchBContext env (optimizeAndEvalDatabaseContextExpr True (Assign "branchBOnlyRelvar" (ExistingRelation relationTrue))) of
    Left err -> assertFailure (show err) >> undefined
    Right st -> pure $ dbc_context st
  (_, graph') <- addTransaction "branchB" (createTrans (fakeUUID 3) (TI.singleParent (transactionId branchBTrans) testTime) updatedBranchBContext) graph
  branchBTrans' <- assertMaybe (transactionForHead "branchB" graph') "failed to get branchB head"  
  assertEqual "branchB id 3" (fakeUUID 3) (transactionId branchBTrans')  
  
  -- add another transaction to branchA
  branchATrans <- assertMaybe (transactionForHead "branchA" graph) "failed to get branchA head"  
  (_, graph'') <- addTransaction "branchA" (createTrans (fakeUUID 4) (TI.singleParent (transactionId branchATrans) testTime) (concreteDatabaseContext branchATrans)) graph'
  branchATrans' <- assertMaybe (transactionForHead "branchA" graph'') "failed to get branchA head"
  assertEqual "branchA id 4" (fakeUUID 4) (transactionId branchATrans')
                                              
  --retrieve subgraph                                            
  let subGraphHeads = M.filter (\t -> transactionId t `elem` [fakeUUID 3, fakeUUID 4]) (transactionHeadsForGraph graph'')
  subgraph <- assertEither $ subGraphOfFirstCommonAncestor graph'' subGraphHeads branchATrans' branchBTrans' S.empty
  --verify that the subgraph includes both the heads and the common ancestor
  let expectedTransSet = S.fromList (map fakeUUID [1,3,4])
  assertBool "validate transactions in subgraph" (S.isProperSubsetOf expectedTransSet (S.map transactionId (transactionsForGraph subgraph)))
  
testSelectedBranchMerge :: Test
testSelectedBranchMerge = TestCase $ do
  graph <- basicTransactionGraph
  assertGraph graph
  
  -- add another relvar to branchB
  branchBTrans <- assertMaybe (transactionForHead "branchB" graph) "failed to get branchB head"
  let env = mkDatabaseContextEvalEnv (transactionId branchBTrans) graph
      branchBContext = concreteDatabaseContext branchBTrans  
  updatedBranchBContext <- case runDatabaseContextEvalMonad branchBContext env (optimizeAndEvalDatabaseContextExpr True (Assign "branchBOnlyRelvar" (ExistingRelation relationTrue))) of
    Left err -> assertFailure (show err) >> undefined
    Right st -> pure (dbc_context st)
  (_, graph') <- addTransaction "branchB" (createTrans (fakeUUID 3) (TI.singleParent (transactionId branchBTrans) testTime) updatedBranchBContext) graph
  --create the merge transaction in the graph
  let eGraph' = runGraphRefRelationalExprM gfEnv $ mergeTransactions testTime (fakeUUID 4) (fakeUUID 10) (SelectedBranchMergeStrategy "branchA") ("branchA", "branchB")
      gfEnv = freshGraphRefRelationalExprEnv Nothing graph'
      
  (_, _, graph'') <- assertEither eGraph'

  assertGraph graph''
  --validate that the branchB remains
  branchBTrans' <- assertMaybe (transactionForHead "branchB" graph'') "failed to find branchB head"
  assertEqual "head of merged transaction was removed" (transactionId branchBTrans') (fakeUUID 3)

  --validate that the branchB relvar does *not* appear in the merge because branchA was selected
  mergeTrans <- assertEither (transactionForId (fakeUUID 4) graph'')
  mergedRVs <- assertEither $ resolveDBC' graph'' (concreteDatabaseContext mergeTrans) relationVariables
  assertBool "branchOnlyRelvar is present in merge" (M.notMember "branchBOnlyRelvar" mergedRVs)

-- try various individual component conflicts and check for resolution
testUnionPreferMergeStrategy :: Test
testUnionPreferMergeStrategy = TestCase $ do
  -- create a graph with a relvar conflict
  graph <- basicTransactionGraph
  branchATrans <- assertMaybe (transactionForHead "branchA" graph) "branchATrans"
  branchBTrans <- assertMaybe (transactionForHead "branchB" graph) "branchBTrans"
  branchBRelVar <- assertEither $ mkRelationFromList (attributesFromList [Attribute "conflict" IntAtomType]) []
  branchARVs <- assertEither $ resolveDBC' graph (concreteDatabaseContext branchATrans) relationVariables
  branchBRVs <- assertEither $ resolveDBC' graph (concreteDatabaseContext branchBTrans) relationVariables
  let branchAContext = (concreteDatabaseContext branchATrans) {
        relationVariables = ValueMarker $ M.insert conflictRelVarName branchARelVar branchARVs
        }
      branchARelVar = ExistingRelation relationTrue 
      branchBContext = (concreteDatabaseContext branchBTrans) {
            relationVariables = ValueMarker $  M.insert conflictRelVarName (ExistingRelation branchBRelVar) branchBRVs
            }
      conflictRelVarName = "conflictRelVar" 

  (_, graph') <- addTransaction "branchA" (createTrans (fakeUUID 3) (TI.singleParent (transactionId branchATrans) testTime) branchAContext) graph
  (_, graph'') <- addTransaction "branchB" (createTrans (fakeUUID 4) (TI.singleParent (transactionId branchBTrans) testTime) branchBContext) graph'
  -- validate that the conflict is hidden because we preferred a branch
  let merged = runGraphRefRelationalExprM env $ mergeTransactions testTime (fakeUUID 5) (fakeUUID 3) (UnionPreferMergeStrategy "branchB") ("branchA", "branchB")
      env = freshGraphRefRelationalExprEnv Nothing graph''
  case merged of
    Left err -> assertFailure ("expected merge success: " ++ show err)
    Right (discon, _, _) -> do
      mergedRVs <- assertEither $ resolveDBC' graph'' (Discon.concreteDatabaseContext discon) relationVariables
      let rvExpr = fromMaybe (error "conflictRelVarName") $ M.lookup conflictRelVarName mergedRVs
          reEnv = freshGraphRefRelationalExprEnv (Just (Discon.concreteDatabaseContext discon)) graph
          eRvRel = runGraphRefRelationalExprM reEnv (evalGraphRefRelationalExpr rvExpr)
      assertEqual "branchB relvar preferred in conflict" (Right branchBRelVar) eRvRel
  
-- try various individual component conflicts and check for merge failure
testUnionMergeStrategy :: Test
testUnionMergeStrategy = TestCase $ do
  graph <- basicTransactionGraph
  assertGraph graph
  
  branchBTrans <- assertMaybe (transactionForHead "branchB" graph) "failed to get branchB head"
  branchATrans <- assertMaybe (transactionForHead "branchA" graph) "failed to get branchA head"
  -- add another relvar to branchB - branchBOnlyRelvar should appear in the merge  
  -- add inclusion dependency in branchA

  branchBRVs <- assertEither $ resolveDBC' graph (concreteDatabaseContext branchBTrans) relationVariables
  branchAIncDeps <- assertEither $ resolveDBC' graph (concreteDatabaseContext branchATrans) inclusionDependencies
  let updatedBranchBContext = (concreteDatabaseContext branchBTrans) {
        relationVariables = ValueMarker $ M.insert branchBOnlyRelVarName branchBOnlyRelVar branchBRVs
        }
      updatedBranchAContext = (concreteDatabaseContext branchATrans) {
        inclusionDependencies = ValueMarker $ M.insert branchAOnlyIncDepName branchAOnlyIncDep branchAIncDeps
        }
      branchBOnlyRelVar = ExistingRelation relationTrue
      branchAOnlyIncDepName = "branchAOnlyIncDep"
      branchBOnlyRelVarName = "branchBOnlyRelVar"
      branchAOnlyIncDep = InclusionDependency (ExistingRelation relationTrue) (ExistingRelation relationTrue)
  (_, graph') <- addTransaction "branchB" (createTrans (fakeUUID 3) (TI.singleParent (transactionId branchBTrans) testTime) updatedBranchBContext) graph
  let env = freshGraphRefRelationalExprEnv Nothing graph'
  (discon, _, graph'') <- assertEither $ runGraphRefRelationalExprM env $ mergeTransactions testTime (fakeUUID 5) (fakeUUID 10) UnionMergeStrategy ("branchA", "branchB")
  branchBRVs' <- assertEither $ resolveDBC' graph (Discon.concreteDatabaseContext discon) relationVariables
  let rvExpr = fromMaybe (error "branchOnlyBRelVarName") $ M.lookup branchBOnlyRelVarName branchBRVs'
      rvRel = fromRight (error "rvRel") $ runGraphRefRelationalExprM reEnv (evalGraphRefRelationalExpr rvExpr)
      reEnv = freshGraphRefRelationalExprEnv (Just (Discon.concreteDatabaseContext discon)) graph'' -- graph or graph''?
      
  assertEqual "branchBOnlyRelVar should appear in the merge" relationTrue rvRel
  (_, graph''') <- addTransaction "branchA" (createTrans (fakeUUID 4) (TI.singleParent (transactionId branchATrans) testTime) updatedBranchAContext) graph''
  let eMergeGraph = runGraphRefRelationalExprM gfEnv $ mergeTransactions testTime (fakeUUID 5) (fakeUUID 3) UnionMergeStrategy ("branchA", "branchB")
      gfEnv = freshGraphRefRelationalExprEnv Nothing graph'''
  case eMergeGraph of
    Left err -> assertFailure ("expected merge success: " ++ show err)
    Right (_, _, mergeGraph) -> do
      -- check that the new merge transaction has the correct parents
      mergeTrans <- assertEither $ transactionForId (fakeUUID 5) mergeGraph
      let mergeContext = concreteDatabaseContext mergeTrans
      assertEqual "merge transaction parents" (parentIds mergeTrans) (S.fromList [fakeUUID 3, fakeUUID 4])
      -- check that the new merge tranasction has elements from both A and B branches
      mergeRVs <- assertEither $ resolveDBC' mergeGraph mergeContext relationVariables
      mergeIncDeps <- assertEither $ resolveDBC' mergeGraph mergeContext inclusionDependencies
      let rvExpr' = fromMaybe (error "branchBOnlyRelVarName") $ M.lookup branchBOnlyRelVarName mergeRVs
          rvRel' = fromRight (error "rvRel'") $ runGraphRefRelationalExprM reEnv' (evalGraphRefRelationalExpr rvExpr')
          reEnv' = freshGraphRefRelationalExprEnv (Just mergeContext) graph
      assertEqual "merge transaction relvars" relationTrue rvRel'
      assertEqual "merge transaction incdeps" (Just branchAOnlyIncDep) (M.lookup branchAOnlyIncDepName mergeIncDeps)
      -- test an expected conflict- add branchBOnlyRelVar with same name but different attributes
      conflictRelVar <- assertEither $ mkRelationFromList (attributesFromList [Attribute "conflict" IntAtomType]) []
      updatedBranchARVs <- assertEither $ resolveDBC' mergeGraph updatedBranchAContext relationVariables
      let conflictContextA = updatedBranchAContext {
            relationVariables = ValueMarker $ M.insert branchBOnlyRelVarName (ExistingRelation conflictRelVar) updatedBranchARVs
            }
      conflictBranchATrans <- assertMaybe (transactionForHead "branchA" graph'') "retrieving head transaction for expected conflict"
      (_, graph'''') <- addTransaction "branchA" (createTrans (fakeUUID 6) (TI.singleParent (transactionId conflictBranchATrans) testTime) conflictContextA) graph'''
      let failingMerge = runGraphRefRelationalExprM gfEnv' $ mergeTransactions testTime (fakeUUID 5) (fakeUUID 3) UnionMergeStrategy ("branchA", "branchB")
          gfEnv' = freshGraphRefRelationalExprEnv Nothing graph''''
      case failingMerge of
        Right _ -> assertFailure "expected merge failure"
        Left err -> assertEqual "merge failure" err (MergeTransactionError StrategyWithoutPreferredBranchResolutionMergeError)

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
  (_, graph') <- addTransaction "branchB" (createTrans (fakeUUID 3) (TI.singleParent (transactionId branchBTrans) testTime) branchBContext) graph
                  
  (_, graph'') <- addTransaction "branchA" (createTrans (fakeUUID 4) (TI.singleParent (transactionId branchATrans) testTime) branchAContext) graph'
  
  --check that the union merge fails due to a violated constraint
  let eMerge = runGraphRefRelationalExprM env $ mergeTransactions testTime (fakeUUID 5) (fakeUUID 3) UnionMergeStrategy ("branchA", "branchB")
      env = freshGraphRefRelationalExprEnv Nothing graph''
  case eMerge of
    Left (InclusionDependencyCheckError incDepName' Nothing) -> assertEqual "incdep violation name" incDepName incDepName'
    Left err -> assertFailure ("other error: " ++ show err)
    Right _ -> assertFailure "constraint violation missing"


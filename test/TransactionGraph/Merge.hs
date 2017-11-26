-- tests for transaction merging
import Test.HUnit
import ProjectM36.Base
import ProjectM36.Attribute
import ProjectM36.Relation
import ProjectM36.Transaction
import ProjectM36.TransactionGraph
import ProjectM36.Error
import ProjectM36.Key
import qualified ProjectM36.DisconnectedTransaction as Discon
import qualified ProjectM36.DatabaseContext as DBC
import ProjectM36.RelationalExpression

import qualified Data.ByteString.Lazy as BS
import System.Exit
import Data.Word
import qualified Data.UUID as U
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.Time.Clock
import Data.Time.Calendar

import Control.Monad.State

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

createTrans :: TransactionId -> TransactionInfo -> DatabaseContext -> Transaction
createTrans tid info ctx = Transaction tid info (Schemas ctx M.empty)

basicTransactionGraph :: IO TransactionGraph
basicTransactionGraph = do
  let bsGraph = bootstrapTransactionGraph testTime uuidRoot DBC.basicDatabaseContext
      rootTrans = fromMaybe (error "bonk") (transactionForHead "master" bsGraph)
      uuidA = fakeUUID 10
      uuidB = fakeUUID 11
      uuidRoot = fakeUUID 1
      rootContext = concreteDatabaseContext rootTrans
  (_, bsGraph') <- addTransaction "branchA" (createTrans uuidA (TransactionInfo uuidRoot S.empty testTime) rootContext) bsGraph
  (_, bsGraph'') <- addTransaction "branchB" (createTrans uuidB (TransactionInfo uuidRoot S.empty testTime) rootContext) bsGraph'
  pure bsGraph''
  
addTransaction :: HeadName -> Transaction -> TransactionGraph -> IO (Transaction, TransactionGraph)
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
  assertEqual "no graph changes" (graphEq subgraph) (graphEq graph)
  
-- | Test that a branch anchored at the root transaction is removed when using the first ancestor function.
testSubGraphToFirstAncestorSnipBranch :: Test  
testSubGraphToFirstAncestorSnipBranch = TestCase $ do
  baseGraph <- basicTransactionGraph  
  transA <- assertMaybe (transactionForHead "branchA" baseGraph) "failed to get branchA"
  transB <- assertMaybe (transactionForHead "branchB" baseGraph) "failed to get branchB"
  (_, graph) <- addTransaction "branchC" (createTrans (fakeUUID 12) (TransactionInfo (fakeUUID 1) S.empty testTime) (concreteDatabaseContext transA)) baseGraph
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
  (updatedBranchBContext,_,_) <- case runState (evalDatabaseContextExpr (Assign "branchBOnlyRelvar" (ExistingRelation relationTrue))) (freshDatabaseState (concreteDatabaseContext branchBTrans)) of
    (Left err, _) -> assertFailure (show err) >> undefined
    (Right (), context) -> pure context
  (_, graph') <- addTransaction "branchB" (createTrans (fakeUUID 3) (TransactionInfo (transactionId branchBTrans) S.empty testTime) updatedBranchBContext) graph
  branchBTrans' <- assertMaybe (transactionForHead "branchB" graph') "failed to get branchB head"  
  assertEqual "branchB id 3" (fakeUUID 3) (transactionId branchBTrans')  
  
  -- add another transaction to branchA
  branchATrans <- assertMaybe (transactionForHead "branchA" graph) "failed to get branchA head"  
  (_, graph'') <- addTransaction "branchA" (createTrans (fakeUUID 4) (TransactionInfo (transactionId branchATrans) S.empty testTime) (concreteDatabaseContext branchATrans)) graph'
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
  (updatedBranchBContext,_,_) <- case runState (evalDatabaseContextExpr (Assign "branchBOnlyRelvar" (ExistingRelation relationTrue))) (freshDatabaseState (concreteDatabaseContext branchBTrans)) of
    (Left err, _) -> assertFailure (show err) >> undefined
    (Right (), context) -> pure context
  (_, graph') <- addTransaction "branchB" (createTrans (fakeUUID 3) (TransactionInfo (transactionId branchBTrans) S.empty testTime) updatedBranchBContext) graph
  --create the merge transaction in the graph
  let eGraph' = mergeTransactions testTime (fakeUUID 4) (fakeUUID 10) (SelectedBranchMergeStrategy "branchA") ("branchA", "branchB") graph'
      
  (_, graph'') <- assertEither eGraph'

  assertGraph graph''
  --validate that the branchB remains
  branchBTrans' <- assertMaybe (transactionForHead "branchB" graph'') "failed to find branchB head"
  assertEqual "head of merged transaction was removed" (transactionId branchBTrans') (fakeUUID 3)

  --validate that the branchB relvar does *not* appear in the merge because branchA was selected
  mergeTrans <- assertEither (transactionForId (fakeUUID 4) graph'')
  assertBool "branchOnlyRelvar is present in merge" (M.notMember "branchBOnlyRelvar" (relationVariables (concreteDatabaseContext mergeTrans)))

-- try various individual component conflicts and check for resolution
testUnionPreferMergeStrategy :: Test
testUnionPreferMergeStrategy = TestCase $ do
  -- create a graph with a relvar conflict
  graph <- basicTransactionGraph
  branchATrans <- assertMaybe (transactionForHead "branchA" graph) "branchATrans"
  branchBTrans <- assertMaybe (transactionForHead "branchB" graph) "branchBTrans"
  branchBRelVar <- assertEither $ mkRelationFromList (attributesFromList [Attribute "conflict" IntAtomType]) []  
  let branchAContext = (concreteDatabaseContext branchATrans) {relationVariables = M.insert conflictRelVarName branchARelVar (relationVariables (concreteDatabaseContext branchATrans))}
      branchARelVar = relationTrue
      branchBContext = (concreteDatabaseContext branchBTrans) {relationVariables = M.insert conflictRelVarName branchBRelVar (relationVariables (concreteDatabaseContext branchBTrans))}
      conflictRelVarName = "conflictRelVar"

  (_, graph') <- addTransaction "branchA" (createTrans (fakeUUID 3) (TransactionInfo (transactionId branchATrans) S.empty testTime) branchAContext) graph
  (_, graph'') <- addTransaction "branchB" (createTrans (fakeUUID 4) (TransactionInfo (transactionId branchBTrans) S.empty testTime) branchBContext) graph'
  -- validate that the conflict is hidden because we preferred a branch
  let merged = mergeTransactions testTime (fakeUUID 5) (fakeUUID 3) (UnionPreferMergeStrategy "branchB") ("branchA", "branchB") graph''
  case merged of
    Left err -> assertFailure ("expected merge success: " ++ show err)
    Right (discon, _) ->
      assertEqual "branchB relvar preferred in conflict" (Just branchBRelVar) (M.lookup conflictRelVarName (relationVariables (Discon.concreteDatabaseContext discon)))
  
-- try various individual component conflicts and check for merge failure
testUnionMergeStrategy :: Test
testUnionMergeStrategy = TestCase $ do
  graph <- basicTransactionGraph
  assertGraph graph
  
  branchBTrans <- assertMaybe (transactionForHead "branchB" graph) "failed to get branchB head"
  branchATrans <- assertMaybe (transactionForHead "branchA" graph) "failed to get branchA head"
  -- add another relvar to branchB - branchBOnlyRelvar should appear in the merge  
  -- add inclusion dependency in branchA

  let updatedBranchBContext = (concreteDatabaseContext branchBTrans) {relationVariables = M.insert branchBOnlyRelVarName branchBOnlyRelVar (relationVariables (concreteDatabaseContext branchBTrans)) }
      updatedBranchAContext = (concreteDatabaseContext branchATrans) {inclusionDependencies = M.insert branchAOnlyIncDepName branchAOnlyIncDep (inclusionDependencies (concreteDatabaseContext branchATrans)) }
      branchBOnlyRelVar = relationTrue
      branchAOnlyIncDepName = "branchAOnlyIncDep"
      branchBOnlyRelVarName = "branchBOnlyRelVar"
      branchAOnlyIncDep = InclusionDependency (ExistingRelation relationTrue) (ExistingRelation relationTrue)
  (_, graph') <- addTransaction "branchB" (createTrans (fakeUUID 3) (TransactionInfo (transactionId branchBTrans) S.empty testTime) updatedBranchBContext) graph
  (discon, _) <- assertEither $ mergeTransactions testTime (fakeUUID 5) (fakeUUID 10) UnionMergeStrategy ("branchA", "branchB") graph'
  assertEqual "branchBOnlyRelVar should appear in the merge" (M.lookup branchBOnlyRelVarName (relationVariables (Discon.concreteDatabaseContext discon))) (Just relationTrue)
  (_, graph'') <- addTransaction "branchA" (createTrans (fakeUUID 4) (TransactionInfo (transactionId branchATrans) S.empty testTime) updatedBranchAContext) graph'
  let eMergeGraph = mergeTransactions testTime (fakeUUID 5) (fakeUUID 3) UnionMergeStrategy ("branchA", "branchB") graph''
  case eMergeGraph of
    Left err -> assertFailure ("expected merge success: " ++ show err)
    Right (_, mergeGraph) -> do
      -- check that the new merge transaction has the correct parents
      mergeTrans <- assertEither $ transactionForId (fakeUUID 5) mergeGraph
      let mergeContext = concreteDatabaseContext mergeTrans
      assertEqual "merge transaction parents" (transactionParentIds mergeTrans) (S.fromList [fakeUUID 3, fakeUUID 4])
      -- check that the new merge tranasction has elements from both A and B branches
      assertEqual "merge transaction relvars" (Just relationTrue) (M.lookup branchBOnlyRelVarName (relationVariables mergeContext))
      assertEqual "merge transaction incdeps" (Just branchAOnlyIncDep) (M.lookup branchAOnlyIncDepName (inclusionDependencies mergeContext)) 
      -- test an expected conflict- add branchBOnlyRelVar with same name but different attributes
      conflictRelVar <- assertEither $ mkRelationFromList (attributesFromList [Attribute "conflict" IntAtomType]) []
      let conflictContextA = updatedBranchAContext {relationVariables = M.insert branchBOnlyRelVarName conflictRelVar (relationVariables updatedBranchAContext) }
      conflictBranchATrans <- assertMaybe (transactionForHead "branchA" graph'') "retrieving head transaction for expected conflict"
      (_, graph''') <- addTransaction "branchA" (createTrans (fakeUUID 6) (TransactionInfo (transactionId conflictBranchATrans) S.empty testTime) conflictContextA) graph''
      let failingMerge = mergeTransactions testTime (fakeUUID 5) (fakeUUID 3) UnionMergeStrategy ("branchA", "branchB") graph'''
      case failingMerge of
        Right _ -> assertFailure "expected merge failure"
        Left err -> assertEqual "merge failure" err (MergeTransactionError StrategyViolatesRelationVariableMergeError)

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
      Right branchArv = eRel 2
      Right branchBrv = eRel 3
      branchAContext = (concreteDatabaseContext branchBTrans) {relationVariables = M.singleton rvName branchArv}
      branchBContext = (concreteDatabaseContext branchBTrans) {relationVariables = M.singleton rvName branchBrv, 
                                                               inclusionDependencies = M.singleton incDepName incDep}      
      incDepName = "x_key"
      incDep = inclusionDependencyForKey (AttributeNames (S.singleton "x")) (RelationVariable "x" ())


   --add the rv in new commits to both branches
  (_, graph') <- addTransaction "branchB" (createTrans (fakeUUID 3) (TransactionInfo (transactionId branchBTrans) S.empty testTime) branchBContext) graph
                  
  (_, graph'') <- addTransaction "branchA" (createTrans (fakeUUID 4) (TransactionInfo (transactionId branchATrans) S.empty testTime) branchAContext) graph'
  
  --check that the union merge fails due to a violated constraint
  let eMerge = mergeTransactions testTime (fakeUUID 5) (fakeUUID 3) UnionMergeStrategy ("branchA", "branchB") graph''
  case eMerge of
    Left (InclusionDependencyCheckError incDepName') -> assertEqual "incdep violation name" incDepName incDepName'
    Left err -> assertFailure ("other error: " ++ show err)
    Right _ -> assertFailure "constraint violation missing"


-- tests for transaction merging
{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import ProjectM36.Base
import ProjectM36.TransactionGraph
import ProjectM36.TransactionGraph.Show
import ProjectM36.DateExamples
import ProjectM36.Error
import qualified Data.ByteString.Lazy as BS
import System.Exit
import Data.Word
import ProjectM36.RelationalExpression
import qualified Data.UUID as U
import qualified ProjectM36.DatabaseContext as DBC
import Control.Monad.State hiding (join)
import ProjectM36.Relation (relationTrue)
import Debug.Trace

main :: IO ()           
main = do 
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testList :: Test
testList = TestList [
                     testSubGraphToFirstAncestorBasic,
                     testSubGraphToFirstAncestorSnipBranch,
                     testSelectedBranchMerge
                    ]

-- | Create a transaction graph with two branches and no changes between them.
{-
root 1
|--- branchA aaaa 
|--- branchB bbbb
-}
basicTransactionGraph :: IO TransactionGraph
basicTransactionGraph = do
  let bsGraph = bootstrapTransactionGraph uuidRoot DBC.empty --dateExamples
      rootTrans = case transactionForHead "master" bsGraph of
        Just trans -> trans
        Nothing -> error "bonk"
      uuidA = fakeUUID 10
      uuidB = fakeUUID 11
      uuidRoot = fakeUUID 1
      rootContext = transactionContext rootTrans
  (_, bsGraph') <- addTransaction "branchA" uuidA rootContext rootTrans bsGraph
  (_, bsGraph'') <- addTransaction "branchB" uuidB rootContext rootTrans bsGraph'
  pure bsGraph''
  
addTransaction :: HeadName -> U.UUID -> DatabaseContext -> Transaction -> TransactionGraph -> IO (Transaction, TransactionGraph)
addTransaction headName newTransUUID context parentTrans graph = case addTransactionToGraph headName parentTrans newTransUUID context graph of
  Left err -> assertFailure (show err) >> error ""
  Right (t,g) -> pure (t,g)
              
fakeUUID :: Word8 -> U.UUID
fakeUUID x = case U.fromByteString (BS.concat (replicate 4 w32)) of
  Nothing -> error "impossible uuid"
  Just u -> u
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
  subgraph <- assertEither $ subGraphToFirstCommonAncestor graph (transactionHeadsForGraph graph) emptyTransactionGraph transA transB
  assertEqual "no graph changes" subgraph graph
  
-- | Test that a branch anchored at the root transaction is removed when using the first ancestor function.
testSubGraphToFirstAncestorSnipBranch :: Test  
testSubGraphToFirstAncestorSnipBranch = TestCase $ do
  baseGraph <- basicTransactionGraph  
  transA <- assertMaybe (transactionForHead "branchA" baseGraph) "failed to get branchA"
  transB <- assertMaybe (transactionForHead "branchB" baseGraph) "failed to get branchB"
  rootTrans <- assertEither (transactionForUUID (fakeUUID 1) baseGraph)
  (_, graph) <- addTransaction "branchC" (fakeUUID 12) (transactionContext transA) rootTrans baseGraph
  subgraph <- assertEither $ subGraphToFirstCommonAncestor graph (transactionHeadsForGraph baseGraph) emptyTransactionGraph transA transB
  assertGraph subgraph
  assertEqual "failed to snip branch" baseGraph subgraph
  
testSelectedBranchMerge :: Test
testSelectedBranchMerge = TestCase $ do
  graph <- basicTransactionGraph
  assertGraph graph
  
  -- add another relvar to branchB
  branchBTrans <- assertMaybe (transactionForHead "branchB" graph) "failed to get branchB head"
  updatedBranchBContext <- case runState (evalContextExpr (Assign "branchBOnlyRelvar" (ExistingRelation relationTrue))) (transactionContext branchBTrans) of
    (Just err, _) -> assertFailure (show err) >> undefined
    (Nothing, context) -> pure context
  (_, graph') <- addTransaction "branchB" (fakeUUID 3) updatedBranchBContext branchBTrans graph
  --create the merge transaction in the graph
  let eGraph' = mergeTransactions (fakeUUID 4) (fakeUUID 10) (SelectedBranchMergeStrategy "branchA") ("branchA", "branchB") graph'
  putStrLn "SPAMMO"
      
  (DisconnectedTransaction _ mergedContext, graph'') <- assertEither eGraph'
  assertGraph graph''
  --validate that the branchB was removed
  rootTrans <- assertEither $ transactionForUUID (fakeUUID 1) graph''
  assertEqual "head still available" (transactionForHead "branchB" graph'') Nothing
  --assertEqual "bad merge" mergedContext (transactionContext rootTrans)
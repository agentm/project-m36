import ProjectM36.Client
import ProjectM36.DataFrame
import TutorialD.Interpreter.TestBase

import Test.HUnit
import System.Exit
import qualified Data.Set as S

testList :: Test
testList = TestList [testOrderBy, testLimit, testOffset]

main :: IO ()
main = do 
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess  

testOrderBy :: Test
testOrderBy = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  Right df <- executeDataFrameExpr sessionId dbconn (
    DataFrameExpr {
        convertExpr = Project (AttributeNames (S.singleton "status")) (RelationVariable "s" ()),
        orderExprs = [AttributeOrderExpr "status" AscendingOrder],
        offset = Nothing,
        limit = Nothing
        })
  let vals = map (\tup -> case atomForAttributeName "status" tup of
                     Left err -> error (show err)
                     Right atom -> atom) (tuples df)
  assertEqual "sort order of s" [IntegerAtom 10, IntegerAtom 20, IntegerAtom 30] vals
  
testOffset :: Test
testOffset = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  Right df <- executeDataFrameExpr sessionId dbconn (
    DataFrameExpr {
        convertExpr = Project (AttributeNames (S.singleton "status")) (RelationVariable "s" ()),
        orderExprs = [AttributeOrderExpr "status" AscendingOrder],
        offset = Just 1,
        limit = Nothing
        })
  let vals = map (\tup -> case atomForAttributeName "status" tup of
                     Left err -> error (show err)
                     Right atom -> atom) (tuples df)
  assertEqual "sort + offset" [IntegerAtom 20, IntegerAtom 30] vals

testLimit :: Test
testLimit = TestCase $ do
  (sessionId, dbconn) <- dateExamplesConnection emptyNotificationCallback
  Right df <- executeDataFrameExpr sessionId dbconn (
    DataFrameExpr {
        convertExpr = Project (AttributeNames (S.singleton "status")) (RelationVariable "s" ()),
        orderExprs = [AttributeOrderExpr "status" DescendingOrder],
        offset = Nothing,
        limit = Just 1
        })
  let vals = map (\tup -> case atomForAttributeName "status" tup of
                     Left err -> error (show err)
                     Right atom -> atom) (tuples df)
  assertEqual "sort + limit" [IntegerAtom 30] vals
  

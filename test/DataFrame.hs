import ProjectM36.Client
import ProjectM36.DataFrame (atomForAttributeName, tuples)
import TutorialD.Interpreter.TestBase

import Test.HUnit
import System.Exit
import qualified Data.Set as S

testList :: Test
testList = TestList []

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
  
--testOffset :: Test

--testLimit :: Test

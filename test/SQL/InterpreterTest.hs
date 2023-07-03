{-# LANGUAGE OverloadedStrings #-}
import SQL.Interpreter.Select
import SQL.Interpreter.Convert
import TutorialD.Interpreter.RelationalExpr
import System.Exit
import Test.HUnit
import Text.Megaparsec
import qualified Data.Text as T

main :: IO ()
main = do
  tcounts <- runTestTT (TestList tests)
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess
  where
    tests = [testSelect]

testSelect :: Test
testSelect = TestCase $ do
  -- check that SQL and tutd compile to same thing
  let p tin = parse selectP "test" tin
      readTests = [("SELECT * FROM test", "test"),
                  ("SELECT a FROM test", "test{a}"),
                  ("SELECT a FROM test where b=3","(test where b=3){a}"),
                  ("SELECT a,b FROM test where b>3","(test where gt(@b,3)){a,b}"),
                  ("SELECT a,b,10 FROM test","test{a,b}:{attr_3:=10}"),
                  ("SELECT a AS x FROM test","(test rename {a as x}){x}"),
                  ("sElECt A aS X FRoM TeST","(test rename {a as x}){x}"),
                  ("SELECT sup.city FROM s AS sup","with (sup as s) ((sup rename {city as `sup.city`}){`sup.city`})"),
                  ("SELECT sup.city,sup.sname FROM s AS sup","with (sup as s) ((sup rename {city as `sup.city`,sname as `sup.sname`}){`sup.city`,`sup.sname`})"),
                  ("SELECT sup.* FROM s as sup","with (sup as s) (sup{all from sup})")
                  ]
      check (sql, tutd) = do
        --parse SQL
        select <- case parse (selectP <* eof) "test" sql of
          Left err -> error (errorBundlePretty err)
          Right x -> print x >> pure x
        --parse tutd
        relExpr <- case parse (relExprP <* eof) "test" tutd of
          Left err -> error (errorBundlePretty err)
          Right x -> pure x
        selectAsRelExpr <- case convert select of
          Left err -> error (show err)
          Right x -> pure x
        print ("selectAsRelExpr", selectAsRelExpr)
        assertEqual (T.unpack sql) relExpr selectAsRelExpr 
  mapM_ check readTests
  
  assertEqual "SELECT * FROM test"  (Right (Select {distinctness = Nothing, projectionClause = [(Identifier (QualifiedProjectionName [Asterisk]),Nothing)], tableExpr = Just (TableExpr {fromClause = [SimpleTableRef (QualifiedName ["test"])], whereClause = Nothing, groupByClause = [], havingClause = Nothing, orderByClause = [], limitClause = Nothing, offsetClause = Nothing})})) (p "SELECT * FROM test")


{-# LANGUAGE OverloadedStrings #-}
import SQL.Interpreter.Select
import SQL.Interpreter.Convert
import TutorialD.Interpreter.RelationalExpr
import ProjectM36.RelationalExpression
import ProjectM36.TransactionGraph
import ProjectM36.DateExamples
import ProjectM36.NormalizeExpr
import ProjectM36.Base
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
  (tgraph,transId) <- freshTransactionGraph dateExamples  
  let p tin = parse selectP "test" tin
      readTests = [{-("SELECT * FROM test", "test"),
                  ("SELECT a FROM test", "test{a}"),
                  ("SELECT a FROM test where b=3","(test where b=3){a}"),
                  ("SELECT a,b FROM test where b>3","(test where gt(@b,3)){a,b}"),
                  ("SELECT a,b,10 FROM test","test{a,b}:{attr_3:=10}"),
                  ("SELECT a AS x FROM test","(test rename {a as x}){x}"),
                  ("sElECt A aS X FRoM TeST","(test rename {a as x}){x}"),
                  ("SELECT sup.city FROM s AS sup","with (sup as s) ((sup rename {city as `sup.city`}){`sup.city`})"),
                  ("SELECT sup.city,sup.sname FROM s AS sup","with (sup as s) ((sup rename {city as `sup.city`,sname as `sup.sname`}){`sup.city`,`sup.sname`})"),
                  ("SELECT sup.* FROM s as sup","with (sup as s) (sup{all from sup})"),
                  ("SELECT * FROM s NATURAL JOIN sp","s join sp"),
                  ("SELECT * FROM s CROSS JOIN sp", "(s rename {s# as s#_a1}) join sp"),
                  ("SELECT * FROM sp INNER JOIN sp USING (\"s#\")",
                   "(sp rename {p# as p#_a1, qty as qty_a1}) join sp"),-}
                    ("SELECT * FROM sp JOIN s ON s.s# = sp.s#","sp join s")
                  ]
      gfEnv = GraphRefRelationalExprEnv {
        gre_context = Just dateExamples,
        gre_graph = tgraph,
        gre_extra = mempty }
      typeF expr = do
        let gfExpr = runProcessExprM (TransactionMarker transId) (processRelationalExpr expr)
        runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr gfExpr)
      check (sql, tutd) = do
        --parse SQL
        select <- case parse (selectP <* eof) "test" sql of
          Left err -> error (errorBundlePretty err)
          Right x -> print x >> pure x
        --parse tutd
        relExpr <- case parse (relExprP <* eof) "test" tutd of
          Left err -> error (errorBundlePretty err)
          Right x -> pure x
        selectAsRelExpr <- case convert typeF select of
          Left err -> error (show err)
          Right x -> pure x

        print ("selectAsRelExpr"::String, selectAsRelExpr)
        assertEqual (T.unpack sql) relExpr selectAsRelExpr 
  mapM_ check readTests
  
  assertEqual "SELECT * FROM test"  (Right (Select {distinctness = Nothing, projectionClause = [(Identifier (QualifiedProjectionName [Asterisk]),Nothing)], tableExpr = Just (TableExpr {fromClause = [SimpleTableRef (QualifiedName ["test"])], whereClause = Nothing, groupByClause = [], havingClause = Nothing, orderByClause = [], limitClause = Nothing, offsetClause = Nothing})})) (p "SELECT * FROM test")


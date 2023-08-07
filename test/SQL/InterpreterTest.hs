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
      readTests = [
        -- simple relvar
        ("SELECT * FROM test", "test"),
        -- simple projection
        ("SELECT a FROM test", "test{a}"),
        -- restriction
        ("SELECT a FROM test where b=3","(test where b=3){a}"),
        -- restriction
        ("SELECT a,b FROM test where b>3","(test where gt(@b,3)){a,b}"),
        -- extension mixed with projection
        ("SELECT a,b,10 FROM test","(test:{attr_3:=10}){a,b,attr_3}"),
        -- column alias
        ("SELECT a AS x FROM test","(test rename {a as x}){x}"),
        -- case insensitivity
        ("sElECt A aS X FRoM TeST","(test rename {a as x}){x}"),
        --column from aliased table
        ("SELECT sup.city FROM s AS sup","with (sup as s) ((sup rename {city as `sup.city`}){`sup.city`})"),
        --projection with alias
        ("SELECT sup.city,sup.sname FROM s AS sup","with (sup as s) ((sup rename {city as `sup.city`,sname as `sup.sname`}){`sup.city`,`sup.sname`})"),
        ("SELECT sup.* FROM s as sup","with (sup as s) (sup{all from sup})"),
        -- natural join
        ("SELECT * FROM s NATURAL JOIN sp","s join sp"),
        -- cross join
        ("SELECT * FROM s CROSS JOIN sp", "(s rename {s# as `s.s#`}) join sp"),
        -- unaliased join using
        ("SELECT * FROM sp INNER JOIN sp USING (\"s#\")",
         "(sp rename {p# as `sp.p#`, qty as `sp.qty`}) join sp"),
        -- unaliased join
        ("SELECT * FROM sp JOIN s ON s.s# = sp.s#","((((s rename {s# as `s.s#`,sname as `s.sname`,city as `s.city`,status as `s.status`}) join (sp rename {s# as `sp.s#`,p# as `sp.p#`,qty as `sp.qty`})):{join_1:=eq(@`s.s#`,@`sp.s#`)}) where join_1=True) {all but join_1}"),
        -- aliased join on
        ("SELECT * FROM sp AS sp2 JOIN s AS s2 ON s2.s# = sp2.s#",
         "with (s2 as s, sp2 as sp) ((((s2 rename {s# as `s2.s#`,sname as `s2.sname`,city as `s2.city`,status as `s2.status`}) join (sp2 rename {s# as `sp2.s#`,p# as `sp2.p#`,qty as `sp2.qty`})):{join_1:=eq(@`s2.s#`,@`sp2.s#`)}) where join_1=True) {all but join_1}"),
        -- formula extension
          ("SELECT status+10 FROM s", "(s : {attr_1:=add(@status,10)}) { attr_1 }")
                  ]
      gfEnv = GraphRefRelationalExprEnv {
        gre_context = Just dateExamples,
        gre_graph = tgraph,
        gre_extra = mempty }
      typeF expr = do
        let gfExpr = runProcessExprM (TransactionMarker transId) (processRelationalExpr expr)
        runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr gfExpr)
      check (sql, tutd) = do
        print sql
        --parse SQL
        select <- case parse (selectP <* eof) "test" sql of
          Left err -> error (errorBundlePretty err)
          Right x -> do
            --print x
            pure x
        --parse tutd
        relExpr <- case parse (relExprP <* eof) "test" tutd of
          Left err -> error (errorBundlePretty err)
          Right x -> pure x
        selectAsRelExpr <- case convert typeF select of
          Left err -> error (show err)
          Right x -> pure x

        --print ("selectAsRelExpr"::String, selectAsRelExpr)
        assertEqual (T.unpack sql) relExpr selectAsRelExpr 
  mapM_ check readTests
  
  assertEqual "SELECT * FROM test"  (Right (Select {distinctness = Nothing, projectionClause = [(Identifier (QualifiedProjectionName [Asterisk]),Nothing)], tableExpr = Just (TableExpr {fromClause = [SimpleTableRef (QualifiedName ["test"])], whereClause = Nothing, groupByClause = [], havingClause = Nothing, orderByClause = [], limitClause = Nothing, offsetClause = Nothing})})) (p "SELECT * FROM test")


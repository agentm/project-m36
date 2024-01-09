{-# LANGUAGE OverloadedStrings #-}
import SQL.Interpreter.Select
import SQL.Interpreter.Convert
--import TutorialD.Interpreter.RelationalExpr
import TutorialD.Interpreter.RODatabaseContextOperator
import TutorialD.Printer
import Prettyprinter
import ProjectM36.RelationalExpression
import ProjectM36.TransactionGraph
import ProjectM36.DateExamples
import ProjectM36.DatabaseContext
import ProjectM36.NormalizeExpr
import ProjectM36.Client
import ProjectM36.Base
import System.Exit
import Test.HUnit
import Text.Megaparsec
import qualified Data.Text as T
import qualified Data.Map as M

main :: IO ()
main = do
  tcounts <- runTestTT (TestList tests)
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess
  where
    tests = [testFindColumn, testSelect]


testFindColumn :: Test
testFindColumn = TestCase $ do
  let tctx = TableContext $ M.fromList [(TableAlias "s",
                                         (RelationVariable "s" (),
                                          attributesFromList [Attribute "city" TextAtomType, Attribute "status" IntegerAtomType],
                                          mempty
                                          )
                                        )]
  let findCol colName =
        case runConvertM tctx (findColumn colName) of
          Left err -> error (show err)
          Right val -> fst val
  assertEqual "findColumn city" [TableAlias "s"] (findCol (ColumnName ["city"]))
  assertEqual "findColumn s.city" [TableAlias "s"] (findCol (ColumnName ["s", "city"]))

testSelect :: Test
testSelect = TestCase $ do
  -- check that SQL and tutd compile to same thing
  (tgraph,transId) <- freshTransactionGraph dateExamples
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  
  let readTests = [{-
        -- simple relvar
        ("SELECT * FROM s", "(s)"),
        -- simple projection
        ("SELECT city FROM s", "(s{city})"),
        -- restriction
        ("SELECT city FROM s where status=20","((s where status=20){city})"),
        -- restriction with asterisk and qualified name
        ("SELECT * FROM s WHERE \"s\".\"status\"=20","(s where status=20)"),
        -- join via where clause-}
        ("SELECT city FROM s, sp where \"s\".\"s#\" = \"sp\".\"s#\"",
         "((((s rename {s# as `s.s#`}) join sp) where `s.s#` = @s#){city})"
         ){-,
        -- restriction
        ("SELECT status,city FROM s where status>20","((s where gt(@status,20)){status,city})"),
        -- extension mixed with projection
        ("SELECT city,status,10 FROM s","((s:{attr_3:=10}){city,status,attr_3})"),
        -- column alias
        ("SELECT city AS x FROM s","((s rename {city as x}){x})"),
        -- case insensitivity
        ("sElECt CitY aS X FRoM s","((s rename {city as x}){x})"),
        --column from aliased table
        ("SELECT sup.city FROM s AS sup","(with (sup as s) ((sup rename {city as `sup.city`}){`sup.city`}))"),
        --projection with alias
        ("SELECT sup.city,sup.sname FROM s AS sup","(with (sup as s) ((sup rename {city as `sup.city`,sname as `sup.sname`}){`sup.city`,`sup.sname`}))"),
        ("SELECT sup.* FROM s as sup","(with (sup as s) (sup{all from sup}))"),
        -- natural join
        ("SELECT * FROM s NATURAL JOIN sp","(s join sp)"),
        -- cross join
        ("SELECT * FROM s CROSS JOIN sp", "((s rename {s# as `s.s#`}) join sp)"),
        -- unaliased join using
        ("SELECT * FROM sp INNER JOIN sp AS sp2 USING (\"s#\")",
         "(with (sp2 as sp) ((sp rename {p# as `sp.p#`, qty as `sp.qty`}) join (sp2 rename {p# as `sp2.p#`, qty as `sp2.qty`})))"),
        -- unaliased join
        ("SELECT * FROM sp JOIN s ON s.s# = sp.s#","(((((s rename {s# as `s.s#`,sname as `s.sname`,city as `s.city`,status as `s.status`}) join (sp rename {s# as `sp.s#`,p# as `sp.p#`,qty as `sp.qty`})):{join_1:=eq(@`s.s#`,@`sp.s#`)}) where join_1=True) {all but join_1})"),
        -- aliased join on
        ("SELECT * FROM sp AS sp2 JOIN s AS s2 ON s2.s# = sp2.s#",
         "(with (s2 as s, sp2 as sp) ((((s2 rename {s# as `s2.s#`,sname as `s2.sname`,city as `s2.city`,status as `s2.status`}) join (sp2 rename {s# as `sp2.s#`,p# as `sp2.p#`,qty as `sp2.qty`})):{join_1:=eq(@`s2.s#`,@`sp2.s#`)}) where join_1=True) {all but join_1})"),
        -- formula extension
        ("SELECT status+10 FROM s", "((s : {attr_1:=add(@status,10)}) { attr_1 })"),
        -- extension and formula
        ("SELECT status+10,city FROM s", "((s : {attr_1:=add(@status,10)}) {city,attr_1})"),
        -- complex join condition
        ("SELECT * FROM sp JOIN s ON s.s# = sp.s# AND s.s# = sp.s#",
         "(((((s rename {s# as `s.s#`,sname as `s.sname`,city as `s.city`,status as `s.status`}) join (sp rename {s# as `sp.s#`,p# as `sp.p#`,qty as `sp.qty`})):{join_1:=and(eq(@`s.s#`,@`sp.s#`),eq(@`s.s#`,@`sp.s#`))}) where join_1=True) {all but join_1})"),
        -- TABLE <tablename>
        ("TABLE s", "(s)"),
        -- any, all, some
        -- IN()
        ("SELECT * FROM s WHERE s# IN ('S1','S2')", "(s where eq(@s#,\"S1\") or eq(@s#,\"S2\"))"),
        -- NOT IN()
        ("SELECT * FROM s WHERE s# NOT IN ('S1','S2')",
         "(s where not (eq(@s#,\"S1\") or eq(@s#,\"S2\")))"),
        -- where not exists
        -- group by
        -- group by having
        -- limit
        -- case when
        -- union
        -- intersect
        -- except
        ("SELECT * FROM s LIMIT 10","(s) limit 10"),
        -- offset
        ("SELECT * FROM s OFFSET 10","(s) offset 10"),
        -- limit offset
        ("SELECT * FROM s LIMIT 10 OFFSET 20","(s) limit 10 offset 20"),
        -- order by
        ("SELECT * FROM s ORDER BY status","(s) orderby {status}"),
        -- order by descending
        ("SELECT * FROM s ORDER BY status DESC,city","(s) orderby {status descending,city}"),
        -- CTEs
        ("WITH x AS (SELECT * FROM s) SELECT * FROM x", "(with (x as s) x)"),
        -- SELECT with no table expression
        ("SELECT 1,2,3","((relation{}{}:{attr_1:=1,attr_2:=2,attr_3:=3}){attr_1,attr_2,attr_3})"),
        -- basic NULL
--        ("SELECT NULL", "((relation{}{}:{attr_1:=Nothing}){attr_1})"),
        -- where exists
        -- complication: we need to add attribute renamers due to the subselect
        ("SELECT * FROM s WHERE EXISTS (SELECT * FROM sp WHERE \"s\".\"s#\"=\"sp\".\"s#\")",
         "(s where (((sp rename {s# as `sp.s#`}) where `s#`= @`sp.s#`){}))")-}
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
        select <- case parse (queryExprP <* eof) "test" sql of
          Left err -> error (errorBundlePretty err)
          Right x -> do
            --print x
            pure x
        --parse tutd
        tutdAsDFExpr <- case parse (dataFrameP <* eof) "test" tutd of
          Left err -> error (errorBundlePretty err)
          Right x -> do
            --print x
            pure x
        selectAsDFExpr <- case evalConvertM mempty (convertSelect typeF select) of
          Left err -> error (show err)
          Right x -> do
            print x
            pure x

        --print ("selectAsRelExpr"::String, selectAsRelExpr)
        print ("expected: ", pretty tutdAsDFExpr)
        print ("actual: ", pretty selectAsDFExpr)
        assertEqual (T.unpack sql) tutdAsDFExpr selectAsDFExpr
        --check that the expression can actually be executed
        eEvald <- executeDataFrameExpr sess conn tutdAsDFExpr
        case eEvald of
          Left err -> assertFailure (show err <> ": " <> show tutdAsDFExpr)
          Right _ -> pure ()
  mapM_ check readTests
  
--  assertEqual "SELECT * FROM test"  (Right (Select {distinctness = Nothing, projectionClause = [(Identifier (QualifiedProjectionName [Asterisk]),Nothing)], tableExpr = Just (TableExpr {fromClause = [SimpleTableRef (QualifiedName ["test"])], whereClause = Nothing, groupByClause = [], havingClause = Nothing, orderByClause = [], limitClause = Nothing, offsetClause = Nothing})})) (p "SELECT * FROM test")

dateExamplesConnection :: NotificationCallback -> IO (SessionId, Connection)
dateExamplesConnection callback = do
  dbconn <- connectProjectM36 (InProcessConnectionInfo NoPersistence callback [])
  case dbconn of 
    Left err -> error (show err)
    Right conn -> do
      eSessionId <- createSessionAtHead conn "master"
      case eSessionId of
        Left err -> error (show err)
        Right sessionId -> do
          executeDatabaseContextExpr sessionId conn (databaseContextAsDatabaseContextExpr dateExamples) >>= eitherFail
      --skipping atom functions for now- there are no atom function manipulation operators yet
          commit sessionId conn >>= eitherFail
          pure (sessionId, conn)

eitherFail :: Either RelationalError a -> IO ()
eitherFail (Left err) = assertFailure (show err)
eitherFail (Right _) = pure ()

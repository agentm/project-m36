{-# LANGUAGE OverloadedStrings #-}
import SQL.Interpreter.Select
import SQL.Interpreter.Convert
--import TutorialD.Interpreter.RelationalExpr
import TutorialD.Interpreter.RODatabaseContextOperator
--import TutorialD.Printer
import ProjectM36.DataTypes.SQL.Null
import ProjectM36.RelationalExpression
import ProjectM36.TransactionGraph
import ProjectM36.DateExamples
import ProjectM36.DatabaseContext
import ProjectM36.NormalizeExpr
import ProjectM36.Client
import ProjectM36.SQLDatabaseContext
import ProjectM36.Base
import ProjectM36.Relation
import qualified ProjectM36.Attribute as A
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
  let sqlDBContext = dateExamples { relationVariables = M.insert "snull" (ExistingRelation s_nullRelVar) (relationVariables dateExamples) }
  (tgraph,transId) <- freshTransactionGraph sqlDBContext
  (sess, conn) <- dateExamplesConnection emptyNotificationCallback
  
  let readTests = [
        -- simple relvar
        ("SELECT * FROM s", "(s)", "(s)"),
        -- simple projection
        ("SELECT city FROM s", "(s{city})", "(s{city})"),
        -- restriction
        ("SELECT city FROM s where status=20",
         "((s where sql_coalesce_bool(sql_equals(@status,20))){city})",
         "((s where status=20){city})"         
        ),
        -- restriction with asterisk and qualified name
        ("SELECT * FROM s WHERE \"s\".\"status\"=20",
         "(s where sql_coalesce_bool(sql_equals(@status,20)))",
         "(s where status=20)"),          
        -- join via where clause
        ("SELECT city FROM s, sp where \"s\".\"s#\" = \"sp\".\"s#\"",
         "((((s rename {s# as `s.s#`}) join sp) where sql_coalesce_bool(sql_equals(@`s.s#`, @s#))){city})",
         "(s{city} where city=\"London\" or city=\"Paris\")"
         ),
        -- restriction
        ("SELECT status,city FROM s where status>20",
         "((s where sql_coalesce_bool(sql_gt(@status,20))){status,city})",
         "((s where s#=\"S3\" or s#=\"S5\"){status,city})"),
        -- extension mixed with projection
        ("SELECT city,status,10 FROM s",
         "((s:{attr_3:=10}){city,status,attr_3})",
         "((s:{attr_3:=10}){city,status,attr_3})"         
         ),
        -- column alias
        ("SELECT city AS x FROM s",
         "((s rename {city as x}){x})",
         "((s rename {city as x}){x})"         
         ),
        -- case insensitivity
        ("sElECt CitY aS X FRoM s",
         "((s rename {city as x}){x})",
         "((s rename {city as x}){x})"         
        ),
        --column from aliased table
        ("SELECT sup.city FROM s AS sup",
         "(with (sup as s) ((sup rename {city as `sup.city`}){`sup.city`}))",
         "((s rename {city as `sup.city`}){`sup.city`})"),
        --projection with alias
        ("SELECT sup.city,sup.sname FROM s AS sup",
         "(with (sup as s) ((sup rename {city as `sup.city`,sname as `sup.sname`}){`sup.city`,`sup.sname`}))",
         "((s rename {city as `sup.city`, sname as `sup.sname`}){`sup.city`,`sup.sname`})"
         ),
        ("SELECT sup.* FROM s as sup",
         "(with (sup as s) (sup{all from sup}))",
         "(s)"
         ),
        -- natural join
        ("SELECT * FROM s NATURAL JOIN sp",
         "(s join sp)",
         "(s join sp)"),
        -- cross join
        ("SELECT * FROM s CROSS JOIN sp",
         "((s rename {s# as `s.s#`}) join sp)",
         "((s rename {s# as `s.s#`}) join sp)"),         
        -- unaliased join using
        ("SELECT * FROM sp INNER JOIN sp AS sp2 USING (\"s#\")",
         "(with (sp2 as sp) ((sp rename {p# as `sp.p#`, qty as `sp.qty`}) join (sp2 rename {p# as `sp2.p#`, qty as `sp2.qty`})))",
         "(with (sp2 as sp) ((sp rename {p# as `sp.p#`, qty as `sp.qty`}) join (sp2 rename {p# as `sp2.p#`, qty as `sp2.qty`})))"),
        -- unaliased join
        ("SELECT * FROM sp JOIN s ON s.s# = sp.s#",
         "(((((s rename {s# as `s.s#`,sname as `s.sname`,city as `s.city`,status as `s.status`}) join (sp rename {s# as `sp.s#`,p# as `sp.p#`,qty as `sp.qty`})):{join_1:=sql_coalesce_bool(sql_equals(@`s.s#`,@`sp.s#`))}) where join_1=True) {all but join_1})",
         "(((((s rename {s# as `s.s#`,sname as `s.sname`,city as `s.city`,status as `s.status`}) join (sp rename {s# as `sp.s#`,p# as `sp.p#`,qty as `sp.qty`})):{join_1:=sql_coalesce_bool(sql_equals(@`s.s#`,@`sp.s#`))}) where join_1=True) {all but join_1})"         
         ),
        -- aliased join on
        ("SELECT * FROM sp AS sp2 JOIN s AS s2 ON s2.s# = sp2.s#",
         "(with (s2 as s, sp2 as sp) ((((s2 rename {s# as `s2.s#`,sname as `s2.sname`,city as `s2.city`,status as `s2.status`}) join (sp2 rename {s# as `sp2.s#`,p# as `sp2.p#`,qty as `sp2.qty`})):{join_1:=sql_coalesce_bool(sql_equals(@`s2.s#`,@`sp2.s#`))}) where join_1=True) {all but join_1})",
         "(with (s2 as s, sp2 as sp) ((((s2 rename {s# as `s2.s#`,sname as `s2.sname`,city as `s2.city`,status as `s2.status`}) join (sp2 rename {s# as `sp2.s#`,p# as `sp2.p#`,qty as `sp2.qty`})):{join_1:=sql_coalesce_bool(sql_equals(@`s2.s#`,@`sp2.s#`))}) where join_1=True) {all but join_1})"),         
        -- formula extension
        ("SELECT status+10 FROM s",
         "((s : {attr_1:=sql_add(@status,10)}) { attr_1 })",
         "((s : {attr_1:=sql_add(@status,10)}) { attr_1 })"),
        -- extension and formula
        ("SELECT status+10,city FROM s",
         "((s : {attr_1:=sql_add(@status,10)}) {city,attr_1})",
         "((s : {attr_1:=sql_add(@status,10)}) {city,attr_1})"),
        -- complex join condition
        ("SELECT * FROM sp JOIN s ON s.s# = sp.s# AND s.s# = sp.s#",
         "(((((s rename {s# as `s.s#`,sname as `s.sname`,city as `s.city`,status as `s.status`}) join (sp rename {s# as `sp.s#`,p# as `sp.p#`,qty as `sp.qty`})):{join_1:=sql_coalesce_bool(sql_and(sql_equals(@`s.s#`,@`sp.s#`),sql_equals(@`s.s#`,@`sp.s#`)))}) where join_1=True) {all but join_1})",
          "(((((s rename {s# as `s.s#`,sname as `s.sname`,city as `s.city`,status as `s.status`}) join (sp rename {s# as `sp.s#`,p# as `sp.p#`,qty as `sp.qty`})):{join_1:=sql_coalesce_bool(sql_and(sql_equals(@`s.s#`,@`sp.s#`),sql_equals(@`s.s#`,@`sp.s#`)))}) where join_1=True) {all but join_1})"),
        -- TABLE <tablename>
        ("TABLE s",
         "(s)",
         "(s)"),
        -- any, all, some
        -- IN()
        ("SELECT * FROM s WHERE s# IN ('S1','S2')",
         "(s where eq(@s#,\"S1\") or eq(@s#,\"S2\"))",
         "(s where eq(@s#,\"S1\") or eq(@s#,\"S2\"))"    
         ),
        -- NOT IN()
        ("SELECT * FROM s WHERE s# NOT IN ('S1','S2')",
         "(s where not (eq(@s#,\"S1\") or eq(@s#,\"S2\")))",
         "(s where not (eq(@s#,\"S1\") or eq(@s#,\"S2\")))"
         ),
        -- where not exists
        -- group by
        -- group by having
        -- limit
        -- case when
        -- union
        -- intersect
        -- except
        ("SELECT * FROM s LIMIT 10",
         "(s) limit 10",
         "(s) limit 10"         
         ),
        -- offset
        ("SELECT * FROM s OFFSET 10",
         "(s) offset 10",
         "(s) offset 10"         
         ),
        -- limit offset
        ("SELECT * FROM s LIMIT 10 OFFSET 20",
         "(s) limit 10 offset 20",
         "(s) limit 10 offset 20"
         ),
        -- order by
        ("SELECT * FROM s ORDER BY status",
         "(s) orderby {status}",
         "(s) orderby {status}"         
         ),
        -- order by descending
        ("SELECT * FROM s ORDER BY status DESC,city",
         "(s) orderby {status descending,city}",
         "(s) orderby {status descending,city}"         
        ),
        -- CTEs
        ("WITH x AS (SELECT * FROM s) SELECT * FROM x",
         "(with (x as s) x)",
         "(s)"
         ),
        -- SELECT with no table expression
        ("SELECT 1,2,3",
         "((relation{}{tuple{}}:{attr_1:=1,attr_2:=2,attr_3:=3}){attr_1,attr_2,attr_3})",
         "(relation{tuple{attr_1 1, attr_2 2, attr_3 3}})"
         ),
        -- where exists
        -- complication: we need to add attribute renamers due to the subselect
        ("SELECT * FROM s WHERE EXISTS (SELECT * FROM sp WHERE \"s\".\"s#\"=\"sp\".\"s#\")",
         "(s where (((sp rename {s# as `sp.s#`}) where sql_coalesce_bool(sql_equals(@`s#`, @`sp.s#`))){}))",
         "(s where not (s#=\"S5\"))"
         ),
        -- basic projection NULL
        ("SELECT NULL",
         "((relation{}{tuple{}}:{attr_1:=SQLNull}){attr_1})",
         "((true:{attr_1:=SQLNull}){attr_1})"
         ),
        -- restriction NULL
        ("SELECT * FROM s WHERE s# IS NULL",
         "(s where sql_coalesce_bool(sql_isnull(@s#)))",
         "(s where false)"),
        ("SELECT * FROM snull WHERE city IS NULL",
         "(snull where sql_coalesce_bool(sql_isnull(@city)))",
         "(snull where s#=\"S2\")"),
        ("SELECT NULL AND FALSE",
         "((relation{}{tuple{}}:{attr_1:=sql_and(SQLNull,False)}){attr_1})",
         "(relation{attr_1 SQLNullable Bool}{tuple{attr_1 SQLJust False}})"),
        ("SELECT NULL AND TRUE",
         "((relation{}{tuple{}}:{attr_1:=sql_and(SQLNull,True)}){attr_1})",
         "(relation{attr_1 SQLNullable Bool}{tuple{attr_1 SQLNull}})")
        ]
      gfEnv = GraphRefRelationalExprEnv {
        gre_context = Just sqlDBContext,
        gre_graph = tgraph,
        gre_extra = mempty }
      typeF expr = do
        let gfExpr = runProcessExprM (TransactionMarker transId) (processRelationalExpr expr)
        runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr gfExpr)
      parseTutd tutd = do
        case parse (dataFrameP <* eof) "test" tutd of
          Left err -> assertFailure (errorBundlePretty err)
          Right x -> do
            pure x        
      check (sql, equivalent_tutd, confirmation_tutd) = do
        print sql
        --parse SQL
        select <- case parse (queryExprP <* eof) "test" sql of
          Left err -> assertFailure (errorBundlePretty err)
          Right x -> do
            --print ("parsed SQL:"::String, x)
            pure x
        --parse tutd
        tutdAsDFExpr <- parseTutd equivalent_tutd
        selectAsDFExpr <- case evalConvertM mempty (convertSelect typeF select) of
          Left err -> assertFailure (show err)
          Right x -> do
            --print ("convert SQL->tutd:"::String, x)
            pure x
        confirmationDFExpr <- parseTutd confirmation_tutd

        --print ("selectAsRelExpr"::String, selectAsRelExpr)
        --print ("expected: "::String, pretty tutdAsDFExpr)
        --print ("actual  : "::String, pretty selectAsDFExpr)
        assertEqual (T.unpack sql) tutdAsDFExpr selectAsDFExpr
        --check that the expression can actually be executed
        eEvald <- executeDataFrameExpr sess conn tutdAsDFExpr
        sqlResult <- case eEvald of
          Left err -> assertFailure (show err <> ": " <> show tutdAsDFExpr)
          Right rel -> pure rel
        eConfirmationEvald <- executeDataFrameExpr sess conn confirmationDFExpr
        print ("confirmation"::String, confirmation_tutd)
        confirmationResult <- case eConfirmationEvald of
          Left err -> assertFailure (show err <> ": " <> show confirmationDFExpr)
          Right rel -> pure rel
        assertEqual "SQL result confirmation" confirmationResult sqlResult
  mapM_ check readTests  
  
--  assertEqual "SELECT * FROM test"  (Right (Select {distinctness = Nothing, projectionClause = [(Identifier (QualifiedProjectionName [Asterisk]),Nothing)], tableExpr = Just (TableExpr {fromClause = [SimpleTableRef (QualifiedName ["test"])], whereClause = Nothing, groupByClause = [], havingClause = Nothing, orderByClause = [], limitClause = Nothing, offsetClause = Nothing})})) (p "SELECT * FROM test")
dateExamplesConnection :: NotificationCallback -> IO (SessionId, Connection)
dateExamplesConnection callback = do
  dbconn <- connectProjectM36 (InProcessConnectionInfo NoPersistence callback [] sqlDatabaseContext)
  case dbconn of 
    Left err -> error (show err)
    Right conn -> do
      eSessionId <- createSessionAtHead conn "master"
      case eSessionId of
        Left err -> error (show err)
        Right sessionId -> do
          executeDatabaseContextExpr sessionId conn (databaseContextAsDatabaseContextExpr dateExamples) >>= eitherFail
          --add a relvar with some nulls
          executeDatabaseContextExpr sessionId conn addNullTable >>= eitherFail
          commit sessionId conn >>= eitherFail
          pure (sessionId, conn)


eitherFail :: Either RelationalError a -> IO ()
eitherFail (Left err) = assertFailure (show err)
eitherFail (Right _) = pure ()

addNullTable :: DatabaseContextExpr
addNullTable = Assign "snull" (ExistingRelation s_nullRelVar)

-- snull := relation{s# Text, sname Text, status Integer, city SQLNullable Text}{tuple{s# "S1", sname "Smith", status 20, city SQLNull}}
s_nullRelVar :: Relation
s_nullRelVar =
  case mkRelationFromList attrs atomMatrix of
      Left err -> error (show err)
      Right rel -> rel
  where
    attrs = A.attributesFromList [Attribute "s#" TextAtomType,
                                  Attribute "sname" TextAtomType,
                                  Attribute "status" IntegerAtomType,
                                  Attribute "city" (nullAtomType TextAtomType)]
    atomMatrix = [
      [TextAtom "S1", TextAtom "Smith", IntegerAtom 20, nullAtom TextAtomType (Just (TextAtom "London"))],
      [TextAtom "S2", TextAtom "Jones", IntegerAtom 10, nullAtom TextAtomType Nothing]
      ]

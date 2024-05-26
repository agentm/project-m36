{-# LANGUAGE OverloadedStrings #-}
import SQL.Interpreter.Select
import ProjectM36.SQL.Convert
import ProjectM36.SQL.Select
import TutorialD.Interpreter.RODatabaseContextOperator
import TutorialD.Interpreter.DatabaseContextExpr
import ProjectM36.RelationalExpression
--import ProjectM36.StaticOptimizer
import SQL.Interpreter.DBUpdate
import SQL.Interpreter.CreateTable
import SQL.Interpreter.Base (semi)
import ProjectM36.DataTypes.SQL.Null
import ProjectM36.TransactionGraph
import ProjectM36.DateExamples
import ProjectM36.DatabaseContext
import ProjectM36.NormalizeExpr
import ProjectM36.Client hiding (typeConstructorMapping)
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
    tests = [testFindColumn,
             testSelect,
             testCreateTable,
             testDBUpdate
            ]


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
  let sqlDBContext = dateExamples { relationVariables = M.insert "snull" (ExistingRelation sNullRelVar) (relationVariables dateExamples) }
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
        -- any, all, some
        -- IN()
        ("SELECT * FROM s WHERE s# IN ('S1','S2')",
         "(s where sql_coalesce_bool(sql_or(sql_equals(@s#,\"S1\"),sql_equals(@s#,\"S2\"))))",
         "(s where s#=\"S1\" or s#=\"S2\")"    
         ),
        -- NOT IN()
        ("SELECT * FROM s WHERE s# NOT IN ('S1','S2')",
         "(s where not sql_coalesce_bool(sql_or(sql_equals(@s#,\"S1\"),sql_equals(@s#,\"S2\"))))",
         "(s where not (eq(@s#,\"S1\") or eq(@s#,\"S2\")))"
         ),
        -- function application
        ("SELECT abs(-4)",
         "((relation{}{tuple{}}:{attr_1:=sql_abs(sql_negate(4))}){attr_1})",
         "(relation{tuple{attr_1 SQLJust 4}})"
         ),
        -- where not exists
        -- group by with max aggregate
        ("SELECT city,max(status) FROM s GROUP BY city",
         "((s group ({all but city} as `_sql_aggregate`) : {attr_2:=sql_max(@`_sql_aggregate`{status})}){city,attr_2})",
         "(relation{city Text, attr_2 SQLNullable Integer}{tuple{city \"London\", attr_2 SQLJust 20}, tuple{city \"Paris\", attr_2 SQLJust 30}, tuple{city \"Athens\", attr_2 SQLJust 30}})"
         ),
        -- group by with aggregate max column alias
        ("SELECT city,max(status) as status FROM s GROUP BY city",
         "((s group ({all but city} as `_sql_aggregate`) : {status:=sql_max(@`_sql_aggregate`{status})}){city,status})",
         "(relation{city Text, status SQLNullable Integer}{tuple{city \"London\", status SQLJust 20}, tuple{city \"Paris\", status SQLJust 30}, tuple{city \"Athens\", status SQLJust 30}})"),
        -- aggregate max without grouping
        ("SELECT max(status) as status FROM s",
         "(((s group ({all but } as `_sql_aggregate`)):{status:=sql_max( (@`_sql_aggregate`){ status } )}){ status })",
         "(relation{status SQLNullable Integer}{tuple{status SQLJust 30}})"),
        -- group by having max
        ("select city,max(status) as status from s group by city having max(status)=30",
         "((((s group ({all but city} as `_sql_aggregate`)):{status:=sql_max( (@`_sql_aggregate`){ status } ), `_sql_having`:=sql_coalesce_bool( sql_equals( sql_max( (@`_sql_aggregate`){ status } ), 30 ) )}){ city, status }) where `_sql_having`=True)",
         "(relation{city Text,status SQLNullable Integer}{tuple{city \"Athens\",status SQLJust 30},tuple{city \"Paris\",status SQLJust 30}})"),
        -- count(*) aggregate
        ("select count(*) as c from s",
         "(((s group ({all but } as `_sql_aggregate`)):{c:=sql_count( @`_sql_aggregate` )}){ c })",
         "(relation{tuple{c 5}})"
        ),
        -- count(city) aggregate counts how many cities are not null
        ("select count(city) as c from s",
         "(((s group ({all but } as `_sql_aggregate`)):{c:=sql_count( ((@`_sql_aggregate`) where not( sql_isnull( @city ) )) )}){ c })",
         "(relation{tuple{c 5}})"
         ),
        -- case when
        ("SELECT city,case when city='London' then true else false end as islondon from s",
         "((s:{islondon:=if sql_coalesce_bool( sql_equals( @city, \"London\" ) ) then True else False}){ city, islondon })",
         "(relation{tuple{city \"London\", islondon True},tuple{city \"Paris\", islondon False},tuple{city \"Athens\", islondon False}})"
         ),
        -- union
        ("SELECT * FROM s union select * from s",
         "(s union s)",
         "(s)"
         ),
        -- intersect
        ("select city from s intersect select 'London' as city",
         "((s{ city }) join ((relation{  }{ tuple{  } }:{city:=\"London\"}){ city }))",
         "(relation{tuple{city \"London\"}})"
         ),
        -- except
        ("select city from s except select 'London' as city",
         "((s{city}) minus ((relation{}{tuple{}}:{city:=\"London\"}){city}))",
         "(relation{tuple{city \"Athens\"}, tuple{city \"Paris\"}})"
         ),
        -- limit        
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
        ("SELECT NULL", -- convert to null of text type by default
         "((relation{}{tuple{}}:{attr_1:=SQLNullOfUnknownType}){attr_1})",
         "((true:{attr_1:=SQLNullOfUnknownType}){attr_1})"
         ),
        -- restriction NULL
        ("SELECT * FROM s WHERE s# IS NULL",
         "(s where sql_coalesce_bool(sql_isnull(@s#)))",
         "(s where false)"),
        ("SELECT * FROM snull WHERE city IS NULL",
         "(snull where sql_coalesce_bool(sql_isnull(@city)))",
         "(snull where s#=\"S2\")"),
        -- restriction IS NOT NULL
        ("SELECT city FROM s WHERE city IS NOT NULL",
         "(((s where not sql_coalesce_bool(sql_isnull(@city)))){city})",
         "(s{city})"
         ),
        -- some basic NULL logic
        ("SELECT NULL AND FALSE",
         "((relation{}{tuple{}}:{attr_1:=sql_and(SQLNullOfUnknownType,False)}){attr_1})",
         "(relation{attr_1 SQLNullable Bool}{tuple{attr_1 SQLJust False}})"),
        ("SELECT NULL AND TRUE",
         "((relation{}{tuple{}}:{attr_1:=sql_and(SQLNullOfUnknownType,True)}){attr_1})",
         "(relation{attr_1 SQLNullable Bool}{tuple{attr_1 SQLNull}})"),
        ("SELECT NULL OR FALSE",
         "((relation{}{tuple{}}:{attr_1:=sql_or(SQLNullOfUnknownType,False)}){attr_1})",
         "(relation{attr_1 SQLNullable Bool}{tuple{attr_1 SQLNull}})"),
        ("SELECT NULL OR TRUE",
         "((relation{}{tuple{}}:{attr_1:=sql_or(SQLNullOfUnknownType,True)}){attr_1})",
         "(relation{attr_1 SQLNullable Bool}{tuple{attr_1 SQLJust True}})")
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
        query <- case parse (queryP <* eof) "test" sql of
          Left err -> assertFailure (errorBundlePretty err)
          Right x -> do
            --print ("parsed SQL:"::String, x)
            pure x
        --parse tutd
        tutdAsDFExpr <- parseTutd equivalent_tutd
        queryAsDFExpr <- case evalConvertM mempty (convertQuery typeF query) of
          Left err -> assertFailure (show err)
          Right x -> do
            --print ("convert SQL->tutd:"::String, x)
            pure x
        confirmationDFExpr <- parseTutd confirmation_tutd

        --print ("selectAsRelExpr"::String, queryAsRelExpr)
        --print ("expected: "::String, pretty tutdAsDFExpr)
        --print ("actual  : "::String, pretty queryAsDFExpr)
        assertEqual (T.unpack sql) tutdAsDFExpr queryAsDFExpr
        --check that the expression can actually be executed
        eEvald <- executeDataFrameExpr sess conn tutdAsDFExpr
        sqlResult <- case eEvald of
          Left err -> assertFailure (show err <> ": " <> show tutdAsDFExpr)
          Right rel -> pure rel
        eConfirmationEvald <- executeDataFrameExpr sess conn confirmationDFExpr
--        print ("confirmation"::String, confirmation_tutd)
        confirmationResult <- case eConfirmationEvald of
          Left err -> assertFailure (show err <> ": " <> show confirmationDFExpr)
          Right rel -> pure rel
        assertEqual "SQL result confirmation" confirmationResult sqlResult
  mapM_ check readTests

testCreateTable :: Test
testCreateTable = TestCase $ do
  let sqlDBContext = dateExamples { relationVariables = M.insert "snull" (ExistingRelation sNullRelVar) (relationVariables dateExamples) }
  (tgraph,transId) <- freshTransactionGraph sqlDBContext

  let createTableTests = [
        --no columns
        ("create table test()",
          "test :: {}"
        ),
        --simple column
        ("create table test(col1 integer)",
         "test :: {col1 SQLNullable Integer}"
         ),
        --not null
        ("create table test(col1 integer not null)",
         "test :: {col1 Integer}"
        ),
        ("create table test(col1 integer, \"col2\" text not null)",
         "test :: {col1 SQLNullable Integer, col2 Text}"
        ),
        -- foreign key "references"
        ("create table test(col1 integer, col2 integer references test2(pk))",
         "test :: {col1 SQLNullable Integer, col2 SQLNullable Integer}; foreign key test_col2__test2_pk_fk test{col2} in test2{pk}"),
        -- uniqueness constraint
        ("create table test(col1 integer unique)",
         "test :: {col1 SQLNullable Integer};  key test_col1_unique {col1} test where not(sql_isnull(@col1))"),
        -- primary key (equivalent to uniqueness constraint + not null)
        ("create table test(col1 integer primary key)",
         "test :: {col1 Integer}; key test_col1_key {col1} test")
        ]
      parseTutd tutd = do
        case parse (multipleDatabaseContextExprP <* eof) "test" tutd of
          Left err -> assertFailure (errorBundlePretty err)
          Right x -> do
            pure x
      gfEnv = GraphRefRelationalExprEnv {
        gre_context = Just sqlDBContext,
        gre_graph = tgraph,
        gre_extra = mempty }
      typeF expr = do
        let gfExpr = runProcessExprM (TransactionMarker transId) (processRelationalExpr expr)
        runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr gfExpr)
            
      check (sql, equivalent_tutd) = do
        --parse SQL
        query <- case parse (createTableP <* eof) "test" sql of
          Left err -> assertFailure (errorBundlePretty err)
          Right x -> do
            --print ("parsed SQL:"::String, x)
            pure x
        --parse tutd
        tutdAsDFExpr <- parseTutd equivalent_tutd
        queryAsDFExpr <- case evalConvertM mempty (convertCreateTable typeF query) of
          Left err -> assertFailure (show err)
          Right x -> do
            --print ("convert SQL->tutd:"::String, x)
            pure x
        print sql
        assertEqual "create table SQL" tutdAsDFExpr queryAsDFExpr

  mapM_ check createTableTests

testDBUpdate :: Test
testDBUpdate = TestCase $ do
  let sqlDBContext = dateExamples { relationVariables =
                                      M.insert "snull" (ExistingRelation sNullRelVar) (relationVariables dateExamples),
                                    typeConstructorMapping = typeConstructorMapping dateExamples <> nullTypeConstructorMapping
                                  }
  (tgraph,transId) <- freshTransactionGraph sqlDBContext

  let updateTests = [
        -- simple insert with no nulls
        ("insert into s(city,status) values(\'New York\',15);",
         "insert s relation{tuple{attr_1 \"New York\", attr_2 15}} rename {attr_1 as city, attr_2 as status}"
        ),
        -- simple insert into nullable column with value
        ("insert into snull(\"s#\",sname,status,city) values ('S6','Smith',20,'New York');",
         "insert snull ((relation{tuple{attr_1 \"S6\", attr_2 \"Smith\", attr_3 20, attr_4 \"New York\"}} rename {attr_1 as s#, attr_2 as sname, attr_3 as status}) : {city:=SQLJust @attr_4}){all but attr_4}" 
         ),
        -- simple insert into nullable column with NULL
        ("insert into snull(\"s#\",sname,status,city) values ('S6','Smith',20,NULL);",
         "insert snull ((relation{tuple{attr_1 \"S6\", attr_2 \"Smith\", attr_3 20, attr_4 SQLNullOfUnknownType}} rename {attr_1 as s#, attr_2 as sname, attr_3 as status}):{city:=SQLNull}){all but attr_4}" 
         ),
        -- simple update
        ("update s set city='New York' where status=20;",
         "update s where sql_coalesce_bool(sql_equals(@status,20)) (city:=\"New York\")"
        ),
        --simple delete
        ("delete from s where city='New York';",
         "delete s where sql_coalesce_bool(sql_equals(@city,\"New York\"))"
         )
        ]

      parseTutd tutd = do
        case parse (multipleDatabaseContextExprP <* eof) "test" tutd of
          Left err -> assertFailure (errorBundlePretty err)
          Right x -> do
            pure x
      gfEnv = GraphRefRelationalExprEnv {
        gre_context = Just sqlDBContext,
        gre_graph = tgraph,
        gre_extra = mempty }
{-      typeF = 
        let reEnv = mkRelationalExprEnv sqlDBContext tgraph in
          optimizeAndEvalRelationalExpr reEnv-}
      typeF expr = do
        let gfExpr = runProcessExprM (TransactionMarker transId) (processRelationalExpr expr)
        runGraphRefRelationalExprM gfEnv (typeForGraphRefRelationalExpr gfExpr)
            
      check (sql, equivalent_tutd) = do
        --parse SQL
        query <- case parse (dbUpdateP <* semi <* eof) "test" sql of
          Left err -> assertFailure (errorBundlePretty err)
          Right x -> do
            --print ("parsed SQL:"::String, x)
            pure x
        --parse tutd
        tutdAsDFExpr <- parseTutd equivalent_tutd
        queryAsDFExpr <- case evalConvertM mempty (convertDBUpdate typeF query) of
          Left err -> assertFailure (show err)
          Right x -> do
            --print ("convert SQL->tutd:"::String, x)
            pure x
        print sql
        assertEqual "db update SQL" tutdAsDFExpr queryAsDFExpr

  mapM_ check updateTests

{-
testTransactionGraphOps :: Test
testTransactionGraphOps = TestCase $ do
  let sqlDBContext = dateExamples { relationVariables =
                                      M.insert "snull" (ExistingRelation sNullRelVar) (relationVariables dateExamples),
                                    typeConstructorMapping = typeConstructorMapping dateExamples <> nullTypeConstructorMapping
                                  }
  (tgraph,transId) <- freshTransactionGraph sqlDBContext

  let graphTests = [("begin;create table x(a integer not null);commit;",
                     "(x==relation{a integer})")
                   ]
-}
  
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
addNullTable = Assign "snull" (ExistingRelation sNullRelVar)

-- snull := relation{s# Text, sname Text, status Integer, city SQLNullable Text}{tuple{s# "S1", sname "Smith", status 20, city SQLNull}}
sNullRelVar :: Relation
sNullRelVar =
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

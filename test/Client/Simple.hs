import ProjectM36.Client.Simple
import Test.HUnit
import System.Exit
import ProjectM36.Relation
import qualified ProjectM36.Client as C
import ProjectM36.DateExamples
import ProjectM36.DatabaseContext.Basic
import System.IO.Temp
import System.FilePath
import ProjectM36.TupleSet as TS
import ProjectM36.Attribute
import qualified Data.Map as M

main :: IO ()           
main = do 
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess
  
testList :: Test
testList = TestList [testSimpleCommitSuccess, testSimpleCommitFailure, testSimpleUpdate]

assertEither :: (Show a) => IO (Either a b) -> IO b
assertEither x = do
  res <- x
  case res of
    Left err -> assertFailure (show err) >> undefined
    Right val -> pure val

testSimpleCommitSuccess :: Test
testSimpleCommitSuccess = TestCase $
  withSystemTempDirectory "m36tempdb" $ \tempdir -> do
    let connInfo = InProcessConnectionInfo (MinimalPersistence (tempdir </> "db")) emptyNotificationCallback [] basicDatabaseContext C.adminRoleName
        relExpr = Union (RelationVariable "x" ()) (RelationVariable "y" ())
    
    dbconn <- assertEither (simpleConnectProjectM36 connInfo)
    Right rel <- withTransaction dbconn $ do
      
      execute (Assign "x" (ExistingRelation relationTrue))
      execute (Assign "y" (ExistingRelation relationFalse))
      query relExpr
  
    assertEqual "true/false simple" relationTrue rel
  -- re-open with standard API and validate that the relvars are available
    conn <- assertEither $ C.connectProjectM36 connInfo
    
    sess <- assertEither $ C.createSessionAtHead conn "master"
    eRes <- C.executeRelationalExpr sess conn relExpr
    assertEqual "x and y" (Right relationTrue) eRes
    

testSimpleCommitFailure :: Test
testSimpleCommitFailure = TestCase $ do
  let failAttrs = attributesFromList [Attribute "fail" IntAtomType]
  err <- withSystemTempDirectory "m36tempdb" $ \tempdir -> do
    let connInfo = InProcessConnectionInfo (MinimalPersistence (tempdir </> "db")) emptyNotificationCallback [] basicDatabaseContext C.adminRoleName
    dbconn <- assertEither (simpleConnectProjectM36 connInfo)
    withTransaction dbconn $ do
      execute $ Assign "x" (ExistingRelation relationTrue)
      --cause error
      execute $ Assign "x" (MakeStaticRelation failAttrs TS.empty)
  let expectedErr = Left (RelError (RelationTypeMismatchError mempty failAttrs))
  assertEqual "dbc error" expectedErr err

-- #176 default merge couldn't handle Update  
testSimpleUpdate :: Test
testSimpleUpdate = TestCase $ do
  let connInfo = InProcessConnectionInfo NoPersistence emptyNotificationCallback [] basicDatabaseContext C.adminRoleName
  dbconn <- assertEither (simpleConnectProjectM36 connInfo)
  Right dateExprs <- pure dateExamplesDatabaseContextExpr
  assertEither $ withTransaction dbconn $ 
    execute dateExprs
  assertEither $ withTransaction dbconn $ 
    execute $ Update "s" (M.singleton "sname" (C.NakedAtomExpr (C.TextAtom "Blakey"))) (C.AttributeEqualityPredicate "sname" (C.NakedAtomExpr (C.TextAtom "Blake")))

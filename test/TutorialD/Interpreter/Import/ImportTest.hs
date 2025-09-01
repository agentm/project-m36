{-# LANGUAGE CPP #-}
import Test.HUnit
import ProjectM36.Base
import TutorialD.Interpreter.Import.TutorialD
import System.Exit
import qualified Data.Text as T
import System.IO.Temp
import qualified Data.Map as M
import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TE
import Text.URI hiding (makeAbsolute)
import Network.Wai.Handler.Warp
import Control.Concurrent.MVar
import Control.Concurrent
import Network.Wai
import Network.HTTP.Types

main :: IO ()
main = do 
  tcounts <- runTestTT $ TestList [testTutdFileImport
                                  ,testTutdHTTPSImport
                                  ]
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testTutdFileImport :: Test
testTutdFileImport = TestCase $
  withSystemTempFile "m.tutd" $ \tempPath handle -> do
    BS.hPut handle (TE.encodeUtf8 "x:=relation{tuple{a 5,b \"spam\"}}; y:=relation{tuple{b \"漢字\"}}")
    hClose handle
    let expectedExpr = MultipleExpr [
          Assign "x" (MakeRelationFromExprs Nothing 
                      $ TupleExprs () [TupleExpr (M.fromList [("a", NakedAtomExpr $ IntegerAtom 5),
                                              ("b", NakedAtomExpr $ TextAtom "spam")])]),
          Assign "y" (MakeRelationFromExprs Nothing 
                      $ TupleExprs () [TupleExpr (M.fromList [("b", NakedAtomExpr (TextAtom "漢字"))])])]
    --on Windows, the file URI should not include the drive letter "/c/Users..." -> "/Users"
#if defined(mingw32_HOST_OS)
    let uri = "file:" <> map (\c -> if c == '\\' then '/' else c) tempPath
#else
    let uri = "file:" <> tempPath   
#endif
    fileURI <- mkURI (T.pack uri)
    imported <- importTutorialDFromFile fileURI Nothing
    assertEqual "import tutd" (Right expectedExpr) imported


startTestHTTPServer :: MVar () -> IO ThreadId
startTestHTTPServer startVar = do
  let settings = setHost "127.0.0.1" $ setBeforeMainLoop (putMVar startVar ()) $ setPort 8899 defaultSettings
      app req respond = do
        case pathInfo req of
          ["test1"] -> respond $ responseLBS status200 [] "x:=true;\ny:=false;"
          other -> respond $ responseLBS status404 [] (BSL.fromStrict $ TE.encodeUtf8 $ T.pack ("no path at" <> show other))
  forkIO $ runSettings settings app

testTutdHTTPSImport :: Test
testTutdHTTPSImport = TestCase $ do
  uri <- mkURI "http://localhost:8899/test1"
  let hash = "effe32b247586dc3ac0079fc241b9618d41d189afcaeb7907edbe5a8b45992a4"
      expected = Right (MultipleExpr [Assign "x" (RelationVariable "true" ()),Assign "y" (RelationVariable "false" ())])
  continueTestVar <- newEmptyMVar
  httpServerThread <- startTestHTTPServer continueTestVar 
  actual <- importTutorialDViaHTTP uri (Just hash)
  assertEqual "github https" expected actual
  killThread httpServerThread
  

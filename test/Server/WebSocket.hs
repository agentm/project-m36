-- test the websocket server

import Test.HUnit
import qualified Network.WebSockets as WS
import ProjectM36.Server.WebSocket
import ProjectM36.Server.Config
import ProjectM36.Server
import ProjectM36.Client
import Network.Socket
import Control.Exception
import Control.Concurrent
import System.Exit
import Data.Typeable
import Data.Text hiding (map)
import Data.Aeson
import ProjectM36.Base
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import ProjectM36.Relation

--start the websocket server
-- run some tutoriald against it

launchTestServer :: IO (PortNumber, DatabaseName)
launchTestServer = do
  portMVar <- newEmptyMVar
  let config = defaultServerConfig { databaseName = testDatabaseName }
      testDatabaseName = "test"
  -- start normal server
  _ <- forkIO (launchServer config (Just portMVar) >> pure ())
  serverPort <- takeMVar portMVar
  let serverHost = "127.0.0.1"
  let testPort = 8889
  -- start websocket server proxy -- runServer doesn't support returning an arbitrary socket
  _ <- forkIO (WS.runServer serverHost testPort (websocketProxyServer serverPort serverHost))
  --wait for socket to be listening
  waitForListenSocket 5 (fromIntegral testPort)
  pure (fromIntegral testPort, testDatabaseName)
  
data TestException = WaitForSocketListenException
                   deriving (Show, Typeable)
                            
instance Exception TestException                            

waitForListenSocket :: Int -> PortNumber -> IO ()  
waitForListenSocket secondsToTry port = do
  if secondsToTry <= 0 then
    throw WaitForSocketListenException
    else do
    hostaddr <- inet_addr "127.0.0.1"
    sock <- socket AF_INET Stream defaultProtocol
    let handler :: IOException -> IO ()
        handler = \_ -> do
          threadDelay 1000000
          waitForListenSocket (secondsToTry - 1) port
    catch (connect sock (SockAddrInet port hostaddr)) handler
  
main :: IO ()
main = do
  (port, dbname) <- launchTestServer
  tcounts <- runTestTT (testList port dbname)
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testList :: PortNumber -> DatabaseName -> Test
testList port dbName = TestList $ map (\f -> f port dbName) [testBasicConnection,
                                                             testTutorialD]
                          
basicConnection :: PortNumber -> WS.ClientApp () -> IO ()
basicConnection port block = WS.runClient "127.0.0.1" (fromIntegral port) "/" block

basicConnectionWithDatabase :: PortNumber -> DatabaseName -> WS.ClientApp () -> IO ()
basicConnectionWithDatabase port dbname block = basicConnection port (\conn -> do
                                                                WS.sendTextData conn ("connectdb:" `append` (pack dbname))
                                                                block conn)
    
testBasicConnection :: PortNumber -> DatabaseName -> Test
testBasicConnection port _ = TestCase $ basicConnection port (\conn -> WS.sendClose conn ("test close"::Text))

testTutorialD :: PortNumber -> DatabaseName -> Test
testTutorialD port dbname = TestCase $ basicConnectionWithDatabase port dbname testtutd
  where
    testtutd = \conn -> do
      WS.sendTextData conn ("executetutd:" `append` ":showexpr true")
      response <- WS.receiveData conn :: IO BS.ByteString
      let decoded = decode response :: Maybe (M.Map Text Relation)
      case decoded of
        Just val -> assertEqual "round-trip true relation" (M.lookup "displayrelation" val) (Just relationTrue) >> WS.sendClose conn ("test close"::Text)
        Nothing -> assertFailure "failed to decode relation"

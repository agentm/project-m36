-- test the websocket server
import Test.HUnit
import qualified Network.WebSockets as WS
import ProjectM36.Server.WebSocket
import ProjectM36.Server.Config
import ProjectM36.Server
import ProjectM36.Client
import ProjectM36.Base

import Network.Socket
import Control.Exception
import Control.Concurrent
import System.Exit
import Data.Typeable
import Data.Text hiding (map)
import Data.Aeson
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import ProjectM36.Relation
#if MIN_VERSION_network_transport_tcp(0,6,0)                
import Network.Transport.TCP.Internal (decodeEndPointAddress)
#else
import Network.Transport.TCP (decodeEndPointAddress)
#endif

--start the websocket server
-- run some tutoriald against it

launchTestServer :: IO (PortNumber, DatabaseName)
launchTestServer = do
  addressMVar <- newEmptyMVar
  let config = defaultServerConfig { databaseName = testDatabaseName, 
                                     bindPort = 0 }
      testDatabaseName = "test"
  -- start normal server
  _ <- forkIO (launchServer config (Just addressMVar) >> pure ())
  serverAddress <- takeMVar addressMVar
  let wsServerHost = "127.0.0.1"
      wsServerPort = 8889
      Just (dbHost, dbPort, _) = decodeEndPointAddress serverAddress      
  -- start websocket server proxy -- runServer doesn't support returning an arbitrary socket
  _ <- forkIO (WS.runServer wsServerHost wsServerPort (websocketProxyServer (read dbPort) dbHost))
  --wait for socket to be listening
  waitForListenSocket 5 (fromIntegral wsServerPort)
  pure (fromIntegral wsServerPort, testDatabaseName)
  
data TestException = WaitForSocketListenException
                   deriving (Show, Typeable)
                            
instance Exception TestException                            

waitForListenSocket :: Int -> PortNumber -> IO ()  
waitForListenSocket secondsToTry port = 
  if secondsToTry <= 0 then
    throw WaitForSocketListenException
    else do
    hostaddr <- inet_addr "127.0.0.1"
    sock <- socket AF_INET Stream defaultProtocol
    let handler :: IOException -> IO ()
        handler _ = do
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
basicConnection port = WS.runClient "127.0.0.1" (fromIntegral port) "/"

basicConnectionWithDatabase :: PortNumber -> DatabaseName -> WS.ClientApp () -> IO ()
basicConnectionWithDatabase port dbname block = basicConnection port (\conn -> do
                                                                WS.sendTextData conn ("connectdb:" `append` pack dbname)
                                                                block conn)
    
testBasicConnection :: PortNumber -> DatabaseName -> Test
testBasicConnection port _ = TestCase $ basicConnection port (\conn -> WS.sendClose conn ("test close"::Text))

testTutorialD :: PortNumber -> DatabaseName -> Test
testTutorialD port dbname = TestCase $ basicConnectionWithDatabase port dbname testtutd
  where
    discardPromptInfo conn = do
      response <- WS.receiveData conn :: IO BS.ByteString
      let decoded = decode response :: Maybe (M.Map Text (M.Map Text Text))
      case decoded of
        Just _ -> pure ()
        Nothing ->  assertFailure ("failed to decode prompt info: " ++ show response)
      
    testtutd conn = do
      discardPromptInfo conn
      WS.sendTextData conn ("executetutd/json:" `append` ":showexpr true")
      discardPromptInfo conn
      discardPromptInfo conn
      
      --receive relation response
      response <- WS.receiveData conn :: IO BS.ByteString      
      let decoded = decode response :: Maybe (M.Map Text (M.Map Text Relation))
      case decoded of 
        Nothing -> assertFailure "failed to decode"
        Just decoded' -> do
            assertEqual "round-trip true relation" ((decoded' M.! "displayrelation") M.! "json") relationTrue 
            WS.sendClose conn ("test close" :: Text)


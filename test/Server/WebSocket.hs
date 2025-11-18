{-# LANGUAGE CPP, TypeApplications #-}
-- test the websocket server
import Test.HUnit
import qualified Network.WebSockets as WS
import ProjectM36.Server.WebSocket
import ProjectM36.Server.Config
import ProjectM36.Server
import ProjectM36.Client
import qualified Network.RPC.Curryer.Server as S

import Network.Socket
import Control.Exception
import Control.Concurrent
import System.Exit
import Data.Typeable
import Data.Text hiding (map)
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import ProjectM36.Relation
import Control.Monad (void)
import Data.UUID.V4 (nextRandom)

--start the websocket server
-- run some tutoriald against it

launchTestServer :: IO (PortNumber, DatabaseName)
launchTestServer = do
  addressMVar <- newEmptyMVar
  let config = defaultServerConfig { databaseName = testDatabaseName, 
                                     bindAddress = RemoteServerHostAddress "127.0.0.1" 0,
                                     checkFS = False,
                                     connConfig = S.UnencryptedConnectionConfig
                                     }
      testDatabaseName = "test"
  -- start normal server
  void $ forkIO (void $ launchServer config (Just addressMVar))
  (SockAddrInet dbPort _) <- takeMVar addressMVar
  let wsServerHost = "127.0.0.1"
      wsServerPort = 8889
      dbHost = "127.0.0.1"
  -- start websocket server proxy -- runServer doesn't support returning an arbitrary socket
  _ <- forkIO (WS.runServer wsServerHost wsServerPort (websocketProxyServer (fromIntegral dbPort) dbHost))
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
    --hostaddr <- inet_addr "127.0.0.1"
    hostaddr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just (show port))
    sock <- socket AF_INET Stream defaultProtocol
    let handler :: IOException -> IO ()
        handler _ = do
          threadDelay 1000000
          waitForListenSocket (secondsToTry - 1) port
    catch (connect sock (addrAddress hostaddr)) handler
  
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
basicConnectionWithDatabase port dbname block = basicConnection port
  (\conn -> do
      --setup connection
      let connectMsg = encode (ConnectionSetupRequest dbname)
      WS.sendTextData conn connectMsg
      setupResponse <- WS.receiveData conn
      case eitherDecode setupResponse of
        Left err -> error (show err)
        Right ConnectionSetupResponse{} -> pure ()
        Right _other -> error (show setupResponse)
      block conn)
    
testBasicConnection :: PortNumber -> DatabaseName -> Test
testBasicConnection port _ = TestCase $ basicConnection port (\conn -> WS.sendClose conn (""::Text))

testTutorialD :: PortNumber -> DatabaseName -> Test
testTutorialD port dbname = TestCase $ basicConnectionWithDatabase port dbname testtutd
  where
    discardPromptInfo conn = do
      response <- WS.receiveData conn :: IO BS.ByteString
      case eitherDecode @Response response of
        Right _promptInfo -> pure ()
        Left err -> assertFailure ("failed to decode prompt info: " ++ err ++ ": " ++ show response)
      
    testtutd conn = do
      -- create new session at master
      reqId <- RequestId <$> nextRandom
      let createSessionMsg = encode (CreateSessionAtHeadRequest reqId "master")
      WS.sendTextData conn createSessionMsg
      sessionResponse <- WS.receiveData conn :: IO BS.ByteString
      sessionId <- case eitherDecode sessionResponse of
                     Left err -> assertFailure err
                     Right (CreateSessionAtHeadResponse reqId' sid) -> do
                       assertEqual "request ID round-trip" reqId' reqId
                       pure sid
                     Right _other -> assertFailure ("expected session response but got: " <> show sessionResponse)
      -- send tutd message
      reqIdB <- RequestId <$> nextRandom
      let tutdMsg = encode (ExecuteTutorialDRequest reqIdB sessionId jsonOnlyPresentation (TutorialDText ":showexpr true"))
      WS.sendTextData conn tutdMsg
      discardPromptInfo conn
      
      --receive relation response
      response <- WS.receiveData conn :: IO BS.ByteString      
      let decoded = decode @Response response 
      case decoded of 
        Nothing -> assertFailure "failed to decode"
        Just (RelationResponse _ trueRel _) -> do
            assertEqual "round-trip true relation" trueRel relationTrue 
            WS.sendClose conn ("" :: Text)
        Just _other -> assertFailure ("unexpected response: " <> show response)


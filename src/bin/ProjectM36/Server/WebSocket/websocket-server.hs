import ProjectM36.Server.WebSocket
import ProjectM36.Server.Config
import ProjectM36.Server.ParseArgs
import ProjectM36.Server
import Control.Concurrent
import qualified Network.WebSockets as WS
import Network.Transport.TCP (decodeEndPointAddress)

main :: IO ()
main = do
  -- launch normal project-m36-server
  addressMVar <- newEmptyMVar
  serverConfig <- parseConfig
  --usurp the serverConfig for our websocket server and make the proxied server run locally
  let wsHost = bindHost serverConfig
      wsPort = bindPort serverConfig
      serverHost = "127.0.0.1"
      serverConfig' = serverConfig {bindPort = 0, bindHost = serverHost}
  _ <- forkFinally (launchServer serverConfig' (Just addressMVar)) failureHandler
  --wait for server to be listening
  address <- takeMVar addressMVar
  let Just (_, serverPort, _) = decodeEndPointAddress address
  --this built-in server is apparently not meant for production use, but it's easier to test than starting up the wai or snap interfaces
  WS.runServer wsHost (fromIntegral wsPort) (websocketProxyServer (read serverPort) serverHost)


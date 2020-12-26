{-# LANGUAGE CPP #-}
import ProjectM36.Server.WebSocket
import ProjectM36.Server.Config
import ProjectM36.Server.ParseArgs
import ProjectM36.Server
import Control.Concurrent
import qualified Network.WebSockets as WS
import Control.Exception
import Network.Socket

main :: IO ()
main = do
  -- launch normal project-m36-server
  addressMVar <- newEmptyMVar
  serverConfig <- parseConfigWithDefaults (defaultServerConfig { bindPort = 8000, bindHost = "127.0.0.1" })
  --usurp the serverConfig for our websocket server and make the proxied server run locally
  let wsHost = bindHost serverConfig
      wsPort = bindPort serverConfig
      serverHost = "127.0.0.1"
      serverConfig' = serverConfig {bindPort = 0, bindHost = serverHost}
  _ <- forkFinally (launchServer serverConfig' (Just addressMVar) >> pure ()) (either throwIO pure)
  --wait for server to be listening
  addr <- takeMVar addressMVar
  let port =
        case addr of
          SockAddrInet port' _ -> fromIntegral port'
          _ -> error "unsupported socket address (IPv4 only currently)"
  --this built-in server is apparently not meant for production use, but it's easier to test than starting up the wai or snap interfaces
  WS.runServer wsHost (fromIntegral wsPort) (websocketProxyServer port serverHost)


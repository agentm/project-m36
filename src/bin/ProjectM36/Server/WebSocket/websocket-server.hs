{-# LANGUAGE CPP #-}

import Control.Concurrent
import Control.Exception
import Control.Monad (when, void)
import Data.Maybe (isJust)
import Data.String (fromString)
import Network.HTTP.Types (status400)
import Network.Socket
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import qualified Network.Wai.Handler.WebSockets as WS
import Network.WebSockets (defaultConnectionOptions)
import ProjectM36.Server
import ProjectM36.Server.Config
import ProjectM36.Server.ParseArgs
import ProjectM36.Server.WebSocket
import ProjectM36.Client (RemoteServerAddress(..))

main :: IO ()
main = do
  -- launch normal project-m36-server
  addressMVar <- newEmptyMVar
  wsConfig <- parseWSConfigWithDefaults (defaultServerConfig {bindAddress = RemoteServerHostAddress "127.0.0.1" 8000})

  --usurp the serverConfig for our websocket server and make the proxied server run locally
  let serverConfig = wsServerConfig wsConfig
      wsAddress = bindAddress serverConfig
      (wsHost, wsPort) = case wsAddress of
                           RemoteServerHostAddress host port -> (host, port)
                           _ -> error "expected host-based address"
      serverHost = "127.0.0.1"
      serverConfig' = serverConfig {bindAddress = RemoteServerHostAddress serverHost 0}
      configCertificateFile = tlsCertificatePath wsConfig
      configKeyFile = tlsKeyPath wsConfig

  when (isJust configCertificateFile /= isJust configKeyFile) $
    throwIO $ ErrorCall "TLS_CERTIFICATE_PATH and TLS_KEY_PATH must be set in tandem"

  _ <- forkFinally (void (launchServer serverConfig' (Just addressMVar))) (either throwIO pure)
  --wait for server to be listening
  addr <- takeMVar addressMVar
  let port =
        case addr of
          SockAddrInet port' _ -> fromIntegral port'
          _ -> error "unsupported socket address (IPv4 only currently)"
      wsApp = websocketProxyServer port serverHost
      waiApp = WS.websocketsOr defaultConnectionOptions wsApp backupApp
      settings = warpSettings wsHost (fromIntegral wsPort)

  case (configCertificateFile, configKeyFile) of
    (Just certificate, Just key) -> runTLS (tlsSettings certificate key) settings waiApp
    _ -> runSettings settings waiApp

backupApp :: Application
backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

warpSettings :: HostName -> Port -> Settings
warpSettings host port =
  setHost (fromString host)
    . setPort port
    . setServerName "project-m36"
    . setTimeout 3600
    . setGracefulShutdownTimeout (Just 5)
    $ defaultSettings

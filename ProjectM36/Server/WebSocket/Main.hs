import ProjectM36.Server.WebSocket

main :: IO ()
main = do
  -- launch normal project-m36-server
  portMVar <- newEmptyMVar
  serverConfig <- parseConfig
  let serverHost = bindHost serverConfig
  _ <- forkFinally (launchServer serverConfig (Just portMVar)) failureHandler
  --wait for server to be listening
  serverPort <- takeMVar portMVar
  --this built-in server is apparently not meant for production use, but it's easier to test than starting up the wai or snap interfaces
  WS.runServer "0.0.0.0" 8888 (websocketProxyServer serverPort serverHost)


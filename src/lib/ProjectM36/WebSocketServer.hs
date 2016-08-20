{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.WebSocketServer where
import qualified Network.WebSockets as WS
import Control.Monad

--add json derivation to RemoteCallTypes
runServer :: WS.Connection -> IO ()
runServer conn = forever $ handleRequest

handleRequest :: WS.Connection -> IO ()
handleRequest conn = do
  dat <- WS.receiveData conn
  
  

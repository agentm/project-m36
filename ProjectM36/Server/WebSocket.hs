{-# LANGUAGE OverloadedStrings #-}
--module ProjectM36.Server.WebSocket where

import Control.Monad (forever)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import ProjectM36.Server.RemoteCallTypes.Json

main :: IO ()
main = do
  --this built-in server is apparently not meant for production use, but it's easier to test than starting up the wai or snap interfaces
  WS.runServer "0.0.0.0" 8888 application
    
application :: WS.ServerApp
application pending = do    
  conn <- WS.acceptRequest pending
  forever $ do
    msg <- (WS.receiveData conn) :: IO T.Text
    case msg of
      "hello" -> WS.sendTextData conn ("nice" :: T.Text)
      "bye" -> WS.sendClose conn ("bye" :: T.Text)
      str -> WS.sendTextData conn str
    return ()
       
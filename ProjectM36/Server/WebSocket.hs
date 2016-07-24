{-# LANGUAGE OverloadedStrings #-}
--module ProjectM36.Server.WebSocket where

import Control.Monad (forever)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import ProjectM36.Server.RemoteCallTypes.Json ()
import ProjectM36.Base
import qualified Data.Set as S
import Data.Aeson

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
      "db" -> WS.sendTextData conn (encode (Project (AttributeNames (S.fromList ["spam1", "spam2"])) (RelationVariable "elgringo")))
      str -> WS.sendTextData conn str
    return ()
       
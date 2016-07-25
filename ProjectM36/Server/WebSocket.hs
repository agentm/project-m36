{-# LANGUAGE OverloadedStrings #-}
--module ProjectM36.Server.WebSocket where
-- while the tutd client performs TutorialD parsing on the client, the websocket server will pass tutd to be parsed and executed on the server- otherwise I have to pull in ghcjs as a dependency to allow client-side parsing- that's not appealing because then the frontend is not language-agnostic, but this could change in the future, perhaps by sending different messages over the websocket
-- ideally, the wire protocol should not be exposed to a straight string-based API ala SQL, so we could make perhaps a javascript DSL which compiles to the necessary JSON- anaylyze tradeoffs
import Control.Monad (forever)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import ProjectM36.Server.RemoteCallTypes.Json ()
import ProjectM36.Client.Json ()
import ProjectM36.Base
import Data.Aeson
import TutorialD.Interpreter
import TutorialD.Interpreter.Base
import ProjectM36.Client

main :: IO ()
main = do
  --this built-in server is apparently not meant for production use, but it's easier to test than starting up the wai or snap interfaces
  WS.runServer "0.0.0.0" 8888 application
    
application :: WS.ServerApp
application pending = do    
  conn <- WS.acceptRequest pending
  (sessionId, dbconn) <- createConnection conn
  forever $ do
    msg <- (WS.receiveData conn) :: IO T.Text
    let tutdprefix = "executetutd:"
    case msg of
      _ | tutdprefix `T.isPrefixOf` msg -> do
        let tutdString = T.drop (T.length tutdprefix) msg
        case parseTutorialD (T.unpack tutdString) of
          Left err -> handleOpResult conn (DisplayErrorResult ("parse error: " `T.append` T.pack (show err)))
          Right parsed -> do
            result <- evalTutorialD sessionId dbconn parsed
            handleOpResult conn result
      _ -> WS.sendTextData conn ("message not expected" :: T.Text)
    return ()
    
notificationCallback :: WS.Connection -> NotificationCallback    
notificationCallback conn notifName evaldNotif = WS.sendTextData conn (encode (object ["notificationname" .= notifName,
                                                                                       "evaldnotification" .= evaldNotif
                                        ]))
    
--this creates a new database for each connection- perhaps not what we want (?)
createConnection :: WS.Connection -> IO (SessionId, Connection)
createConnection wsconn = do
  eConn <- connectProjectM36 (InProcessConnectionInfo NoPersistence (notificationCallback wsconn))
  case eConn of
    Left err -> error $ "failed to create database" ++ show err
    Right conn -> do
      eSessionId <- createSessionAtHead "master" conn
      case eSessionId of
        Left err -> error $ "failed to create connection on master branch" ++ show err
        Right sessionId -> pure (sessionId, conn)
       
handleOpResult :: WS.Connection -> TutorialDOperatorResult -> IO ()
handleOpResult conn QuitResult = WS.sendClose conn ("close" :: T.Text)
handleOpResult conn (DisplayResult out) = WS.sendTextData conn (encode (object ["display" .= out]))
handleOpResult _ (DisplayIOResult ioout) = ioout
handleOpResult conn (DisplayErrorResult err) = WS.sendTextData conn (encode (object ["displayerror" .= err]))
handleOpResult conn QuietSuccessResult = WS.sendTextData conn (encode (object ["acknowledged" .= True]))
handleOpResult conn (DisplayRelationResult rel) = WS.sendTextData conn (encode (object ["displayrelation" .= rel]))

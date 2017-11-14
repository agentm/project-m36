module ProjectM36.Server.WebSocket where
-- while the tutd client performs TutorialD parsing on the client, the websocket server will pass tutd to be parsed and executed on the server- otherwise I have to pull in ghcjs as a dependency to allow client-side parsing- that's not appealing because then the frontend is not language-agnostic, but this could change in the future, perhaps by sending different messages over the websocket
-- ideally, the wire protocol should not be exposed to a straight string-based API ala SQL, so we could make perhaps a javascript DSL which compiles to the necessary JSON- anaylyze tradeoffs

-- launch the project-m36-server
-- proxy all connections to it through ProjectM36.Client
import Control.Monad (forever)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import ProjectM36.Server.RemoteCallTypes.Json ()
import ProjectM36.Client.Json ()
import ProjectM36.Relation.Show.Term
import ProjectM36.Relation.Show.HTML
import Data.Aeson
import TutorialD.Interpreter
import TutorialD.Interpreter.Base
import ProjectM36.Client
import Control.Exception
import Data.Attoparsec.Text
import Control.Applicative
import Text.Megaparsec.Error

websocketProxyServer :: Port -> Hostname -> WS.ServerApp
websocketProxyServer port host pending = do    
  conn <- WS.acceptRequest pending
  let unexpectedMsg = WS.sendTextData conn ("messagenotexpected" :: T.Text)
  --phase 1- accept database name for connection
  dbmsg <- WS.receiveData conn :: IO T.Text
  let connectdbmsg = "connectdb:"
  if not (connectdbmsg `T.isPrefixOf` dbmsg) then unexpectedMsg >> WS.sendClose conn ("" :: T.Text)
    else do
        let dbname = T.unpack $ T.drop (T.length connectdbmsg) dbmsg
        bracket (createConnection conn dbname port host) 
          (\eDBconn -> case eDBconn of
                            Right dbconn -> close dbconn
                            Left _ -> pure ()) $ \eDBconn -> 
          case eDBconn of
            Left err -> sendError conn err
            Right dbconn -> do
                eSessionId <- createSessionAtHead dbconn "master"
                case eSessionId of
                  Left err -> sendError conn err
                  Right sessionId -> do
                    --phase 2- accept tutoriald commands
                    _ <- forever $ do
                      pInfo <- promptInfo sessionId dbconn
                      --figure out why sending three times during startup is necessary
                      sendPromptInfo pInfo conn                
                      sendPromptInfo pInfo conn
                      msg <- WS.receiveData conn :: IO T.Text
                      case parseOnly parseExecuteMessage msg of
                        Left _ -> unexpectedMsg
                        Right (presentation, tutdString) ->
                          case parseTutorialD tutdString of
                            Left err -> handleOpResult conn dbconn presentation (DisplayErrorResult ("parse error: " `T.append` T.pack (parseErrorPretty err)))
                            Right parsed -> do
                              let timeoutFilter exc = if exc == RequestTimeoutException 
                                                          then Just exc 
                                                          else Nothing
                                  responseHandler = do
                                    result <- evalTutorialD sessionId dbconn SafeEvaluation parsed
                                    pInfo' <- promptInfo sessionId dbconn
                                    sendPromptInfo pInfo' conn                       
                                    handleOpResult conn dbconn presentation result
                              catchJust timeoutFilter responseHandler (\_ -> handleOpResult conn dbconn presentation (DisplayErrorResult "Request Timed Out."))
                    pure ()
    
notificationCallback :: WS.Connection -> NotificationCallback    
notificationCallback conn notifName evaldNotif = WS.sendTextData conn (encode (object ["notificationname" .= notifName,
                                                                                       "evaldnotification" .= evaldNotif
                                        ]))
    
--this creates a new database for each connection- perhaps not what we want (?)
createConnection :: WS.Connection -> DatabaseName -> Port -> Hostname -> IO (Either ConnectionError Connection)
createConnection wsconn dbname port host = connectProjectM36 (RemoteProcessConnectionInfo dbname (createNodeId host port) (notificationCallback wsconn))

sendError :: (ToJSON a) => WS.Connection -> a -> IO ()
sendError conn err = WS.sendTextData conn (encode (object ["displayerror" .= err]))

handleOpResult :: WS.Connection -> Connection -> Presentation -> TutorialDOperatorResult -> IO ()
handleOpResult conn db _ QuitResult = WS.sendClose conn ("close" :: T.Text) >> close db
handleOpResult conn  _ _ (DisplayResult out) = WS.sendTextData conn (encode (object ["display" .= out]))
handleOpResult _ _ _ (DisplayIOResult ioout) = ioout
handleOpResult conn _ presentation (DisplayErrorResult err) = do
  let jsono = ["json" .= err | jsonPresentation presentation]
      texto = ["text" .= err | textPresentation presentation]
      htmlo = ["html" .= err | htmlPresentation presentation]
  WS.sendTextData conn (encode (object ["displayerror" .= object (jsono ++ texto ++ htmlo)]))
handleOpResult conn _ _ (DisplayParseErrorResult _ err) = WS.sendTextData conn (encode (object ["displayparseerrorresult" .= show err]))
handleOpResult conn _ _ QuietSuccessResult = WS.sendTextData conn (encode (object ["acknowledged" .= True]))
handleOpResult conn _ presentation (DisplayRelationResult rel) = do
  let jsono = ["json" .= rel | jsonPresentation presentation]
      texto = ["text" .= showRelation rel | textPresentation presentation]
      htmlo = ["html" .= relationAsHTML rel | htmlPresentation presentation]
  WS.sendTextData conn (encode (object ["displayrelation" .= object (jsono ++ texto ++ htmlo)]))
-- get current schema and head name for client
promptInfo :: SessionId -> Connection -> IO (HeadName, SchemaName)
promptInfo sessionId conn = do
  eHeadName <- headName sessionId conn  
  eSchemaName <- currentSchemaName sessionId conn
  pure (either (const "<unknown>") id eHeadName, either (const "<no schema>") id eSchemaName)
  
sendPromptInfo :: (HeadName, SchemaName) -> WS.Connection -> IO ()
sendPromptInfo (hName, sName) conn = WS.sendTextData conn (encode (object ["promptInfo" .= object ["headname" .= hName, "schemaname" .= sName]]))

--a returning relation can be returned as JSON, Text (for consoles), or HTML
data Presentation = Presentation {
  jsonPresentation :: Bool, 
  textPresentation :: Bool,
  htmlPresentation :: Bool }
                    
data PresentationFlag = JSONFlag | TextFlag | HTMLFlag
 
parseExecuteMessage :: Parser (Presentation, T.Text)
parseExecuteMessage = do
  _ <- string "executetutd/"
  flags <- sepBy ((string "json" *> pure JSONFlag) <|>
                  (string "text" *> pure TextFlag) <|>
                  (string "html" *> pure HTMLFlag)) "+"
  let presentation = foldr (\flag acc -> case flag of 
                               JSONFlag -> acc {jsonPresentation = True}
                               TextFlag -> acc {textPresentation = True}
                               HTMLFlag -> acc {htmlPresentation = True}) (Presentation False False False) flags
  _ <- char ':'
  tutd <- T.pack <$> manyTill anyChar endOfInput
  pure (presentation, tutd)
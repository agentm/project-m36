{-# LANGUAGE LambdaCase, CPP #-}
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
import ProjectM36.DataFrame
import ProjectM36.Relation.Show.Term
import ProjectM36.Relation.Show.HTML
import Data.Aeson
import TutorialD.Interpreter
import ProjectM36.Interpreter (ConsoleResult(..), SafeEvaluationFlag(..))
import ProjectM36.Client
import Control.Exception
import Data.Attoparsec.Text as Atto
import Control.Applicative
import Text.Megaparsec.Error
import Data.Functor
import Data.Either (fromRight)
import qualified Data.UUID as UUID

#if MIN_VERSION_megaparsec(7,0,0)
import Data.List.NonEmpty as NE
#endif

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
          (\case
              Right dbconn -> close dbconn
              Left _ -> pure ()) $ \case
            Left err -> sendError conn err
            Right dbconn -> do
              --phase 2- accept tutoriald commands
              _ <- forever $ do
                {-pInfo <- promptInfo sessionId dbconn
                --figure out why sending three times during startup is necessary
                sendPromptInfo pInfo conn
                sendPromptInfo pInfo conn-}
                msg <- WS.receiveData conn :: IO T.Text
                case parseOnly parseIncomingRequest msg of
                        Left _ -> unexpectedMsg
                        Right (CreateSessionAtHeadRequest branchName) -> do
                          ret <- createSessionAtHead dbconn branchName
                          case ret of
                            Left err -> handleOpResult conn dbconn jsonOnlyPresentation (DisplayErrorResult ("createSessionAtHead error: " <> T.pack (show err)))
                                                                                     
                        Right (ExecuteTutorialDRequest sessionId presentation tutdString) ->
                          case parseTutorialD tutdString of
                            Left err -> handleOpResult conn dbconn presentation
#if MIN_VERSION_megaparsec(7,0,0)
                              (DisplayErrorResult
                                ("parse error: " `T.append` T.pack
                                  (parseErrorPretty . NE.head . bundleErrors $ err)))
#else
                              (DisplayErrorResult ("parse error: " `T.append` T.pack (parseErrorPretty err)))
#endif
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
createConnection wsconn dbname port host = connectProjectM36 (RemoteConnectionInfo dbname host (show port) (notificationCallback wsconn))

sendError :: (ToJSON a) => WS.Connection -> a -> IO ()
sendError conn err = WS.sendTextData conn (encode (object ["displayerror" .= err]))

handleOpResult :: WS.Connection -> Connection -> Presentation -> ConsoleResult -> IO ()
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
handleOpResult conn _ presentation (DisplayDataFrameResult df) = do
  let jsono = ["json" .= df | jsonPresentation presentation]
      texto = ["text" .= showDataFrame df | textPresentation presentation]
      htmlo = ["html" .= dataFrameAsHTML df | htmlPresentation presentation]
  WS.sendTextData conn (encode (object ["displaydataframe" .= object (jsono ++ texto ++ htmlo)]))
handleOpResult conn _ _ (DisplayRelationalErrorResult relErr) =
  WS.sendTextData conn (encode (object ["displayrelationalerrorresult" .= relErr]))
handleOpResult conn dbconn presentation (DisplayHintWith txt conResult) = do
  -- we should wrap this up into one response instead of two responses for clarity
  WS.sendTextData conn (encode (object ["hint" .= txt]))
  handleOpResult conn dbconn presentation conResult


-- get current schema and head name for client
promptInfo :: SessionId -> Connection -> IO (HeadName, SchemaName)
promptInfo sessionId conn = do
  eHeadName <- headName sessionId conn
  eSchemaName <- currentSchemaName sessionId conn
  pure (fromRight "<unknown>" eHeadName, fromRight "<no schema>" eSchemaName)

sendPromptInfo :: (HeadName, SchemaName) -> WS.Connection -> IO ()
sendPromptInfo (hName, sName) conn = WS.sendTextData conn (encode (object ["promptInfo" .= object ["headname" .= hName, "schemaname" .= sName]]))

--a returning relation can be returned as JSON, Text (for consoles), or HTML
data Presentation = Presentation {
  jsonPresentation :: Bool,
  textPresentation :: Bool,
  htmlPresentation :: Bool }

jsonOnlyPresentation :: Presentation
jsonOnlyPresentation = Presentation { jsonPresentation = True,
                                  textPresentation = False,
                                  htmlPresentation = False }
                   
data PresentationFlag = JSONFlag | TextFlag | HTMLFlag

data IncomingRequest = ExecuteTutorialDRequest SessionId Presentation T.Text |
                       CreateSessionAtHeadRequest HeadName

parseIncomingRequest :: Parser IncomingRequest
parseIncomingRequest = parseExecuteTutorialDMessage <|>
                       parseCreateSessionAtHead

parseExecuteTutorialDMessage :: Parser IncomingRequest
parseExecuteTutorialDMessage = do
  _ <- string "executetutd/"
  flags <- sepBy ((string "json" $> JSONFlag) <|>
                  (string "text" $> TextFlag) <|>
                  (string "html" $> HTMLFlag)) "+"
  let presentation = foldr (\flag acc -> case flag of
                               JSONFlag -> acc {jsonPresentation = True}
                               TextFlag -> acc {textPresentation = True}
                               HTMLFlag -> acc {htmlPresentation = True}) (Presentation False False False) flags
  _ <- char ':'
  sessionId <- sessionIdP
  _ <- char ':'
  tutd <- takeText
  pure (ExecuteTutorialDRequest sessionId presentation tutd)

parseCreateSessionAtHead :: Parser IncomingRequest
parseCreateSessionAtHead = do
  _ <- string "createSessionAtHead:"
  CreateSessionAtHeadRequest <$> takeText

textToEOFP :: Parser T.Text
textToEOFP = T.pack <$> manyTill anyChar endOfInput

sessionIdP :: Parser SessionId
sessionIdP = do
  let hexDigitP = satisfy isHexDigit
      nHexDigitsP n = T.pack <$> Atto.count n hexDigitP
      isHexDigit c = (c >= '0' && c <= '9') ||
                   (c >= 'a' && c <= 'f') ||
                   (c >= 'A' && c <= 'F')
  a <- nHexDigitsP 8
  _ <- char '-'
  b <- nHexDigitsP 4
  _ <- char '-'
  c <- nHexDigitsP 4
  _ <- char '-'
  d <- nHexDigitsP 12
  case UUID.fromText (T.concat [a,b,c,d]) of
    Nothing -> fail "invalid UUID"
    Just uuid -> pure uuid

{-# LANGUAGE LambdaCase, CPP, GeneralizedNewtypeDeriving, DeriveAnyClass, DeriveGeneric, DerivingStrategies, ScopedTypeVariables #-}
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
import ProjectM36.Base
import TutorialD.Interpreter
import ProjectM36.Interpreter (ConsoleResult(..), SafeEvaluationFlag(..))
import ProjectM36.Client
import Control.Exception
import Text.Megaparsec.Error
import Data.Either (fromRight)
import qualified Data.ByteString.Lazy as BS
import qualified Data.UUID as UUID
import GHC.Generics
import qualified Data.Text.Encoding as TE

#if MIN_VERSION_megaparsec(7,0,0)
import Data.List.NonEmpty as NE
#endif

websocketProxyServer :: Port -> Hostname -> WS.ServerApp
websocketProxyServer port host pending = do
  conn <- WS.acceptRequest pending
  let unexpectedMsg expecting received = do
        print (expecting, received)
        sendResponse conn (MessageNotExpected expecting)
  --phase 1- accept database name for connection
  dbmsg <- WS.receiveData conn :: IO BS.ByteString
  case eitherDecode dbmsg of
    Left _ -> do
      unexpectedMsg "ConnectionSetupRequest" dbmsg
      WS.sendClose conn ("" :: T.Text)
    Right (ConnectionSetupRequest dbname) -> do
        bracket (createConnection conn dbname port host)
          (\case
              Right dbconn -> close dbconn
              Left _ -> pure ()) $ \case
            Left err -> sendResponse conn (TextErrorResponse (RequestId UUID.nil) (T.pack (show err)))
            Right dbconn -> do
              sendResponse conn ConnectionSetupResponse
              --phase 2- accept tutoriald commands
              _ <- forever $ do
                {-pInfo <- promptInfo sessionId dbconn
                --figure out why sending three times during startup is necessary
                sendPromptInfo pInfo conn
                sendPromptInfo pInfo conn-}
                msg <- WS.receiveData conn :: IO BS.ByteString
                case eitherDecode msg of
                        Left err -> unexpectedMsg "Request" err
                        Right (CreateSessionAtHeadRequest reqId branchName) -> do
                          ret <- createSessionAtHead dbconn branchName
                          case ret of
                            Left err -> sendResponse conn (RelationalErrorResponse reqId err)
                            Right sessionId -> sendResponse conn (CreateSessionAtHeadResponse reqId sessionId) 
                        Right (ExecuteTutorialDRequest reqId sessionId presentation (TutorialDText tutdString)) ->
                          case parseTutorialD tutdString of
                            Left err -> do
                              let parseErr = ParseError $ T.pack
                                    (parseErrorPretty . NE.head . bundleErrors $ err)

                              sendResponse conn (RelationalErrorResponse reqId parseErr)
                            Right parsed -> do
                              let timeoutFilter exc = if exc == RequestTimeoutException
                                                          then Just exc
                                                          else Nothing
                                  responseHandler = do
                                    result <- evalTutorialD sessionId dbconn SafeEvaluation parsed
                                    (headName', schemaName) <- promptInfo sessionId dbconn
                                    sendResponse conn (PromptInfoResponse sessionId headName' schemaName)
                                    let resp = makeResponse reqId presentation result
                                    handleResponse conn dbconn resp
                              catchJust timeoutFilter responseHandler (\_ ->
                                                                         sendResponse conn (TimeoutResponse reqId))
              pure ()

        
notificationCallback :: WS.Connection -> NotificationCallback
notificationCallback conn notifName evaldNotif =
  sendResponse conn (NotificationResponse notifName evaldNotif)

--this creates a new database for each connection- perhaps not what we want (?)
createConnection :: WS.Connection -> DatabaseName -> Port -> Hostname -> IO (Either ConnectionError Connection)
createConnection wsconn dbname port host = connectProjectM36 (RemoteConnectionInfo dbname (RemoteServerHostAddress host port) (notificationCallback wsconn))

handleResponse :: WS.Connection -> Connection -> Response -> IO ()
handleResponse conn dbconn resp =
  case resp of
    ConnectionClosedResponse -> do
      close dbconn 
      sendResp
    DisplayTextResponse{} -> sendResp
    RelationResponse{} -> sendResp
    DataFrameResponse{} -> sendResp
    CreateSessionAtHeadResponse{} -> sendResp
    SuccessResponse{} -> sendResp
    TimeoutResponse{} -> sendResp
    PromptInfoResponse{} -> sendResp
    RelationalErrorResponse{} -> sendResp
    TextErrorResponse{} -> sendResp
    HintWithResponse{} -> sendResp
    ConnectionSetupResponse{} -> sendResp
    NotificationResponse{} -> sendResp
    MessageNotExpected{} -> sendResp
    where
      sendResp = sendResponse conn resp
    
makeResponse :: RequestId -> Presentation -> ConsoleResult -> Response
makeResponse reqId presentation consoleResult =
  case consoleResult of
    QuitResult -> do
      ConnectionClosedResponse
    DisplayResult out ->
      DisplayTextResponse reqId out
    DisplayIOResult _ ->
      -- we can't send it over the websocket, so just ignore it- in other context, this just used to launch plots       
      SuccessResponse reqId
    DisplayErrorResult err ->
      TextErrorResponse reqId err
    DisplayParseErrorResult _ err -> 
      let err' = ParseError $ T.pack (parseErrorPretty . NE.head . bundleErrors $ err) in
        RelationalErrorResponse reqId err'
    QuietSuccessResult ->
      SuccessResponse reqId
    DisplayRelationResult rel ->
      RelationResponse reqId rel presentation
    DisplayDataFrameResult df -> 
      DataFrameResponse reqId df presentation
    DisplayRelationalErrorResult relErr ->
      RelationalErrorResponse reqId relErr
    DisplayHintWith txt conResult ->
      HintWithResponse reqId txt (makeResponse reqId presentation conResult)
  
sendResponse :: WS.Connection -> Response -> IO ()
sendResponse conn response =
  -- this could be optimized to work with a lazy bytestring
  WS.sendTextData conn (TE.decodeUtf8 (BS.toStrict (encode response)))

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
  deriving (Generic, ToJSON, FromJSON)


jsonOnlyPresentation :: Presentation
jsonOnlyPresentation = Presentation { jsonPresentation = True,
                                  textPresentation = False,
                                  htmlPresentation = False }
                   
data PresentationFlag = JSONFlag | TextFlag | HTMLFlag
  deriving (Generic, ToJSON, FromJSON)

newtype RequestId = RequestId UUID.UUID
  deriving newtype (ToJSON, FromJSON, Eq, Show)

newtype TutorialDText = TutorialDText T.Text
  deriving newtype (ToJSON, FromJSON)

newtype ConnectionSetupRequest = ConnectionSetupRequest DatabaseName

instance FromJSON ConnectionSetupRequest where
  parseJSON = withObject "ConnectionSetupRequest" $ \o -> do
    tag::T.Text <- o .: "tag"
    case tag of
      "ConnectionSetupRequest" ->
        ConnectionSetupRequest <$> o .: "databaseName"
      other -> fail ("bad tag: " <> show other)

instance ToJSON ConnectionSetupRequest where
  toJSON (ConnectionSetupRequest dbName) =
    object [ "tag" .= ("ConnectionSetupRequest"::T.Text),
             "databaseName" .= dbName ]

data Request = ExecuteTutorialDRequest RequestId SessionId Presentation TutorialDText |
               CreateSessionAtHeadRequest RequestId HeadName

data Response = RelationResponse RequestId Relation Presentation |
                DataFrameResponse RequestId DataFrame Presentation |
                CreateSessionAtHeadResponse RequestId SessionId |
                SuccessResponse RequestId |
                TimeoutResponse RequestId |
                PromptInfoResponse SessionId HeadName SchemaName |
                RelationalErrorResponse RequestId RelationalError |
                TextErrorResponse RequestId T.Text |
                ConnectionClosedResponse |
                ConnectionSetupResponse |
                DisplayTextResponse RequestId T.Text |
                HintWithResponse RequestId T.Text Response |
                NotificationResponse NotificationName EvaluatedNotification |
                MessageNotExpected T.Text

instance ToJSON Request where
  toJSON (ExecuteTutorialDRequest reqId sessionId presentation tutd) =
    object [ "tag" .= ("ExecuteTutorialDRequest"::T.Text),
             "requestId" .= reqId,
             "sessionId" .= sessionId,
             "presentation" .= presentation,
             "tutoriald" .= tutd ]
  toJSON (CreateSessionAtHeadRequest reqId headName') =
    object [ "tag" .= ("CreateSessionAtHeadRequest"::T.Text),
             "requestId" .= reqId,
             "headName" .= headName' ]

instance FromJSON Request where
  parseJSON = withObject "ExecuteTutorialDRequest" $ \o -> do
    tag::T.Text <- o .: "tag"
    case tag of
      "ExecuteTutorialDRequest" ->
        ExecuteTutorialDRequest <$> o .: "requestId"
                                <*> o .: "sessionId"
                                <*> o .: "presentation"
                                <*> o .: "tutoriald"
      "CreateSessionAtHeadRequest" ->
        CreateSessionAtHeadRequest <$> o .: "requestId"
                                   <*> o .: "headName"
      other -> fail ("bad tag: " <> show other)

-- this is useful for Haskell clients to decode JSON messages
instance FromJSON Response where
  parseJSON = withObject "Response" $ \o -> do
    tag::T.Text <- o .: "tag"
    case tag of
      "RelationResponse" -> do
        mJsonRel <- o .: "jsonRelation"
        case mJsonRel of
          Nothing -> fail "missing json relation"
          Just jsonRel ->
            RelationResponse <$>  o .: "requestId" <*> pure jsonRel <*> pure jsonOnlyPresentation
      "DataFrameResponse" -> do
        mJsonDF <- o .: "jsonDataFrame"
        case mJsonDF of
          Nothing -> fail "missing json dataframe"
          Just jsonDF ->
            DataFrameResponse <$> o .: "requestId" <*> pure jsonDF <*> pure jsonOnlyPresentation
      "CreateSessionAtHeadResponse" ->
        CreateSessionAtHeadResponse <$> o .: "requestId" <*> o .: "sessionId"
      "SuccessResponse" ->
        SuccessResponse <$> o .: "requestId"
      "TimeoutResponse" ->
        TimeoutResponse <$> o .: "requestId"
      "PromptInfoResponse" ->
        PromptInfoResponse <$> o .: "sessionId" <*> o .: "headName" <*> o .: "schemaName"
      "RelationalErrorResponse" ->
        RelationalErrorResponse <$> o .: "requestId" <*> o .: "error"
      "TextErrorResponse" ->
        TextErrorResponse <$> o .: "requestId" <*> o .: "error"
      "ConnectionClosedResponse" ->
        pure ConnectionClosedResponse
      "DisplayTextResponse" ->
        DisplayTextResponse <$> o .: "requestId" <*> o .: "text"
      "HintWithResponse" ->
        HintWithResponse <$> o .: "requestId" <*> o .: "hintText" <*> o .: "response"
      "ConnectionSetupResponse" ->
        pure ConnectionSetupResponse
      "NotificationResponse" ->
        NotificationResponse <$> o .: "notificationName" <*> o .: "evaluatedNotification"
      "MessageNotExpected" ->
        MessageNotExpected <$> o .: "expected"
      other ->
        fail ("unexpected tag: " <> T.unpack other)

instance ToJSON Response where
  toJSON (RelationResponse reqId rel presentation) =
    object [ "tag" .= ("RelationResponse"::T.Text),
             "requestId" .= reqId,
             "jsonRelation" .= if jsonPresentation presentation then
               Just rel else Nothing,
             "textRelation" .= if textPresentation presentation then
               Just (showRelation rel) else Nothing,
             "htmlRelation" .= if htmlPresentation presentation then
               Just (relationAsHTML rel) else Nothing
             
           ]
  toJSON (DataFrameResponse reqId df presentation) =
    object [ "tag" .= ("DataFrameResponse"::T.Text),
             "requestId" .= reqId,
             "jsonDataFrame" .= if jsonPresentation presentation then
               Just df else Nothing,
             "textDataFrame" .= if textPresentation presentation then
               Just (showDataFrame df) else Nothing,
             "htmlDataFrame" .= if htmlPresentation presentation then
               Just (dataFrameAsHTML df) else Nothing
             ]
  toJSON (CreateSessionAtHeadResponse reqId sessionId) =
    object [ "tag" .= ("CreateSessionAtHeadResponse"::T.Text),
             "requestId" .= reqId,
             "sessionId" .= sessionId ]
  toJSON (SuccessResponse reqId) =
    object [ "tag" .= ("SuccessResponse"::T.Text),
             "requestId" .= reqId ]
  toJSON (TimeoutResponse reqId) = 
    object [ "tag" .= ("TimeoutResponse"::T.Text),
             "requestId" .= reqId ]
  toJSON (PromptInfoResponse sessionId headName' schemaName) =
    object [ "tag" .= ("PromptInfoResponse"::T.Text),
             "sessionId" .= sessionId,
             "headName" .= headName',
             "schemaName" .= schemaName ]
  toJSON (RelationalErrorResponse reqId relErr) =
    object [ "tag" .= ("RelationalErrorResponse"::T.Text),
             "requestId" .= reqId,
             "error" .= relErr ]
  toJSON (TextErrorResponse reqId err) =
    object [ "tag" .= ("TextErrorResponse"::T.Text),
             "requestId" .= reqId,
             "error" .= err ]
  toJSON ConnectionClosedResponse =
    object [ "tag" .= ("ConnectionClosedResponse"::T.Text) ]
  toJSON (DisplayTextResponse reqId txt) =
    object [ "tag" .= ("DisplayTextResponse"::T.Text),
             "requestId" .= reqId,
             "text" .= txt ]
  toJSON (HintWithResponse reqId hintTxt resp) =
    object [ "tag" .= ("HintWithResponse"::T.Text),
             "requestId" .= reqId,
             "hintText" .= hintTxt,
             "response" .= resp ]
  toJSON ConnectionSetupResponse =
    object [ "tag" .= ("ConnectionSetupResponse"::T.Text),
             "status" .= ("ready"::T.Text) ]
  toJSON (NotificationResponse notificationName evaldNotification) =
    object [ "tag" .= ("NotificationResponse"::T.Text),
             "notificationName" .= notificationName,
             "evaluatedNotification" .= evaldNotification ]
  toJSON (MessageNotExpected expected) =
    object [ "tag" .= ("MessageNotExpected"::T.Text),
             "expected" .= expected ]

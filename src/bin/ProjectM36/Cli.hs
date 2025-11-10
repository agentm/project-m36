{-# LANGUAGE LambdaCase #-}
-- functions common to both tutd and sqlegacy command line interfaces
module ProjectM36.Cli where
import qualified ProjectM36.Client as C
import qualified Data.Text as T
import ProjectM36.Base
import ProjectM36.DatabaseContext.Types
import ProjectM36.IsomorphicSchema.Types
import ProjectM36.Client (RemoteServerAddress(..))
import System.Console.Haskeline
import Control.Exception
import System.IO
import ProjectM36.Relation.Show.Term -- probably want to display dataframes instead
import ProjectM36.Error
import Options.Applicative 
import ProjectM36.Server.ParseArgs
import ProjectM36.Server (checkFSType, checkFSErrorMsg)
import Data.Maybe (fromMaybe)
import GHC.IO.Encoding
import Control.Monad (when)
import System.Exit
import Text.Megaparsec.Error
import Data.Void (Void)
import ProjectM36.Interpreter hiding (Parser)
import Network.RPC.Curryer.Client

type GhcPkgPath = String
type TutorialDExec = String
type CheckFS = Bool

type DirectExecute = String
type ParserError = ParseErrorBundle T.Text Void

data InterpreterConfig = LocalInterpreterConfig PersistenceStrategy HeadName (Maybe DirectExecute) [GhcPkgPath] CheckFS RoleName |
                         RemoteInterpreterConfig RemoteServerAddress ClientConnectionConfig C.DatabaseName HeadName (Maybe TutorialDExec) CheckFS RoleName

outputNotificationCallback :: C.NotificationCallback
outputNotificationCallback notName evaldNot = hPutStrLn stderr $ "Notification received " ++ show notName ++ ":\n" ++ "\n" ++ prettyEvaluatedNotification evaldNot

prettyEvaluatedNotification :: C.EvaluatedNotification -> String
prettyEvaluatedNotification eNotif = let eRelShow eRel = case eRel of
                                           Left err -> show err
                                           Right reportRel -> T.unpack (showRelation reportRel) in
  eRelShow (C.reportOldRelation eNotif) <> "\n" <> eRelShow (C.reportNewRelation eNotif)

type ReprLoopEvaluator = C.SessionId -> C.Connection -> Maybe PromptLength -> T.Text -> IO ()
type MakePrompt = Either RelationalError C.CurrentHead -> Either RelationalError SchemaName -> StringType
type HistoryFilePath = FilePath
  
reprLoop :: InterpreterConfig -> HistoryFilePath -> ReprLoopEvaluator -> MakePrompt -> C.SessionId -> C.Connection -> IO ()
reprLoop config historyFilePath reprLoopEvaluator promptText sessionId conn = do
  let settings = defaultSettings {historyFile = Just historyFilePath} -- (homeDirectory ++ "/.tutd_history")}
  eCurrentHead <- C.currentHead sessionId conn
  eSchemaName <- C.currentSchemaName sessionId conn
  let prompt = promptText eCurrentHead eSchemaName
      catchInterrupt = handleJust (\case
                                      UserInterrupt -> Just Nothing
                                      _ -> Nothing) (\_ -> do
                                                        hPutStrLn stderr "Statement cancelled. Use \":quit\" to exit."
                                                        pure (Just ""))
  maybeLine <- catchInterrupt $ runInputT settings $ getInputLine (T.unpack prompt)
  case maybeLine of
    Nothing -> return ()
    Just line -> do
      reprLoopEvaluator sessionId conn (Just (T.length prompt)) (T.pack line)
      reprLoop config historyFilePath reprLoopEvaluator promptText sessionId conn

parseArgs :: Parser InterpreterConfig
parseArgs = LocalInterpreterConfig <$> parsePersistenceStrategy <*> parseHeadName <*> parseDirectExecute <*> many parseGhcPkgPath <*> parseCheckFS <*> parseRoleName <|>
            RemoteInterpreterConfig <$> parseServerAddress <*> parseClientConnectionConfig <*> parseDatabaseName <*> parseHeadName <*> parseDirectExecute <*> parseCheckFS <*> parseRoleName

parseClientConnectionConfig :: Parser ClientConnectionConfig
parseClientConnectionConfig =
  flag' UnencryptedConnectionConfig (long "disable-tls" <>
                                       help "Disable encryption when connecting to the server.")
  <|>
  EncryptedConnectionConfig <$> parseClientTLSConfig

parseClientTLSConfig :: Parser ClientTLSConfig
parseClientTLSConfig =
  ClientTLSConfig <$>
    parseClientTLSCertInfo <*>
    parseHostname "localhost" <*>
    parseServiceName ""

parseClientTLSCertInfo :: Parser ClientTLSCertInfo
parseClientTLSCertInfo =
  ClientTLSCertInfo <$>
  optional ((,) <$>
              strOption (long "public-x509-key" <>
                         metavar "KEY_PATH" <>
                         help "Enables TLS with path to public key.") <*>
              strOption (long "private-x509-key" <>
                         metavar "KEY_PATH" <>
                         help "Enables TLS with path to private key.")) <*>
  optional (strOption (long "certificate-x509-path" <>
                        metavar "CERT_PATH" <>
                        help "Path to certificate for TLS. Elide to use system's certificate store."))
    
parseHeadName :: Parser HeadName               
parseHeadName = option auto (long "head" <>
                             help "Start session at head name." <>
                             metavar "GRAPH HEAD NAME" <>
                             value "master"
                            )

parseDirectExecute :: Parser (Maybe DirectExecute)
parseDirectExecute = optional $ strOption (long "exec-tutd" <>
                           short 'e' <>
                           metavar "TUTORIALD" <>
                           help "Execute TutorialD expression and exit"
                           )

type PrintWelcome = IO ()
type ExecUserInput = C.SessionId -> C.Connection -> Maybe PromptLength -> T.Text -> IO ()

mainLoop :: IO () -> HistoryFilePath -> ReprLoopEvaluator -> MakePrompt -> ExecUserInput -> ResolvedDatabaseContext -> IO ()
mainLoop printWelcome historyFilePath reprLoopEvaluator promptText execUserInput defaultDBContext = do
  setLocaleIfNecessary
  interpreterConfig <- execParser opts
  let connInfo = connectionInfoForConfig interpreterConfig defaultDBContext
  fscheck <- checkFSType (checkFSForConfig interpreterConfig) (fromMaybe NoPersistence (persistenceStrategyForConfig interpreterConfig))
  if not fscheck then
    errDie checkFSErrorMsg
    else do
    dbconn <- C.connectProjectM36 connInfo
    case dbconn of 
      Left err -> 
        errDie ("Failed to create database connection: " ++ show err)
      Right conn -> do
        let connHeadName = headNameForConfig interpreterConfig
        eSessionId <- C.createSessionAtHead conn connHeadName
        case eSessionId of 
            Left err -> errDie ("Failed to create database session at \"" ++ show connHeadName ++ "\": " ++ show err)
            Right sessionId -> 
              case directExecForConfig interpreterConfig of
                Nothing -> do
                  printWelcome
                  _ <- reprLoop interpreterConfig historyFilePath reprLoopEvaluator promptText sessionId conn
                  pure ()
                Just execStr -> 
                  execUserInput sessionId conn Nothing (T.pack execStr)
  
-- | If the locale is set to ASCII, upgrade it to UTF-8 because tutd outputs UTF-8-encoded attributes. This is especially important in light docker images where the locale data may be missing.
setLocaleIfNecessary :: IO ()
setLocaleIfNecessary = do
  l <- getLocaleEncoding
  when (textEncodingName l == "ASCII") (setLocaleEncoding utf8)

opts :: ParserInfo InterpreterConfig            
opts = info (parseArgs <**> helpOption) idm

connectionInfoForConfig :: InterpreterConfig -> ResolvedDatabaseContext -> C.ConnectionInfo
connectionInfoForConfig (LocalInterpreterConfig pStrategy _ _ ghcPkgPaths _ roleName) defaultDBContext = C.InProcessConnectionInfo pStrategy outputNotificationCallback ghcPkgPaths defaultDBContext roleName
connectionInfoForConfig (RemoteInterpreterConfig remoteAddress connConfig remoteDBName _ _ _ roleName) _ = C.RemoteConnectionInfo remoteDBName remoteAddress connConfig outputNotificationCallback roleName

headNameForConfig :: InterpreterConfig -> HeadName
headNameForConfig (LocalInterpreterConfig _ headn _ _ _ _) = headn
headNameForConfig (RemoteInterpreterConfig _ _ _ headn _ _ _) = headn

directExecForConfig :: InterpreterConfig -> Maybe String
directExecForConfig (LocalInterpreterConfig _ _ t _ _ _) = t
directExecForConfig (RemoteInterpreterConfig _ _ _ _ t _ _) = t

checkFSForConfig :: InterpreterConfig -> Bool
checkFSForConfig (LocalInterpreterConfig _ _ _ _ c _) = c
checkFSForConfig (RemoteInterpreterConfig _ _ _ _ _ c _) = c

persistenceStrategyForConfig :: InterpreterConfig -> Maybe PersistenceStrategy
persistenceStrategyForConfig (LocalInterpreterConfig strat _ _ _ _ _) = Just strat
persistenceStrategyForConfig RemoteInterpreterConfig{} = Nothing
                         
errDie :: String -> IO ()                                                           
errDie err = hPutStrLn stderr err >> exitFailure


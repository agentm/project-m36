{-# LANGUAGE CPP #-}
import TutorialD.Interpreter
import ProjectM36.Base
import ProjectM36.Client
import ProjectM36.Server.ParseArgs
import ProjectM36.Server
import System.IO
import Options.Applicative
import System.Exit
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif
import Data.Maybe
import qualified Data.Text as T

#if !defined(VERSION_project_m36) 
# warning Failed to discover proper version from cabal_macros.h
# define VERSION_project_m36 "<unknown>"
#endif

parseArgs :: Parser InterpreterConfig
parseArgs = LocalInterpreterConfig <$> parsePersistenceStrategy <*> parseHeadName <*> parseTutDExec <*> many parseGhcPkgPath <*> parseCheckFS <|>
            RemoteInterpreterConfig <$> parseNodeId <*> parseDatabaseName <*> parseHeadName <*> parseTutDExec <*> parseCheckFS

parseHeadName :: Parser HeadName               
parseHeadName = option auto (long "head" <>
                             help "Start session at head name." <>
                             metavar "GRAPH HEAD NAME" <>
                             value "master"
                            )

parseNodeId :: Parser NodeId
parseNodeId = createNodeId <$> 
              strOption (long "host" <> 
                         short 'h' <>
                         help "Remote host name" <>
                         metavar "HOSTNAME" <>
                         value "127.0.0.1") <*> 
              option auto (long "port" <>
                           metavar "PORT NUMBER" <>
                           short 'p' <>
                      help "Remote port" <>
                      value defaultServerPort)
              
--just execute some tutd and exit
parseTutDExec :: Parser (Maybe TutorialDExec)
parseTutDExec = optional $ strOption (long "exec-tutd" <>
                           short 'e' <>
                           metavar "TUTORIALD" <>
                           help "Execute TutorialD expression and exit"
                           )

opts :: ParserInfo InterpreterConfig            
opts = info (parseArgs <**> helpOption) idm

connectionInfoForConfig :: InterpreterConfig -> ConnectionInfo
connectionInfoForConfig (LocalInterpreterConfig pStrategy _ _ ghcPkgPaths _) = InProcessConnectionInfo pStrategy outputNotificationCallback ghcPkgPaths
connectionInfoForConfig (RemoteInterpreterConfig remoteNodeId remoteDBName _ _ _) = RemoteProcessConnectionInfo remoteDBName remoteNodeId outputNotificationCallback

headNameForConfig :: InterpreterConfig -> HeadName
headNameForConfig (LocalInterpreterConfig _ headn _ _ _) = headn
headNameForConfig (RemoteInterpreterConfig _ _ headn _ _) = headn

execTutDForConfig :: InterpreterConfig -> Maybe String
execTutDForConfig (LocalInterpreterConfig _ _ t _ _) = t
execTutDForConfig (RemoteInterpreterConfig _ _ _ t _) = t

checkFSForConfig :: InterpreterConfig -> Bool
checkFSForConfig (LocalInterpreterConfig _ _ _ _ c) = c
checkFSForConfig (RemoteInterpreterConfig _ _ _ _ c) = c

persistenceStrategyForConfig :: InterpreterConfig -> Maybe PersistenceStrategy
persistenceStrategyForConfig (LocalInterpreterConfig strat _ _ _ _) = Just strat
persistenceStrategyForConfig RemoteInterpreterConfig{} = Nothing
                         
errDie :: String -> IO ()                                                           
errDie err = hPutStrLn stderr err >> exitFailure

#ifndef VERSION_project_m36
#error VERSION_project_m36 is not defined
#endif
printWelcome :: IO ()
printWelcome = do
  putStrLn $ "Project:M36 TutorialD Interpreter " ++ VERSION_project_m36
  putStrLn "Type \":help\" for more information."
  putStrLn "A full tutorial is available at:"
  putStrLn "https://github.com/agentm/project-m36/blob/master/docs/tutd_tutorial.markdown"

main :: IO ()
main = do
  interpreterConfig <- execParser opts
  let connInfo = connectionInfoForConfig interpreterConfig
  fscheck <- checkFSType (checkFSForConfig interpreterConfig) (fromMaybe NoPersistence (persistenceStrategyForConfig interpreterConfig))
  if not fscheck then
    errDie checkFSErrorMsg
    else do
    dbconn <- connectProjectM36 connInfo
    case dbconn of 
      Left err -> 
        errDie ("Failed to create database connection: " ++ show err)
      Right conn -> do
        let connHeadName = headNameForConfig interpreterConfig
        eSessionId <- createSessionAtHead conn connHeadName
        case eSessionId of 
            Left err -> errDie ("Failed to create database session at \"" ++ show connHeadName ++ "\": " ++ show err)
            Right sessionId -> 
              case execTutDForConfig interpreterConfig of
                Nothing -> do
                  printWelcome
                  _ <- reprLoop interpreterConfig sessionId conn
                  pure ()
                Just tutdStr -> 
                  runTutorialD sessionId conn Nothing (T.pack tutdStr)


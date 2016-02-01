{-# LANGUAGE OverloadedStrings #-}
import TutorialD.Interpreter
import ProjectM36.Base
import ProjectM36.Client
import System.IO
import Options.Applicative
import System.Exit

parseArgs :: Parser InterpreterConfig
parseArgs = LocalInterpreterConfig <$> parsePersistenceStrategy <*> parseHeadName <|>
            RemoteInterpreterConfig <$> parseNodeId <*> parseDatabaseName <*> parseHeadName

parsePersistenceStrategy :: Parser PersistenceStrategy
parsePersistenceStrategy = CrashSafePersistence <$> (dbdirOpt <* fsyncOpt) <|>
                           MinimalPersistence <$> dbdirOpt <|>
                           pure NoPersistence
  where 
    dbdirOpt = strOption (short 'd' <> 
                          long "database-directory" <> 
                          metavar "DIRECTORY" <>
                          showDefaultWith show
                         )
    fsyncOpt = switch (short 'f' <>
                    long "fsync" <>
                    help "Fsync all new transactions.")
               
parseHeadName :: Parser HeadName               
parseHeadName = option auto (long "head" <>
                             help "Start session at head name." <>
                             value "master"
                            )
               
parseDatabaseName :: Parser DatabaseName               
parseDatabaseName = strOption (long "database" <>
                               help "Remote database name")
               
parseNodeId :: Parser NodeId
parseNodeId = createNodeId <$> 
              strOption (long "host" <> 
                         short 'h' <>
                         help "Remote host name" <>
                         value "127.0.0.1") <*> 
              option auto (long "port" <>
                           short 'p' <>
                      help "Remote port" <>
                      value defaultServerPort)

opts :: ParserInfo InterpreterConfig            
opts = info parseArgs idm

connectionInfoForConfig :: InterpreterConfig -> ConnectionInfo
connectionInfoForConfig (LocalInterpreterConfig pStrategy _) = InProcessConnectionInfo pStrategy outputNotificationCallback
connectionInfoForConfig (RemoteInterpreterConfig remoteNodeId remoteDBName _) = RemoteProcessConnectionInfo remoteDBName remoteNodeId outputNotificationCallback

headNameForConfig :: InterpreterConfig -> HeadName
headNameForConfig (LocalInterpreterConfig _ headn) = headn
headNameForConfig (RemoteInterpreterConfig _ _ headn) = headn
                           
errDie :: String -> IO ()                                                           
errDie err = hPutStrLn stderr err >> exitFailure

main :: IO ()
main = do
  interpreterConfig <- execParser opts
  let connInfo = connectionInfoForConfig interpreterConfig
  dbconn <- connectProjectM36 connInfo
  case dbconn of 
    Left err -> do
      errDie ("Failed to create database connection: " ++ show err)
    Right conn -> do
      let connHeadName = headNameForConfig interpreterConfig
      eSessionId <- createSessionAtHead connHeadName conn
      case eSessionId of 
          Left err -> errDie ("Failed to create database session at \"" ++ show connHeadName ++ "\": " ++ show err)
          Right sessionId -> do    
            _ <- reprLoop interpreterConfig sessionId conn
            pure ()


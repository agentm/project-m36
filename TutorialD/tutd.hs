{-# LANGUAGE OverloadedStrings #-}
import TutorialD.Interpreter
import ProjectM36.Base
import ProjectM36.Client
import System.IO
import Options.Applicative
import System.Exit

parseArgs :: Parser InterpreterConfig
parseArgs = LocalInterpreterConfig <$> parsePersistenceStrategy <|>
            RemoteInterpreterConfig <$> parseNodeId <*> parseDatabaseName

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
connectionInfoForConfig (LocalInterpreterConfig pStrategy) = InProcessConnectionInfo pStrategy
connectionInfoForConfig (RemoteInterpreterConfig remoteNodeId remoteDBName) = RemoteProcessConnectionInfo remoteDBName remoteNodeId
                           
main :: IO ()
main = do
  interpreterConfig <- execParser opts
  let connInfo = connectionInfoForConfig interpreterConfig
  dbconn <- connectProjectM36 connInfo
  case dbconn of 
    Left err -> do
      hPutStrLn stderr ("Failed to create database connection: " ++ show err)
      exitFailure
    Right conn -> do
      _ <- reprLoop interpreterConfig conn
      return ()


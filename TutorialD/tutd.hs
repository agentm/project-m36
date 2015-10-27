{-# LANGUAGE OverloadedStrings #-}
import TutorialD.Interpreter
import ProjectM36.Base
import ProjectM36.Client
import System.IO
import Options.Applicative
import System.Exit

{-
invocation:
tutd 
-no arguments indicates to run without persistence
tutd -d /database-directory
-persist to the database directory
-}
parseArgs :: Parser InterpreterConfig
parseArgs = InterpreterConfig <$> parsePersistenceStrategy

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

opts :: ParserInfo InterpreterConfig            
opts = info parseArgs idm
                           
main :: IO ()
main = do
  interpreterConfig <- execParser opts
  dbconn <- connectProjectM36 (InProcessConnectionInfo (persistenceStrategy interpreterConfig))
  case dbconn of 
    Left err -> do
      hPutStrLn stderr ("Failed to create database connection: " ++ show err)
      exitFailure
    Right conn -> do
      _ <- reprLoop interpreterConfig conn
      return ()


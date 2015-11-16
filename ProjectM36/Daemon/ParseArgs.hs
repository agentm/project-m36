module ProjectM36.Daemon.ParseArgs where
import ProjectM36.Base
import ProjectM36.Client
import Options.Applicative

data DaemonConfig = DaemonConfig { persistenceStrategy :: PersistenceStrategy, 
                                   databaseName :: DatabaseName
                                   }

parseArgs :: Parser DaemonConfig
parseArgs = DaemonConfig <$> parsePersistenceStrategy <*> parseDatabaseName

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
parseDatabaseName = strOption (short 'n' <>
                               long "dbname" <>
                               metavar "DATABASE NAME")

parseConfig :: IO DaemonConfig
parseConfig = execParser $ info parseArgs idm
  
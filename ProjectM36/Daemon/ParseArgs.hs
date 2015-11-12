module ProjectM36.Daemon.ParseArgs where
import ProjectM36.Base
import Options.Applicative

data DaemonConfig = DaemonConfig { persistenceStrategy :: PersistenceStrategy }

parseArgs :: Parser DaemonConfig
parseArgs = DaemonConfig <$> parsePersistenceStrategy

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

parseConfig :: IO DaemonConfig
parseConfig = execParser $ info parseArgs idm
  
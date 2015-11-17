module ProjectM36.Daemon.ParseArgs where
import ProjectM36.Base
import ProjectM36.Client
import Options.Applicative
import ProjectM36.Daemon.Config

parseArgs :: Parser DaemonConfig
parseArgs = DaemonConfig <$> parsePersistenceStrategy <*> parseDatabaseName <*> parseHostname <*> parsePort

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
                               long "database" <>
                               metavar "DATABASE_NAME")
                    
parseHostname :: Parser Hostname                    
parseHostname = strOption (short 'h' <>
                           long "hostname" <>
                           metavar "HOST_NAME" <>
                           value (bindHost defaultDaemonConfig))
                
parsePort :: Parser Port                
parsePort = option auto (short 'p' <>
                         long "port" <>
                         metavar "PORT_NUMBER" <>
                         value (bindPort defaultDaemonConfig))

parseConfig :: IO DaemonConfig
parseConfig = execParser $ info parseArgs idm
  
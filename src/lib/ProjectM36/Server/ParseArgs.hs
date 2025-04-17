module ProjectM36.Server.ParseArgs where
import ProjectM36.Base
import ProjectM36.Client
import Options.Applicative
import ProjectM36.Server.Config
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif

parseArgsWithDefaults :: ServerConfig -> Parser ServerConfig
parseArgsWithDefaults defaults = ServerConfig <$>
                                 parsePersistenceStrategy <*>
                                 parseCheckFS <*>
                                 parseDatabaseName <*>
                                 parseServerAddress <*>
                                 many parseGhcPkgPath <*>
                                 parseTimeout (perRequestTimeout defaults) <*>
                                 parseTestMode

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

parseTestMode :: Parser Bool
parseTestMode = flag True False (long "test-mode" <> hidden)

parseCheckFS :: Parser Bool
parseCheckFS = flag True False (long "disable-fscheck" <>
                                help "Disable filesystem check for journaling.")

parseServerAddress :: Parser RemoteServerAddress
parseServerAddress =
  (RemoteServerHostAddress <$>
   parseHostname "127.0.0.1" <*>
   parsePort 6543)
  <|>
  (RemoteServerUnixDomainSocketAddress <$> parseUnixDomainSocketPath)

parseUnixDomainSocketPath :: Parser FilePath
parseUnixDomainSocketPath = strOption (short 'x' <>
                                       long "unix-domain-socket" <>
                                       metavar "SOCKET_PATH")

parseDatabaseName :: Parser DatabaseName
parseDatabaseName = strOption (short 'n' <>
                               long "database" <>
                               metavar "DATABASE_NAME")

parseHostname :: Hostname -> Parser Hostname
parseHostname defHostname = strOption (short 'h' <>
                           long "hostname" <>
                           metavar "HOST_NAME" <>
                           value defHostname)

parsePort :: Port -> Parser Port
parsePort defPort = option auto (short 'p' <>
                         long "port" <>
                         metavar "PORT_NUMBER" <>
                         value defPort)

parseGhcPkgPath :: Parser String
parseGhcPkgPath = strOption (long "ghc-pkg-dir" <>
                              metavar "GHC_PACKAGE_DIRECTORY")

parseTimeout :: Int -> Parser Int
parseTimeout defTimeout = option auto (long "timeout" <>
                            metavar "MICROSECONDS" <>
                            value defTimeout)

parseConfig :: IO ServerConfig
parseConfig = parseConfigWithDefaults defaultServerConfig

parseConfigWithDefaults :: ServerConfig -> IO ServerConfig
parseConfigWithDefaults defaults = execParser (info (parseArgsWithDefaults defaults <**> helpOption) idm)

parseWSConfigWithDefaults :: ServerConfig -> IO WebsocketServerConfig
parseWSConfigWithDefaults defaults = execParser (info (parseWSArgsWithDefaults defaults <**> helpOption) idm)

parseWSArgsWithDefaults :: ServerConfig -> Parser WebsocketServerConfig
parseWSArgsWithDefaults defaults = WebsocketServerConfig <$>
                                 parseArgsWithDefaults defaults <*>
                                 parseTlsCertificatePath <*>
                                 parseTlsKeyPath


parseTlsCertificatePath :: Parser (Maybe String)
parseTlsCertificatePath = optional $ strOption (long "tls-certificate-path" <>
                              metavar "TLS_CERTIFICATE_PATH")

parseTlsKeyPath :: Parser (Maybe String)
parseTlsKeyPath = optional $ strOption (long "tls-key-path" <>
                              metavar "TLS_KEY_PATH")

helpOption :: Parser (a -> a)
helpOption = abortOption helpText $ mconcat
  [ long "help"
  , help "Show this help text"
  , hidden ]
  where
#if MIN_VERSION_optparse_applicative(0,16,0)
    helpText = ShowHelpText Nothing
#else
    helpText = ShowHelpText
#endif

module ProjectM36.Server.ParseArgs where
import ProjectM36.Base
import ProjectM36.Client
import Options.Applicative
import ProjectM36.Server.Config
import Data.ByteString (ByteString)
import Network.RPC.Curryer.Server

type ForServer = Bool

parseArgsWithDefaults :: ServerConfig -> Parser ServerConfig
parseArgsWithDefaults defaults = ServerConfig <$>
                                 parsePersistenceStrategy <*>
                                 parseCheckFS <*>
                                 parseDatabaseName <*>
                                 parseServerAddress <*>
                                 many parseGhcPkgPath <*>
                                 parseTimeout (perRequestTimeout defaults) <*>
                                 parseTestMode <*>
                                 parseServerConnectionConfig

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

parseServiceName :: ByteString -> Parser ByteString
parseServiceName defService = option auto (short 's' <>
                                          long "service" <>
                                          metavar "SERVICE_NAME" <>
                                          help "Used with TLS to distinguish between multiple databases on the same port via ALPN. Elide to use default service." <>
                                          value defService)

parseGhcPkgPath :: Parser String
parseGhcPkgPath = strOption (long "ghc-pkg-dir" <>
                              metavar "GHC_PACKAGE_DIRECTORY")

parseTimeout :: Int -> Parser Int
parseTimeout defTimeout = option auto (long "timeout" <>
                            metavar "MICROSECONDS" <>
                            value defTimeout)

parseServerTLSConfig :: Parser ServerTLSConfig
parseServerTLSConfig =
  ServerTLSConfig <$>
  parseServerTLSCertInfo <*>
  parseHostname "localhost" <*>
  parseServiceName ""
  
parseServerTLSCertInfo :: Parser ServerTLSCertInfo
parseServerTLSCertInfo =
  ServerTLSCertInfo <$>
  strOption (long "public-x509-key" <>
             metavar "KEY_PATH" <>
             help "Enables TLS with path to public key.") <*>
  strOption (long "private-x509-key" <>
             metavar "KEY_PATH" <>
             help "Enables TLS with path to private key.") <*>
  optional (strOption (long "certificate-x509-path" <>
                        metavar "CERT_PATH" <>
                        help "Path to certificate for TLS. Elide to use system's certificate store."))
  
-- TLS configuration for native project-m36 socket communication
parseServerConnectionConfig :: Parser ServerConnectionConfig
parseServerConnectionConfig =
  flag' UnencryptedConnectionConfig (long "disable-tls" <>
                                    help "Disable encryption (not recommended in production).") <|>
  EncryptedConnectionConfig <$>
    parseServerTLSConfig <*>
    parseClientAuth

parseConfig :: IO ServerConfig
parseConfig = parseConfigWithDefaults defaultServerConfig

parseConfigWithDefaults :: ServerConfig -> IO ServerConfig
parseConfigWithDefaults defaults = execParser (info (parseArgsWithDefaults defaults <**> helpOption) idm)

parseWSConfigWithDefaults :: ServerConfig -> IO WebsocketServerConfig
parseWSConfigWithDefaults defaults = execParser (info (parseWSArgsWithDefaults defaults <**> helpOption) idm)

parseWSArgsWithDefaults :: ServerConfig -> Parser WebsocketServerConfig
parseWSArgsWithDefaults defaults = WebsocketServerConfig <$>
                                 parseArgsWithDefaults defaults <*>
                                 parseWSTlsCertificatePath <*>
                                 parseWSTlsKeyPath

parseWSTlsCertificatePath :: Parser (Maybe String)
parseWSTlsCertificatePath = optional $ strOption (long "tls-certificate-path" <>
                              metavar "TLS_CERTIFICATE_PATH")

parseWSTlsKeyPath :: Parser (Maybe String)
parseWSTlsKeyPath = optional $ strOption (long "tls-key-path" <>
                              metavar "TLS_KEY_PATH")

parseRoleName :: Parser RoleName
parseRoleName = strOption (long "login-role" <>
                           metavar "ROLE_NAME" <>
                           value "admin")

-- | Whether or not to require the server to require a client certificate for mutual TLS. By default, the server disables anonymous connections.
parseClientAuth :: Parser ClientAuth
parseClientAuth = flag ClientAuthRequired AcceptAnonymousClient
                  (long "allow-anonymous-clients" <>
                   help "Disable client certificate requirement to allow anonymous users to connect.")
                                                                
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

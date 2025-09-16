module ProjectM36.Server.Config where
import ProjectM36.Client
import Network.RPC.Curryer.Server

data ServerConfig = ServerConfig { persistenceStrategy :: PersistenceStrategy,
                                   checkFS :: Bool,
                                   databaseName :: DatabaseName,
                                   bindAddress :: RemoteServerAddress,
                                   ghcPkgPaths :: [String], -- used for AtomFunction dynamic compilation
                                   perRequestTimeout :: Int,
                                   testMode :: Bool, -- used exclusively for automated testing of the server, thus not accessible from the command line
                                   connConfig :: ServerConnectionConfig
                                   }
                    deriving (Show)

data WebsocketServerConfig = WebsocketServerConfig { wsServerConfig :: ServerConfig,
                                                     tlsCertificatePath :: Maybe String,
                                                     tlsKeyPath :: Maybe String
                                                     }
                              deriving (Show)

defaultServerConfig :: ServerConfig
defaultServerConfig =
  ServerConfig { persistenceStrategy = NoPersistence,
                 checkFS = True,
                 databaseName = "base", 
                 bindAddress = RemoteServerHostAddress "127.0.0.1" 6543,
                 ghcPkgPaths = [],
                 perRequestTimeout = 0,
                 testMode = False,
                 -- default to strongest security
                 connConfig = EncryptedConnectionConfig tlsConfig ClientAuthRequired
               }
  where
    tlsConfig =
      ServerTLSConfig {
      tlsCertInfo = ServerTLSCertInfo {
          x509PublicFilePath = "server.cert.pem",
          x509PrivateFilePath = "server.key.pem",
          x509CertFilePath = Nothing
          },
      tlsServerHostName = "127.0.0.1",
      tlsServerServiceName = ""
                                }


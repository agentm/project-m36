module ProjectM36.Server.Config where
import ProjectM36.Client

data ServerConfig = ServerConfig { persistenceStrategy :: PersistenceStrategy,
                                   checkFS :: Bool,
                                   databaseName :: DatabaseName,
                                   bindHost :: Hostname,
                                   bindPort :: Port,
                                   ghcPkgPaths :: [String], -- used for AtomFunction dynamic compilation
                                   perRequestTimeout :: Int,
                                   testMode :: Bool -- used exclusively for automated testing of the server, thus not accessible from the command line
                                   }
                    deriving (Show)

data WebsocketServerConfig = WebsocketServerConfig { wsServerConfig :: ServerConfig,
                                                     tlsCertificatePath :: Maybe String,
                                                     tlsKeyPath :: Maybe String
                                                     }
                              deriving (Show)

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig { persistenceStrategy = NoPersistence,
                                     checkFS = True,
                                     databaseName = "base", 
                                     bindHost = "127.0.0.1",
                                     bindPort = 6543,
                                     ghcPkgPaths = [],
                                     perRequestTimeout = 0,
                                     testMode = False
                                     }

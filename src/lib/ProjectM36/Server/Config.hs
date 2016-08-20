module ProjectM36.Server.Config where
import ProjectM36.Client

data ServerConfig = ServerConfig { persistenceStrategy :: PersistenceStrategy, 
                                   databaseName :: DatabaseName,
                                   bindHost :: Hostname,
                                   bindPort :: Port
                                   }

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig { persistenceStrategy = NoPersistence,
                                     databaseName = "base", 
                                     bindHost = "127.0.0.1",
                                     bindPort = 6543
                                     }

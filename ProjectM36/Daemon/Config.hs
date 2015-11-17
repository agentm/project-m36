module ProjectM36.Daemon.Config where
import ProjectM36.Client

data DaemonConfig = DaemonConfig { persistenceStrategy :: PersistenceStrategy, 
                                   databaseName :: DatabaseName,
                                   bindHost :: Hostname,
                                   bindPort :: Port
                                   }

defaultDaemonConfig :: DaemonConfig
defaultDaemonConfig = DaemonConfig { persistenceStrategy = NoPersistence,
                                     databaseName = "base", 
                                     bindHost = "127.0.0.1",
                                     bindPort = 6543
                                     }

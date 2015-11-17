module ProjectM36.Daemon.Config where
import ProjectM36.Client

data DaemonConfig = DaemonConfig { persistenceStrategy :: PersistenceStrategy, 
                                   databaseName :: DatabaseName
                                   }

defaultDaemonConfig :: DaemonConfig
defaultDaemonConfig = DaemonConfig { persistenceStrategy = NoPersistence,
                                     databaseName = "base" }

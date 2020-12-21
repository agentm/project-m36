--types shared across server and client modules
module ProjectM36.Server.Types where
import StmContainers.Set as StmSet
import ProjectM36.Base
import Data.Hashable
import ProjectM36.Sessions
import Control.Concurrent.STM
import ProjectM36.ScriptSession
import ProjectM36.FileLock
import Control.Concurrent.MVar
import ProjectM36.TransactionGraph.Persist
import Network.RPC.Curryer.Server as RPC
import Network.Socket

-- | The 'Connection' represents either local or remote access to a database. All operations flow through the connection.
type ClientNodes = StmSet.Set ClientInfo

type DatabaseName = String

-- internal structure specific to in-process connections
data InProcessConnectionConf = InProcessConnectionConf {
  ipPersistenceStrategy :: PersistenceStrategy, 
  ipClientNodes :: ClientNodes, 
  ipSessions :: Sessions, 
  ipTransactionGraph :: TVar TransactionGraph,
  ipScriptSession :: Maybe ScriptSession,
  ipLocks :: Maybe (LockFile, MVar LockFileHash) -- nothing when NoPersistence
  }

-- clients may connect associate one socket with one database
data ClientInfo = ClientInfo { databaseName :: DatabaseName,
                               clientSocket :: RPC.Locking Socket }

instance Eq ClientInfo where
  a == b = RPC.lockless (clientSocket a) == RPC.lockless (clientSocket b)

instance Hashable ClientInfo where
  hashWithSalt salt ci = hashWithSalt salt (show (RPC.lockless (clientSocket ci)))

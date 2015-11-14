module ProjectM36.Daemon.EntryPoints where
import ProjectM36.Base
import ProjectM36.Client
import ProjectM36.Error
import ProjectM36.Daemon.RemoteCallTypes
import Control.Distributed.Process (Process)
import Control.Distributed.Process.ManagedProcess (ProcessReply)
import Control.Distributed.Process.ManagedProcess.Server (reply)
import Control.Monad.IO.Class (liftIO)

handleExecuteRelationalExpr :: Connection -> RelationalExpr -> Process (ProcessReply (Either RelationalError Relation) Connection)
handleExecuteRelationalExpr conn expr = do
  ret <- liftIO $ executeRelationalExpr conn expr
  reply ret conn
  
handleLogin :: Connection -> Process (ProcessReply Bool Connection)
handleLogin conn = reply True conn
  
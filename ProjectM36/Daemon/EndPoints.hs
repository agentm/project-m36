module ProjectM36.Daemon.EndPoints where
import ProjectM36.Base
import ProjectM36.Client
import ProjectM36.Error

import Control.Distributed.Process (Process)
import Control.Distributed.Process.ManagedProcess (ProcessReply)
import Control.Distributed.Process.ManagedProcess.Server (reply)
import Control.Monad.IO.Class (liftIO)

handleExecuteRelationalExpr :: Connection -> RelationalExpr -> Process (ProcessReply (Either RelationalError Relation) Connection)
handleExecuteRelationalExpr conn expr = do
  ret <- liftIO $ executeRelationalExpr conn expr
  reply ret conn
  
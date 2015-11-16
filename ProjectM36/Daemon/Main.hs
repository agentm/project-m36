{-# LANGUAGE ScopedTypeVariables #-}
import ProjectM36.Client
import ProjectM36.Daemon.ParseArgs (parseConfig, persistenceStrategy, databaseName)
import ProjectM36.Daemon.EntryPoints (handleExecuteRelationalExpr, handleExecuteDatabaseContextExpr, handleLogin, handleExecuteHeadName, handleExecuteGraphExpr)
import ProjectM36.Daemon.RemoteCallTypes (RemoteExecution(..))

import System.Exit (exitFailure, exitSuccess)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process.Node (initRemoteTable, runProcess, newLocalNode)
import Control.Distributed.Process.Extras.Time (Delay(..))
import Control.Distributed.Process (ProcessId, spawnLocal, Process, register, RemoteTable)
import Control.Distributed.Process.ManagedProcess (defaultProcess, UnhandledMessagePolicy(..), ProcessDefinition(..), handleCall, serve, InitHandler, InitResult(..))
import Control.Monad (forever)
import System.IO (hPutStrLn, stderr)
import qualified Control.Distributed.Process.Extras.Internal.Types as DIT

serverDefinition :: ProcessDefinition Connection
serverDefinition = defaultProcess {
  apiHandlers = [handleCall (\conn Login -> handleLogin conn),
                 handleCall (\conn ExecuteHeadName -> handleExecuteHeadName conn),
                 handleCall (\conn (ExecuteRelationalExpr expr) -> handleExecuteRelationalExpr conn expr),
                 handleCall (\conn (ExecuteDatabaseContextExpr expr) -> handleExecuteDatabaseContextExpr conn expr),
                 handleCall (\conn (ExecuteGraphExpr expr) -> handleExecuteGraphExpr conn expr)               
                 ],
  unhandledMessagePolicy = Log
  }
                 
initServer :: InitHandler Connection Connection                 
initServer conn = return $ InitOk conn Infinity

launchServer :: Connection -> Process ProcessId
launchServer conn = spawnLocal (serve conn initServer serverDefinition)

remoteTable :: RemoteTable
remoteTable = DIT.__remoteTable initRemoteTable
  
main :: IO ()
main = do
  daemonConfig <- parseConfig
  econn <- connectProjectM36 (InProcessConnectionInfo (persistenceStrategy daemonConfig))
  case econn of 
    Left err -> do      
      hPutStrLn stderr ("Failed to create database connection: " ++ show err)
      exitFailure
    Right conn -> do
      etransport <- createTransport "127.0.0.1" (show defaultServerPort) defaultTCPParameters
      case etransport of
        Left err -> error (show err)
        Right transport -> do
          localTCPNode <- newLocalNode transport remoteTable
          runProcess localTCPNode $ do
            serverPid <- launchServer conn
            let dbname = remoteDBLookupName (databaseName daemonConfig)
            register dbname serverPid
            liftIO $ putStrLn $ show serverPid ++ " " ++ dbname
          _ <- forever $ threadDelay 1000000
          putStrLn "Server Exit."
          exitSuccess
     
        
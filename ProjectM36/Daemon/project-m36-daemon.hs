import ProjectM36.Daemon
import ProjectM36.Daemon.ParseArgs
import System.Exit (exitSuccess, exitFailure)

main :: IO ()
main = do
  daemonConfig <- parseConfig
  ret <- launchServer daemonConfig Nothing
  if ret then exitSuccess else exitFailure
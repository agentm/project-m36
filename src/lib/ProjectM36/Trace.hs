module ProjectM36.Trace where
import Debug.Trace (traceEventIO)
-- utility module to enable easy enabling/disabling of eventlog data

-- | Utility function for tracing with ghc-events-analyze using START and STOP markers
traceBlock :: String -> IO () -> IO ()
traceBlock label m = do
  traceEventIO ("START " <> label)
  m
  traceEventIO ("STOP " <> label)

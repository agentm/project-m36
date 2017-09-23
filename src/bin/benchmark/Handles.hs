{-# LANGUAGE CPP #-}
--benchmark test for file handle leaks
import ProjectM36.Client
import Options.Applicative
import TutorialD.Interpreter
import TutorialD.Interpreter.Base
import qualified Data.Text as T
import Text.Megaparsec hiding (option)
import Data.Monoid
import System.Directory
import Control.Monad

data HandlesArgs = HandlesArgs {
  openCloseCount :: Int,
  transactionCount :: Int,
  dbdir :: FilePath, 
  tutdSetup :: String,
  tutdIterate :: String
  }

parseArgs :: Parser HandlesArgs
parseArgs = HandlesArgs <$> parseOpenAndCloseCount <*> parseTransactionCount <*> parseDbDir <*> parseTutdSetup <*> parseTutdIterate

parseOpenAndCloseCount :: Parser Int
parseOpenAndCloseCount = option auto (short 'o' <> long "open-close-count")

parseTransactionCount :: Parser Int
parseTransactionCount = option auto (short 't' <> long "transaction-count")

parseDbDir :: Parser FilePath
parseDbDir = strOption (short 'd' <> long "dbdir")

parseTutdSetup :: Parser String
parseTutdSetup = strOption (short 's' <> long "setup-tutd" <> value "x:=relation{tuple{v t}}")

parseTutdIterate :: Parser String
parseTutdIterate = strOption (short 'i' <> long "iterate-tutd" <> value "update x (v:=not(@v))")

main :: IO ()
main = do
  args <- execParser $ info (helper <*> parseArgs) fullDesc
  replicateM_ (openCloseCount args) (runOpenClose 
                                     (T.pack (tutdSetup args))
                                     (T.pack (tutdIterate args))
                                     (transactionCount args) 
                                     (dbdir args))
  
runOpenClose :: T.Text -> T.Text -> Int -> FilePath -> IO ()  
runOpenClose tutdSetup' tutdIterate' tCount dbdir' = do
  let connInfo = InProcessConnectionInfo (MinimalPersistence dbdir') emptyNotificationCallback []
  eConn <- connectProjectM36 connInfo
  case eConn of
    Left err -> error (show err)
    Right conn -> do
      eSess <- createSessionAtHead conn "master"
      case eSess of
        Left err -> error (show err)
        Right session -> do
          --database setup
          case parseTutorialD tutdSetup' of
            Left err -> error (show err)
            Right parsed -> do
              res <- evalTutorialD session conn UnsafeEvaluation parsed
              case res of
                DisplayErrorResult err -> error (T.unpack err)
                DisplayParseErrorResult _ err -> error (parseErrorPretty err)
                _ -> do 
                  replicateM_ tCount (runTransaction tutdIterate' session conn)
                  close conn
                  printFdCount
  
runTransaction :: T.Text -> SessionId -> Connection -> IO ()
runTransaction tutdIterate' sess conn = do
  --run tutd on every iteration
  case parseTutorialD tutdIterate' of
    Left err -> error (show err)
    Right parsed -> do
      res <- evalTutorialD sess conn UnsafeEvaluation parsed
      case res of
        DisplayErrorResult err -> error (T.unpack err)
        DisplayParseErrorResult _ err -> error (parseErrorPretty err)
        _ -> do 
          eErr <- commit sess conn 
          case eErr of
            Left err -> error (show err)
            Right _ -> printFdCount
      
--prints out number of consumed file descriptors      
printFdCount :: IO ()
#if defined(linux_HOST_OS)
printFdCount = do
  fdc <- fdCount
  putStrLn ("Fd count: " ++ show fdc)
  --getLine >> pure ()
#else
printFdCount = putStrLn "Fd count not supported on this OS."
#endif


fdCount :: IO Int
#if defined(linux_HOST_OS)
fdCount = do
  fds <- getDirectoryContents "/proc/self/fd"
  pure ((length fds) - 2)
#else 
--not supported on non-linux
fdCount = pure 0
#endif
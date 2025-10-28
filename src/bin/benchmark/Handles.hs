{-# LANGUAGE CPP #-}
--benchmark test for file handle leaks
import ProjectM36.Client
import ProjectM36.Persist
import Options.Applicative
import TutorialD.Interpreter
import ProjectM36.Interpreter hiding (Parser)
import TutorialD.Interpreter.Base hiding (option)
import qualified Data.Text as T
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif
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
  let connInfo = InProcessConnectionInfo (MinimalPersistence dbdir') emptyNotificationCallback [] basicDatabaseContext "admin"
  eConn <- connectProjectM36 connInfo 
  case eConn of
    Left err -> error (show err)
    Right conn -> do
      eSess <- createSessionAtHead conn "master"
      case eSess of
        Left err -> error (show err)
        Right session ->
          --database setup
          case parseTutorialD tutdSetup' of
            Left err -> error (show err)
            Right parsed -> do
              res <- evalTutorialD session conn UnsafeEvaluation parsed
              case res of
                DisplayErrorResult err -> error (T.unpack err)
                DisplayParseErrorResult _ err ->
#if MIN_VERSION_megaparsec(7,0,0)
                  error (errorBundlePretty err)
#else
                  error (parseErrorPretty err)
#endif
                _ -> do
                  replicateM_ tCount (runTransaction tutdIterate' session conn)
                  close conn
                  printFdCount

runTransaction :: T.Text -> SessionId -> Connection -> IO ()
runTransaction tutdIterate' sess conn =
  --run tutd on every iteration
  case parseTutorialD tutdIterate' of
    Left err -> error (show err)
    Right parsed -> do
      res <- evalTutorialD sess conn UnsafeEvaluation parsed
      case res of
        DisplayErrorResult err -> error (T.unpack err)
        DisplayParseErrorResult _ err ->
#if MIN_VERSION_megaparsec(7,0,0)
          error (errorBundlePretty err)
#else
          error (parseErrorPretty err)
#endif
        _ -> do
          eErr <- commit sess conn
          case eErr of
            Left err -> error (show err)
            Right _ -> printFdCount

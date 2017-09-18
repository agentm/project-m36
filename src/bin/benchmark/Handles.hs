{-# LANGUAGE CPP #-}
--benchmark test for file handle leaks
import ProjectM36.Client
import Options.Applicative
import TutorialD.Interpreter
import TutorialD.Interpreter.Base
import qualified Data.Text as T
import Text.Megaparsec hiding (option)
import Data.Monoid
import qualified Data.Map as M
import System.Directory
import Control.Monad

type OpenCloseCount = Int 

type TransactionCount = Int

type DbDir = String

data HandlesArgs = HandlesArgs OpenCloseCount TransactionCount DbDir

parseArgs :: Parser HandlesArgs
parseArgs = HandlesArgs <$> parseOpenAndCloseCount <*> parseTransactionCount <*> parseDbDir

parseOpenAndCloseCount :: Parser OpenCloseCount
parseOpenAndCloseCount = option auto (short 'o' <> long "open-close-count")

parseTransactionCount :: Parser TransactionCount
parseTransactionCount = option auto (short 't' <> long "transaction-count")

parseDbDir :: Parser DbDir
parseDbDir = strOption (short 'd' <> long "dbdir")

main :: IO ()
main = do
  (HandlesArgs ocCount tCount dbdir) <- execParser $ info (helper <*> parseArgs) fullDesc
  replicateM_ ocCount (runOpenClose tCount dbdir)
  
runOpenClose :: TransactionCount -> DbDir -> IO ()  
runOpenClose tCount dbdir = do
  let connInfo = InProcessConnectionInfo (MinimalPersistence dbdir) emptyNotificationCallback []
  eConn <- connectProjectM36 connInfo
  case eConn of
    Left err -> error (show err)
    Right conn -> do
      eSess <- createSessionAtHead conn "master"
      case eSess of
        Left err -> error (show err)
        Right session -> do
          --database setup
          eErr <- executeDatabaseContextExpr session conn (Assign "x" (MakeRelationFromExprs Nothing [TupleExpr (M.singleton "v" (NakedAtomExpr (BoolAtom True)))]))
          case eErr of
            Left err -> error (show err)
            Right _ -> do
              replicateM_ tCount (runTransaction session conn)
              close conn
              printFdCount
  
runTransaction :: SessionId -> Connection -> IO ()
runTransaction sess conn = do
  --run tutd on every iteration
  let tutd = "update x (v:=not(@v))"
  case parseTutorialD tutd of
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
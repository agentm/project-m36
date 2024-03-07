-- the sqlegacy SQL interpreter wrap
{-# LANGUAGE CPP #-}
import SQL.Interpreter.Convert
import ProjectM36.Base
import ProjectM36.Cli
import SQL.Interpreter
import ProjectM36.SQLDatabaseContext
import ProjectM36.Error
import System.Directory
import System.FilePath
import qualified ProjectM36.Client as C
import qualified Data.Text as T
import Data.Either (fromRight)
import Control.Exception (catchJust)
import ProjectM36.Interpreter


#if !defined(VERSION_project_m36) 
# warning Failed to discover proper version from cabal_macros.h
# define VERSION_project_m36 "<unknown>"
#endif

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  let historyPath = homeDir </> ".sqlegacy_history"
  mainLoop printWelcome historyPath sqlReprLoop promptText sqlReprLoop sqlDatabaseContext
  
printWelcome :: IO ()
printWelcome = do
  putStrLn ("Project:M36 SQLegacy Interpreter " ++ VERSION_project_m36)
  putStrLn "SQL does not support the complete relational algebra. To access the complete relational algebra, use the bundled \"tutd\" interpreter."
  putStrLn "Type \"help\" for more information."

sqlReprLoop :: C.SessionId -> C.Connection -> Maybe PromptLength -> T.Text -> IO ()
sqlReprLoop sessionId conn mPromptLength userInput = do
  case parseSQLUserInput userInput of
    Left err ->
      displayResult (DisplayParseErrorResult mPromptLength err)
    Right parsed ->
      catchJust (\exc -> if exc == C.RequestTimeoutException then Just exc else Nothing) (do
        evald <- evalSQLInteractive sessionId conn UnsafeEvaluation True parsed
        displayResult evald)
        (\_ -> displayResult (DisplayErrorResult "Request timed out."))
      

promptText :: Either RelationalError HeadName -> Either RelationalError SchemaName -> StringType
promptText eHeadName eSchemaName = "SQLegacy (" <> transInfo <> "): "
  where
    transInfo = fromRight "<unknown>" eHeadName <> "/" <> fromRight "<no schema>" eSchemaName
  

displayResult :: ConsoleResult -> IO ()
displayResult = undefined


{-# LANGUAGE OverloadedStrings #-}
import TutorialD.Interpreter
import ProjectM36.RelationalExpression
import ProjectM36.TransactionGraph
import ProjectM36.TransactionGraph.Persist
import ProjectM36.Transaction
import ProjectM36.Base
import System.IO
import Options.Applicative
import Data.UUID.V4 (nextRandom)
import qualified Data.Text as T
import System.Exit

{-
invocation:
tutd 
-no arguments indicates to run without persistence
tutd -d /database-directory
-persist to the database directory
-}
parseArgs :: Parser InterpreterConfig
parseArgs = InterpreterConfig <$> parsePersistenceStrategy

parsePersistenceStrategy :: Parser PersistenceStrategy
parsePersistenceStrategy = do
  MinimalPersistence <$> strOption (short 'd' <> 
                                    long "database-directory" <> 
                                    metavar "DIRECTORY" <>
                                    showDefaultWith show
                                   )
    <|> pure NoPersistence

     
       
                           
configSetup :: InterpreterConfig -> IO (TransactionGraph)                           
configSetup config = do
  let failHard msg = do
        hPutStrLn stderr msg
        exitFailure
  firstGraphUUID <- nextRandom
  let bootstrapGraph = bootstrapTransactionGraph firstGraphUUID dateExamples
  case persistenceStrategy config of
    --create date examples graph for now- probably should be empty context in the future
    NoPersistence -> do { putStrLn "Running without filesystem persistence."; return bootstrapGraph}
    MinimalPersistence dbdir -> do
      err <- setupDatabaseDir dbdir bootstrapGraph 
      case err of
        Just err' -> failHard ("Failed to configure database: " ++ show err')
        Nothing -> do 
          graph <- transactionGraphLoad dbdir emptyTransactionGraph
          case graph of
            Left err' -> failHard ("Failed to load graph: " ++ show err')
            Right graph' -> return graph'
            
opts :: ParserInfo InterpreterConfig            
opts = info parseArgs idm
                           
main :: IO ()
main = do
  interpreterConfig <- execParser opts
  let currentHeadName = "master"
  freshGraph <- configSetup interpreterConfig
  case transactionForHead currentHeadName freshGraph of 
      Nothing -> hPutStrLn stderr $ "Failed to find head transaction for " ++ T.unpack currentHeadName ++ "."
      Just headTransaction -> do 
        let currentTransaction = newDisconnectedTransaction (transactionUUID headTransaction) (transactionContext headTransaction)
        reprLoop interpreterConfig currentTransaction freshGraph


{-# LANGUAGE OverloadedStrings #-}
import TutorialD.Interpreter
import ProjectM36.RelationalExpression
import ProjectM36.TransactionGraph
import ProjectM36.TransactionGraph.Persist
import ProjectM36.Transaction
import ProjectM36.Base
import System.IO
import Data.UUID.V4 (nextRandom)
import qualified Data.Text as T

main :: IO ()
main = do
  freshUUID <- nextRandom
  let currentHeadName = "master"
      --if the database is empty, bootstrap it with the bootstrap graph
      bootstrapGraph = bootstrapTransactionGraph freshUUID dateExamples
      interpreterConfig = InterpreterConfig { persistenceStrategy = MinimalPersistence,
                                       databaseDirectory = "/tmp/testdb" }
      dbdir = databaseDirectory interpreterConfig
  err <- setupDatabaseDir dbdir  bootstrapGraph
  case err of
    Just err' -> hPutStrLn stderr $ "Database setup failed: " ++ show err'
    Nothing -> do
      freshGraph <- transactionGraphLoad dbdir emptyTransactionGraph
      case freshGraph of
        Left err'' -> hPutStrLn stderr $ "Database load failed: " ++ show err''
        Right freshGraph' -> case transactionForHead currentHeadName freshGraph' of 
                Nothing -> hPutStrLn stderr $ "Failed to find head transaction for " ++ T.unpack currentHeadName ++ "."
                Just headTransaction -> do 
                  let currentTransaction = newDisconnectedTransaction (transactionUUID headTransaction) (transactionContext headTransaction)
                  reprLoop interpreterConfig currentTransaction freshGraph'


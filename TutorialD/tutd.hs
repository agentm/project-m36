{-# LANGUAGE OverloadedStrings #-}
import TutorialD.Interpreter
import ProjectM36.RelationalExpression
import ProjectM36.TransactionGraph
import ProjectM36.TransactionGraph.Persist
import ProjectM36.Transaction
import ProjectM36.Base
import System.IO
import Data.UUID.V4 (nextRandom)

main :: IO ()
main = do
  freshUUID <- nextRandom
  let currentHeadName = "master"
      --if the database is empty, bootstrap it with the bootstrap graph
      bootstrapGraph = bootstrapTransactionGraph freshUUID dateExamples
      dbdir = "/tmp/testdb"
  err <- setupDatabaseDir dbdir bootstrapGraph
  case err of
    Just err -> hPutStrLn stderr $ "Database setup failed: " ++ show err
    Nothing -> do
      freshGraph <- transactionGraphLoad dbdir emptyTransactionGraph
      case freshGraph of
        Left err -> hPutStrLn stderr $ "Database load failed: " ++ show err
        Right freshGraph' -> do
          let headTransaction = case transactionForHead currentHeadName freshGraph' of { Just x -> x; _ -> undefined}
              currentTransaction = newDisconnectedTransaction (transactionUUID headTransaction) (transactionContext headTransaction)
          reprLoop dbdir currentTransaction freshGraph'


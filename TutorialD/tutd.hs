{-# LANGUAGE OverloadedStrings #-}
import TutorialD.Interpreter
import ProjectM36.RelationalExpression
import ProjectM36.Transaction
import ProjectM36.Base
import Data.UUID.V4 (nextRandom)

main :: IO ()
main = do
  freshUUID <- nextRandom
  let currentHeadName = "master"
      newGraph = bootstrapTransactionGraph freshUUID dateExamples
      headTransaction = case transactionForHead currentHeadName newGraph of { Just x -> x; _ -> undefined}
      dbdir = "/tmp/testdb"
      currentTransaction = newDisconnectedTransaction (transactionUUID headTransaction) (transactionContext headTransaction)
  reprLoop dbdir currentTransaction newGraph


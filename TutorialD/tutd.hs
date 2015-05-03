import TutorialD.Interpreter
import ProjectM36.RelationalExpression
import ProjectM36.Transaction
import ProjectM36.Base
import Data.UUID.V4 (nextRandom)

main :: IO ()
main = do
  freshUUID <- nextRandom
  let currentHeadName = "master"
  let newGraph = bootstrapTransactionGraph freshUUID dateExamples
  let headTransaction = case transactionForHead currentHeadName newGraph of { Just x -> x; _ -> undefined}
  let currentTransaction = newDisconnectedTransaction (transactionUUID headTransaction) (transactionContext headTransaction)
  reprLoop currentTransaction newGraph


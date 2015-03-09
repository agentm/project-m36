import TutorialDInterpreter
import RelationExpr
import RelationalTransaction
import RelationType
import Data.UUID.V4 (nextRandom)

main = do
  freshUUID <- nextRandom
  let currentHeadName = "master"
  let newGraph = bootstrapTransactionGraph freshUUID dateExamples
  let headTransaction = case transactionForHead currentHeadName newGraph of { Just x -> x; _ -> undefined}
  let currentTransaction = newDisconnectedTransaction (transactionUUID headTransaction) (transactionContext headTransaction)
  reprLoop currentTransaction newGraph


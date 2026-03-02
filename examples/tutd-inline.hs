{-# LANGUAGE QuasiQuotes #-}
import TutorialD.Interpreter.Template
import ProjectM36.Client
import ProjectM36.DatabaseContextExpr
import ProjectM36.DatabaseContext
import ProjectM36.DateExamples
import ProjectM36.TransactionGraph
import System.Random

main :: IO ()
main = do
  rando <- initStdGen

  conn <- failFast $ connectProjectM36 (InProcessConnectionInfo NoPersistence emptyNotificationCallback [] basicDatabaseContext rando "admin")

  sessionId <- failFast $ createSessionAtHead conn "master"

  -- add Date relvars
  case databaseContextAsDatabaseContextExpr (toDatabaseContext dateExamples) emptyTransactionGraph of
    Left err -> error (show err) 
    Right dbexpr -> do
      failFast $ executeDatabaseContextExpr sessionId conn dbexpr

  let london_suppliers =
        replaceTextAtom "$1" "London" [relationalExpr|s where city="$1"|]

  _res <- failFast $ executeRelationalExpr sessionId conn london_suppliers
      

  let insert_adelaide = [databaseContextExpr|insert s relation{tuple{city "adelaide", s# "S6", sname "Jacobs", status 10}}|]
      insert_adelaide' = replaceTextAtom "Jacobs" "Jacks" insert_adelaide
  failFast $ executeDatabaseContextExpr sessionId conn insert_adelaide'
  
failFast :: Show a => IO (Either a b) -> IO b
failFast m = do
  ret <- m
  case ret of
    Left err -> error (show err)
    Right val -> pure val

{-# LANGUAGE GADTs,OverloadedStrings #-}
module TutorialD.Interpreter where
import TutorialD.Interpreter.Base
import TutorialD.Interpreter.RODatabaseContextOperator
import TutorialD.Interpreter.DatabaseExpr
import TutorialD.Interpreter.TransactionGraphOperator
import TutorialD.Interpreter.Import.CSV
import TutorialD.Interpreter.Import.TutorialD
import TutorialD.Interpreter.Import.Base
import ProjectM36.Base
import ProjectM36.Relation.Show.Term
import ProjectM36.TransactionGraph
import ProjectM36.Client
import Text.Parsec
import Text.Parsec.String
import Control.Monad.State
import System.Console.Haskeline
import System.Directory (getHomeDirectory)
import qualified Data.Text as T
import Control.Applicative ((<*))

{-
context ops are read-only operations which only operate on the database context (relvars and constraints)
database ops are read-write operations which change the database context (such as relvar assignment)
graph ops are read-write operations which change the transaction graph
-}
data ParsedOperation = RODatabaseContextOp RODatabaseContextOperator |
                       DatabaseExprOp DatabaseExpr |
                       GraphOp TransactionGraphOperator |
                       ROGraphOp ROTransactionGraphOperator |
                       ImportOp DataImportOperator

interpreterParserP :: Parser ParsedOperation
interpreterParserP = liftM RODatabaseContextOp (roDatabaseContextOperatorP <* eof) <|>
                     liftM GraphOp (transactionGraphOpP <* eof) <|>
                     liftM ROGraphOp (roTransactionGraphOpP <* eof) <|>
                     liftM DatabaseExprOp (databaseExprOpP <* eof) <|>
                     liftM ImportOp ((importCSVP <|> tutdImportP) <* eof) 

promptText :: DisconnectedTransaction -> TransactionGraph -> StringType
promptText (DisconnectedTransaction parentUUID _) graph = "TutorialD (" `T.append` transInfo `T.append` "): "
  where
    transInfo = case transactionForUUID parentUUID graph of
      Left _ -> "unknown"
      Right parentTrans -> case headNameForTransaction parentTrans graph of
          Nothing -> T.pack (show $ transactionUUID parentTrans)
          Just headName -> headName
          
parseTutorialD :: Connection -> String -> Either ParseError ParsedOperation
parseTutorialD (InProcessConnection _ _) inputString = parse interpreterParserP "" inputString

--execute the operation and display result
evalTutorialD :: Connection -> ParsedOperation -> IO (TutorialDOperatorResult)
evalTutorialD conn expr = case expr of
  
  --this does not pass through the ProjectM36.Client library because the operations
  --are specific to the interpreter, though some operations may be of general use in the future
  (RODatabaseContextOp execOp) -> do
    (DisconnectedTransaction _ context) <- disconnectedTransaction conn
    return $ evalRODatabaseContextOp context execOp
    
  (DatabaseExprOp execOp) -> do 
    maybeErr <- executeDatabaseContextExpr conn execOp 
    case maybeErr of
      Just err -> barf err
      Nothing -> return $ QuietSuccessResult
    
  (GraphOp execOp) -> do
    maybeErr <- executeGraphExpr conn execOp
    case maybeErr of
      Just err -> barf err
      Nothing -> return $ QuietSuccessResult

  (ROGraphOp execOp) -> do
    discon <- disconnectedTransaction conn
    graph <- transactionGraph conn
    case evalROGraphOp discon graph execOp of
      Left err -> barf err
      Right rel -> return $ DisplayResult $ showRelation rel
      
  (ImportOp execOp) -> do
    (DisconnectedTransaction _ context) <- disconnectedTransaction conn
    exprErr <- evalDataImportOperator execOp context
    case exprErr of
      Left err -> barf err
      Right dbexpr -> evalTutorialD conn (DatabaseExprOp dbexpr)
        
  where
    barf err = return $ DisplayErrorResult (T.pack (show err))
  
data InterpreterConfig = InterpreterConfig { 
  persistenceStrategy :: PersistenceStrategy
  }

reprLoop :: InterpreterConfig -> Connection -> IO ()
reprLoop config conn = do
  homeDirectory <- getHomeDirectory
  let settings = defaultSettings {historyFile = Just (homeDirectory ++ "/.tutd_history")}
  discon <- disconnectedTransaction conn
  graph <- transactionGraph conn
  maybeLine <- runInputT settings $ getInputLine (T.unpack (promptText discon graph))
  case maybeLine of
    Nothing -> return ()
    Just line -> do
      case parseTutorialD conn line of
        Left err -> do
          displayOpResult $ DisplayErrorResult (T.pack (show err))
        Right parsed -> do 
          evald <- evalTutorialD conn parsed
          displayOpResult evald
      reprLoop config conn

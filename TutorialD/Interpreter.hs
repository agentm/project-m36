{-# LANGUAGE GADTs,OverloadedStrings #-}
module TutorialD.Interpreter where
import TutorialD.Interpreter.Base
import TutorialD.Interpreter.RODatabaseContextOperator
import TutorialD.Interpreter.DatabaseExpr
import TutorialD.Interpreter.TransactionGraphOperator
import ProjectM36.Base
import ProjectM36.TransactionGraph
import ProjectM36.TransactionGraph.Persist
import Text.Parsec
import Text.Parsec.String
import Control.Monad.State
import System.Console.Haskeline
import System.IO
import System.Directory (getHomeDirectory)
import qualified Data.UUID as U
import Data.UUID.V4 (nextRandom)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Applicative ((<*))

{-
context ops are read-only operations which only operate on the database context (relvars and constraints)
database ops are read-write operations which change the database context (such as relvar assignment)
graph ops are read-write operations which change the transaction graph
-}
data ParsedOperation = RODatabaseContextOp RODatabaseContextOperator |
                       DatabaseExprOp DatabaseExpr |
                       GraphOp TransactionGraphOperator

interpreterParserP :: Parser ParsedOperation
interpreterParserP = liftM RODatabaseContextOp (roDatabaseContextOperatorP <* eof) <|>
                    liftM GraphOp (transactionGraphOpP <* eof) <|>
                    liftM DatabaseExprOp (databaseExprOpP <* eof)

promptText :: DisconnectedTransaction -> TransactionGraph -> StringType
promptText (DisconnectedTransaction parentUUID _) graph = "TutorialD (" `T.append` transInfo `T.append` "): "
  where
    transInfo = case transactionForUUID parentUUID graph of
      Left _ -> "unknown"
      Right parentTrans -> case headNameForTransaction parentTrans graph of
          Nothing -> T.pack (show $ transactionUUID parentTrans)
          Just headName -> headName

interpret :: U.UUID -> DisconnectedTransaction -> TransactionGraph -> String -> (TutorialDOperatorResult, DisconnectedTransaction, TransactionGraph)
interpret freshUUID discon@(DisconnectedTransaction parentUUID context) graph inputString = case parse interpreterParserP "" inputString of
  Left err -> (DisplayErrorResult (T.pack $ show err), discon, graph)
  Right (RODatabaseContextOp execOp) -> (evalRODatabaseContextOp context execOp, discon, graph)
  Right (DatabaseExprOp execOp) -> case evalDatabaseExpr True context execOp of
                                        Left err -> (DisplayErrorResult $ T.pack (show err), discon, graph)
                                        Right context' -> (QuietSuccessResult, DisconnectedTransaction parentUUID context', graph)
  Right (GraphOp execOp) -> case evalGraphOp freshUUID discon graph execOp of
                                Left err -> (DisplayErrorResult $ T.pack (show err), discon, graph)
                                Right (discon', graph', result') -> (result', discon', graph')

reprLoop :: FilePath -> DisconnectedTransaction -> TransactionGraph -> IO ()
reprLoop transGraphDir currentTransaction graph = do
  homeDirectory <- getHomeDirectory
  let settings = defaultSettings {historyFile = Just (homeDirectory ++ "/.tutd_history")}

  maybeLine <- runInputT settings $ getInputLine (T.unpack (promptText currentTransaction graph))

  let roloop = reprLoop transGraphDir currentTransaction graph
  case maybeLine of
    Nothing -> return ()
    Just line -> do
    newUUID <- nextRandom
    case interpret newUUID currentTransaction graph line of
      (QuitResult, _, _) -> return ()
      (DisplayErrorResult err, _, _) -> TIO.hPutStrLn stderr ("ERROR: " `T.append` err) >> roloop
      (DisplayIOResult outio, updatedTrans, updatedGraph) -> do
        outio
        reprLoop transGraphDir updatedTrans updatedGraph        
      (DisplayResult out, updatedTrans, updatedGraph) -> do
        TIO.putStrLn out
        --save the current transaction graph
        transactionGraphPersist transGraphDir updatedGraph
        reprLoop transGraphDir updatedTrans updatedGraph
      (QuietSuccessResult, updatedTrans, updatedGraph) -> do
        --save the current transaction graph
        transactionGraphPersist transGraphDir updatedGraph        
        TIO.putStrLn "Done."        
        reprLoop transGraphDir updatedTrans updatedGraph

--used by :dumpGraph
{-
displayTransactionGraph :: TransactionGraph -> String
displayTransactionGraph (TransactionGraph _ transSet) = L.intercalate "\n" $ S.foldr (\(Transaction tUUID pUUID _ ) acc -> acc ++ [show tUUID ++ " " ++ show pUUID]) [] transSet
-}

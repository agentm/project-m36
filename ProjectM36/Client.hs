{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.Client
       (ConnectionInfo(..),
       Connection,
       Port,
       Hostname,
       DatabaseName,
       ConnectionError,
       connectProjectM36,
       disconnectedTransaction,
       transactionGraph,
       close,
       executeRelationalExpr,
       executeDatabaseContextExpr,
       executeGraphExpr,
       commit,
       rollback,
       processPersistence,
       PersistenceStrategy(..),
       RelationalExpr(..),
       DatabaseExpr(..),
       Attribute(..),
       attributesFromList,
       Attribute,
       AtomType(..)) where
import ProjectM36.Base
import ProjectM36.Error
import Control.Monad.State
import ProjectM36.Transaction
import ProjectM36.RelationalExpression
import ProjectM36.TransactionGraph
import ProjectM36.TransactionGraph.Persist
import ProjectM36.Attribute
import Data.UUID.V4 (nextRandom)
import Control.Concurrent.STM
import Data.Word

type DatabaseName = StringType       

type Hostname = StringType

type Port = Word16

data ConnectionInfo = InProcessConnectionInfo PersistenceStrategy 
                      --TCPRemoteConnectionInfo HeadName DatabaseName Hostname Port
                      
data Connection = InProcessConnection PersistenceStrategy (TVar (DisconnectedTransaction, TransactionGraph))  
                 -- TCPRemoteConnection
                  
data ConnectionError = SetupDatabaseDirectoryError PersistenceError 
                     deriving (Show, Eq)
                  
-- connect to an in-process database
connectProjectM36 :: ConnectionInfo -> IO (Either ConnectionError Connection)
--create a new in-memory database/transaction graph
connectProjectM36 (InProcessConnectionInfo strat) = do
  freshUUID <- nextRandom
  let bootstrapContext = basicDatabaseContext
      freshDiscon = newDisconnectedTransaction freshUUID bootstrapContext
      freshGraph = bootstrapTransactionGraph freshUUID bootstrapContext
  case strat of
    --create date examples graph for now- probably should be empty context in the future
    NoPersistence -> do
        tvar <- newTVarIO (freshDiscon, freshGraph)
        return $ Right $ InProcessConnection strat tvar
    MinimalPersistence dbdir -> do
      err <- setupDatabaseDir dbdir freshGraph 
      case err of
        Just err' -> return $ Left (SetupDatabaseDirectoryError err')
        Nothing -> do 
          graph <- transactionGraphLoad dbdir emptyTransactionGraph
          case graph of
            Left err' -> return $ Left (SetupDatabaseDirectoryError err')
            Right graph' -> do
              tvar <- newTVarIO (freshDiscon, graph')
              return $ Right $ InProcessConnection strat tvar
              
disconnectedTransaction :: Connection -> IO (DisconnectedTransaction)
disconnectedTransaction (InProcessConnection _ tvar) = liftM fst (readTVarIO tvar)

transactionGraph :: Connection -> IO (TransactionGraph)
transactionGraph (InProcessConnection _ tvar) = liftM snd (readTVarIO tvar)
                       
close :: Connection -> IO ()
close (InProcessConnection _ _) = return ()

executeRelationalExpr :: Connection -> RelationalExpr -> IO (Either RelationalError Relation)
executeRelationalExpr (InProcessConnection _ tvar) expr = atomically $ do
  ((DisconnectedTransaction _ context), _) <- readTVar tvar
  return $ evalState (evalRelationalExpr expr) context
  
executeDatabaseContextExpr :: Connection -> DatabaseExpr -> IO (Maybe RelationalError)
executeDatabaseContextExpr (InProcessConnection _ tvar) expr = atomically $ do
  ((DisconnectedTransaction parentUUID context), graph) <- readTVar tvar
  case runState (evalContextExpr expr) context of
       (Just err,_) -> return $ Just err
       (Nothing, context') -> do
         let newDiscon = DisconnectedTransaction parentUUID context'
         writeTVar tvar (newDiscon, graph)
         return Nothing
         
executeGraphExpr :: Connection -> TransactionGraphOperator -> IO (Maybe RelationalError)
executeGraphExpr (InProcessConnection strat tvar) graphExpr = do
  freshUUID <- nextRandom
  manip <- atomically $ do
    (discon, graph) <- readTVar tvar
    case evalGraphOp freshUUID discon graph graphExpr of
      Left err -> return $ Left err
      Right (discon', graph') -> do
        writeTVar tvar (discon', graph')
        return $ Right (discon', graph')
  case manip of 
    Left err -> return $ Just err
    Right (_, newGraph) -> do
      --update filesystem database, if necessary
      --this should really grab a lock at the beginning of the method to be threadsafe
      processPersistence strat newGraph
      return Nothing
      
commit :: Connection -> IO (Maybe RelationalError)
commit conn = executeGraphExpr conn Commit
          
rollback :: Connection -> IO (Maybe RelationalError)
rollback conn = executeGraphExpr conn Rollback      

processPersistence :: PersistenceStrategy -> TransactionGraph -> IO ()
processPersistence NoPersistence _ = return ()
processPersistence (MinimalPersistence dbdir) graph = transactionGraphPersist dbdir graph

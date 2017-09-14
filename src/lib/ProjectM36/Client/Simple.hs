{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ProjectM36.Client.Simple (
  simpleConnectProjectM36,
  withTransaction,
  execute,
  query,
  rollback,
  close,
  Atom(..),
  AtomType(..),
  DBError(..),
  Attribute(..),
  C.ConnectionInfo(..),
  C.PersistenceStrategy(..),
  C.NotificationCallback,
  C.emptyNotificationCallback,
  C.DatabaseContextExpr(..),
  C.RelationalExprBase(..)  
  ) where
-- | A simplified client interface for Project:M36 database access.
import ProjectM36.Error
import ProjectM36.Base
import qualified ProjectM36.Client as C
import Control.Monad.Reader
import Control.Exception.Base

type DBConn = (C.SessionId, C.Connection)

newtype DB a = DB {runDB :: ReaderT (C.SessionId, C.Connection) IO a}
  deriving (Functor, Applicative, Monad)
           
newtype TransactionCancelled = TransactionCancelled DBError deriving Show
instance Exception TransactionCancelled

-- | A simple alternative to 'connectProjectM36' which includes simple session management
simpleConnectProjectM36 :: C.ConnectionInfo -> IO (Either DBError DBConn)
simpleConnectProjectM36 connInfo = do
  eConn <- C.connectProjectM36 connInfo
  case eConn of
    Left err -> pure (Left (ConnError err))
    Right conn -> do
      eSess <- C.createSessionAtHead conn "master"
      case eSess of
        Left err -> do
          C.close conn
          pure (Left (RelError err))
        Right sess -> pure (Right (sess, conn))
        
close :: DBConn -> IO ()        
close (_ , conn) = C.close conn

-- | Runs an IO monad which may include some database updates. If an exception or error occurs, the transaction is rolled back. Otherwise, the transaction is committed.
withTransaction :: DBConn -> DB a -> IO (Either DBError a)
withTransaction (sess, conn) dbm = do
  eHeadName <- C.headName sess conn
  case eHeadName of 
    Left err -> pure (Left (RelError err))
    Right headName -> do
      let successFunc = C.autoMergeToHead sess conn UnionMergeStrategy headName
          block = runReaderT (runDB dbm) (sess, conn)
          handler :: TransactionCancelled -> IO (Either DBError a)
          handler (TransactionCancelled err) = pure (Left err)
      handle handler $ do
        ret <- C.withTransaction sess conn (block >>= pure . Right) successFunc
        case ret of 
          Left err -> pure (Left (RelError err))
          Right val -> pure (Right val)

-- | A union of connection and other errors that can be returned from 'withDBConnection'.
data DBError = ConnError C.ConnectionError |
               RelError RelationalError |
               TransactionRolledBack
               deriving (Eq, Show)

-- | Execute a 'DatabaseContextExpr' in the 'DB' monad. Database context expressions manipulate the state of the database. In case of an error, the transaction is terminated and the connection's session is rolled back.
execute :: C.DatabaseContextExpr -> DB ()
execute expr = DB $ do
  (sess, conn) <- ask
  ret <- liftIO $ C.executeDatabaseContextExpr sess conn expr
  case ret of
    Left err -> liftIO $ cancelTransaction (RelError err)
    Right _ -> pure ()
    
-- | Run a 'RelationalExpr' query in the 'DB' monad. Relational expressions perform read-only queries against the current database state.
query :: C.RelationalExpr -> DB Relation
query expr = DB $ do
  (sess, conn) <- ask  
  ret <- lift $ C.executeRelationalExpr sess conn expr
  case ret of
    Left err -> liftIO $ cancelTransaction (RelError err) >> error "impossible"
    Right rel -> pure rel

-- | Unconditionally roll back the current transaction and throw an exception to terminate the execution of the DB monad.
rollback :: DB ()
rollback = DB $ lift $ cancelTransaction TransactionRolledBack

cancelTransaction :: DBError -> IO ()
cancelTransaction err = throwIO (TransactionCancelled err)

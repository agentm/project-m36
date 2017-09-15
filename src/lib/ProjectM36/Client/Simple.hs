{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ProjectM36.Client.Simple (
  simpleConnectProjectM36,
  simpleConnectProjectM36At,
  withTransaction,
  withTransactionUsing,
  execute,
  executeOrErr,
  query,
  queryOrErr,
  rollback,
  close,
  Atom(..),
  AtomType(..),
  Db,
  DbConn,
  DbError(..),
  Attribute(..),
  C.ConnectionInfo(..),
  C.PersistenceStrategy(..),
  C.NotificationCallback,
  C.emptyNotificationCallback,
  C.DatabaseContextExpr(..),
  C.RelationalExprBase(..)
  ) where
-- | A simplified client interface for Project:M36 database access.
import Control.Exception.Base
import Control.Monad.Reader
import ProjectM36.Base
import qualified ProjectM36.Client as C
import ProjectM36.Error

type DbConn = (C.SessionId, C.Connection)

newtype Db a = Db {runDb :: ReaderT DbConn IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

newtype TransactionCancelled = TransactionCancelled DbError deriving Show
instance Exception TransactionCancelled

-- | A simple alternative to 'connectProjectM36' which includes simple session management.
simpleConnectProjectM36At :: HeadName -> C.ConnectionInfo -> IO (Either DbError DbConn)
simpleConnectProjectM36At headName connInfo = do
  eConn <- C.connectProjectM36 connInfo
  case eConn of
    Left err -> pure (Left (ConnError err))
    Right conn -> do
      eSess <- C.createSessionAtHead conn headName
      case eSess of
        Left err -> do
          C.close conn
          pure (Left (RelError err))
        Right sess -> pure (Right (sess, conn))

-- | Same as 'simpleConnectProjectM36At' but always connects to the @master@ branch.
simpleConnectProjectM36 :: C.ConnectionInfo -> IO (Either DbError DbConn)
simpleConnectProjectM36 = simpleConnectProjectM36At "master"

-- | Closes the database connection.
close :: DbConn -> IO ()
close (_ , conn) = C.close conn

-- | Runs a Db monad which may include some database updates. If an exception or error occurs, the transaction is rolled back. Otherwise, the transaction is committed to the head of the current branch.
withTransaction :: DbConn -> Db a -> IO (Either DbError a)
withTransaction sessconn = withTransactionUsing sessconn UnionMergeStrategy

-- | Same a 'withTransaction' except that the merge strategy can be specified.
withTransactionUsing :: DbConn -> MergeStrategy -> Db a -> IO (Either DbError a)
withTransactionUsing (sess, conn) strat dbm = do
  eHeadName <- C.headName sess conn
  case eHeadName of
    Left err -> pure (Left (RelError err))
    Right headName -> do
      let successFunc = C.autoMergeToHead sess conn strat headName
          block = runReaderT (runDb dbm) (sess, conn)
          handler :: TransactionCancelled -> IO (Either DbError a)
          handler (TransactionCancelled err) = pure (Left err)
      handle handler $ do
        ret <- C.withTransaction sess conn (block >>= pure . Right) successFunc
        case ret of
          Left err  -> pure (Left (RelError err))
          Right val -> pure (Right val)

-- | A union of connection and other errors that can be returned from 'withTransaction'.
data DbError = ConnError C.ConnectionError |
               RelError RelationalError |
               TransactionRolledBack
               deriving (Eq, Show)

-- | Execute a 'DatabaseContextExpr' in the 'DB' monad. Database context expressions manipulate the state of the database. In case of an error, the transaction is terminated and the connection's session is rolled back.
execute :: C.DatabaseContextExpr -> Db ()
execute expr = do
  ret <- executeOrErr expr
  case ret of
    Left err -> liftIO $ cancelTransaction (RelError err)
    Right _  -> pure ()

-- | Run a 'RelationalExpr' query in the 'DB' monad. Relational expressions perform read-only queries against the current database state.
query :: C.RelationalExpr -> Db Relation
query expr = do
  ret <- queryOrErr expr
  case ret of
    Left err  -> liftIO $ cancelTransaction (RelError err)
    Right rel -> pure rel

-- | Run a 'DatabaseContextExpr' update expression. If there is an error, just return it without cancelling the current transaction.
executeOrErr :: C.DatabaseContextExpr -> Db (Either RelationalError ())
executeOrErr expr = Db $ do
  (sess, conn) <- ask
  lift $ C.executeDatabaseContextExpr sess conn expr

-- | Run a 'RelationalExpr' query expression. If there is an error, just return it without cancelling the transaction.
queryOrErr :: C.RelationalExpr -> Db (Either RelationalError Relation)
queryOrErr expr = Db $ do
  (sess, conn) <- ask
  lift $ C.executeRelationalExpr sess conn expr

-- | Unconditionally roll back the current transaction and throw an exception to terminate the execution of the Db monad.
rollback :: Db ()
rollback = Db $ lift $ cancelTransaction TransactionRolledBack

cancelTransaction :: DbError -> IO a
cancelTransaction err = throwIO (TransactionCancelled err)

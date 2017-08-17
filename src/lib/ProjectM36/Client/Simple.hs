{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ProjectM36.Client.Simple (
  withDBConnection,
  withTransaction,
  execute,
  query,
  rollback,
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
-- | A simplified, monad-based client for Project:M36 database access.
import ProjectM36.Error
import ProjectM36.Base
import qualified ProjectM36.Client as C
import Control.Monad.Reader
import Control.Exception.Base

-- | A union of connection and other errors that can be returned from 'withDBConnection'.
data DBError = ConnError C.ConnectionError |
               RelError RelationalError
               deriving (Eq, Show)

newtype DB a = DB {runDB :: ReaderT (C.SessionId, C.Connection) IO a}
  deriving (Functor, Applicative, Monad)

-- | Opens a connection in the 'DB' monad and closes it after running the monad.
withDBConnection :: C.ConnectionInfo -> DB (Either DBError a) -> IO (Either DBError a)
withDBConnection connInfo block = bracket (C.connectProjectM36 connInfo) (\eConn -> case eConn of
                                                                             Left _ -> pure (Right ())
                                                                             Right conn -> C.close conn >> pure (Right ())) $ \eConn -> do
  case eConn of
    Left err -> pure (Left (ConnError err))
    Right conn -> do
      eSess <- C.createSessionAtHead conn "master"
      case eSess of
        Left err -> pure (Left (RelError err))
        Right session ->
           runReaderT (runDB block) (session, conn)
           
wrapClientIO :: (C.SessionId -> C.Connection -> IO (Either RelationalError a)) -> DB (Either DBError a)
wrapClientIO func = DB $ do
  (sess, conn) <- ask
  ret <- liftIO (func sess conn)
  case ret of
    Left err -> pure (Left (RelError err))
    Right val -> pure (Right val)
           
-- | Atomically commits the result of the database context expressions to disk on the server or restores the initial state.
withTransaction :: [C.DatabaseContextExpr] -> DB (Either DBError ())           
withTransaction exprs = do
  ret <- execute (MultipleExpr exprs)
  case ret of 
    Left err -> pure (Left err)
    Right _ -> do
      eCurrentHead <- wrapClientIO C.headName
      case eCurrentHead of
        Left err -> pure (Left err)
        Right currentHead -> do
          --only add a commit if something actually changed
          eIsDirty <- wrapClientIO C.disconnectedTransactionIsDirty
          case eIsDirty of
            Left err -> pure (Left err)
            Right isDirty -> 
              if isDirty then
                wrapClientIO (\sess conn -> C.autoMergeToHead sess conn C.UnionMergeStrategy currentHead)
              else
                pure (Right ())
           
-- | Execute a 'DatabaseContextExpr' in the 'DB' monad. Database context expressions manipulate the state of the database.
execute :: C.DatabaseContextExpr -> DB (Either DBError ())
execute expr = wrapClientIO (\sess conn -> C.executeDatabaseContextExpr sess conn expr)
    
-- | Run a 'RelationalExpr' query in the 'DB' monad. Relational expressions perform read-only queries against the current database state.
query :: C.RelationalExpr -> DB (Either DBError Relation)    
query expr = wrapClientIO (\sess conn -> C.executeRelationalExpr sess conn expr)

rollback :: DB (Either DBError ())
rollback = wrapClientIO C.rollback
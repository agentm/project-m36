{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ProjectM36.Client.Transaction
  ( Transaction
  , TransactionError(..)
  , TransactionCanceled(..)
  , TransactionCancelReasonUnknown(..)
  , runTransactionAtHead
  , runTransactionWithRebase
  , execute, execute'
  , executeIO, executeIO'
  , query, query'
  , rollback, rollbackWith, maybeRollback, eitherRollback
  ) where

import           Control.Exception    (Exception, SomeException, catch, throwIO,
                                       toException)
import           Control.Monad.Reader
import           Data.Typeable        (Typeable)
import           ProjectM36.Base      (DatabaseContextIOExpr, Relation)
import           ProjectM36.Client    hiding (rollback)
import           ProjectM36.Error     (MergeError (DisconnectedTransactionNotAMergeHeadError),
                                       RelationalError (MergeTransactionError))

newtype Transaction a = Transaction (ReaderT (Connection, SessionId) IO a)
  deriving (Functor, Applicative, Monad)

data TransactionError = TransactionRelationalError RelationalError
                      | TransactionCanceledError SomeException
                      | TransactionRetriesExhausted
                      deriving (Show, Typeable)
instance Exception TransactionError

newtype TransactionCanceled = TransactionCanceled SomeException deriving (Show, Typeable)
instance Exception TransactionCanceled

data TransactionCancelReasonUnknown = TransactionCancelReasonUnknown deriving (Show, Typeable)
instance Exception TransactionCancelReasonUnknown


runTransactionAtHead :: HeadName -> Connection -> Transaction a -> IO (Either TransactionError a)
runTransactionAtHead branch conn trx = do
  errSess <- createSessionAtHead branch conn
  case errSess of
    Left err -> pure $ Left (TransactionRelationalError err)
    Right sess -> do
        a <- addTransactionToSession sess conn trx
        maybe (Right a) (Left . TransactionRelationalError) <$> commit sess conn
      `catch` \(TransactionCanceled e) -> pure (Left $ TransactionCanceledError e)

runTransactionWithRebase :: Int -> HeadName -> Connection -> Transaction a -> IO (Either TransactionError a)
runTransactionWithRebase tries branch conn trx
  | tries <= 0 = pure (Left TransactionRetriesExhausted)
  | otherwise = do
    errResult <- runTransactionAtHead branch conn trx
    case errResult of
      Left err -> case err of
        (TransactionRelationalError (
          MergeTransactionError
            (DisconnectedTransactionNotAMergeHeadError _)))
              -> runTransactionWithRebase (tries - 1) branch conn trx
        _ -> pure (Left err)
      Right a -> pure (Right a)

execute :: DatabaseContextExpr -> Transaction (Maybe RelationalError)
execute = liftToTransaction executeDatabaseContextExpr

executeIO :: DatabaseContextIOExpr -> Transaction (Maybe RelationalError)
executeIO = liftToTransaction executeDatabaseContextIOExpr

query :: RelationalExpr -> Transaction (Either RelationalError Relation)
query = liftToTransaction executeRelationalExpr

execute' :: DatabaseContextExpr -> Transaction ()
execute' expr = maybeRollback =<< execute expr

executeIO' :: DatabaseContextIOExpr -> Transaction ()
executeIO' expr = maybeRollback =<< executeIO expr

query' :: RelationalExpr -> Transaction Relation
query' expr = eitherRollback =<< query expr

rollbackWith :: Exception e => e -> Transaction a
rollbackWith e = Transaction (liftIO $ throwIO $ TransactionCanceled $ toException e)

rollback :: Transaction a
rollback = rollbackWith TransactionCancelReasonUnknown

maybeRollback :: Maybe RelationalError -> Transaction ()
maybeRollback = maybe (pure ()) (rollbackWith . TransactionRelationalError)

eitherRollback :: Either RelationalError a -> Transaction a
eitherRollback = either (rollbackWith . TransactionRelationalError) pure


-- Helpers

addTransactionToSession :: SessionId -> Connection -> Transaction a -> IO a
addTransactionToSession sess conn (Transaction act) = runReaderT act (conn, sess)

liftToTransaction :: (SessionId -> Connection -> expr -> IO a) -> expr -> Transaction a
liftToTransaction runner expr = Transaction $ do
  (conn, sess) <- ask
  liftIO $ runner sess conn expr

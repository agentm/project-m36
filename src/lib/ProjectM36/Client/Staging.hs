{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ProjectM36.Client.Staging
  ( Staging
  , CommitError(..)
  , StagingCanceled(..)
  , StagingCancelReasonUnknown(..)
  , commitStaging
  , commitStagingRebase
  , execute, execute'
  , executeIO, executeIO'
  , query, query'
  , cancel, cancelWith, maybeCancel, leftCancel
  ) where

import           Control.Exception    (Exception, SomeException, catch, throwIO,
                                       toException)
import           Control.Monad.Reader
import           Data.Typeable        (Typeable)
import           ProjectM36.Base      (Relation)
import           ProjectM36.Client
import           ProjectM36.Error     (MergeError (DisconnectedTransactionNotAMergeHeadError),
                                       RelationalError (MergeTransactionError))

-- | A monad for safely composing staged changes that can be committed to
--   a transaction. This monad does not permit @IO@ because it is possible
--   for the whole action to be tried multiple times when committing.
newtype Staging a = Staging (ReaderT (Connection, SessionId) IO a)
  deriving (Functor, Applicative, Monad)

-- | An error type enumerating possible causes of commit failure.
data CommitError = CommitRelationalError RelationalError
                 | CommitStagingCanceled SomeException
                 | CommitRetriesExhausted
                 deriving (Show, Typeable)
instance Exception CommitError

newtype StagingCanceled = StagingCanceled SomeException deriving (Show, Typeable)
instance Exception StagingCanceled

data StagingCancelReasonUnknown = StagingCancelReasonUnknown deriving (Show, Typeable)
instance Exception StagingCancelReasonUnknown

-- | Commits staged changes to the head of a transaction graph.
--   This operation can fail if
--
--     * The staging session is canceled.
--     * The transaction graph advances before the
--       commit is able to complete. (To handle that case automatically,
--       use @commitStagingRebase@.)
--     * The commit creates a merge conflict.
commitStaging :: HeadName -> Connection -> Staging a -> IO (Either CommitError a)
commitStaging branch conn trx = do
  errSess <- createSessionAtHead branch conn
  case errSess of
    Left err -> pure $ Left (CommitRelationalError err)
    Right sess -> do
        a <- addStagingToSession sess conn trx
        maybe (Right a) (Left . CommitRelationalError) <$> commit sess conn
      `catch` \(StagingCanceled e) -> pure (Left $ CommitStagingCanceled e)

-- | Commits staged changes to the head of a transaction graph and rebases
--   if necessary. Merge conflicts are still possible.
commitStagingRebase :: Int -- ^ Maximum number of times to try the commit
                    -> HeadName
                    -> Connection -> Staging a -> IO (Either CommitError a)
commitStagingRebase tries branch conn trx
  | tries <= 0 = pure (Left CommitRetriesExhausted)
  | otherwise = do
    errResult <- commitStaging branch conn trx
    case errResult of
      Left err -> case err of
        (CommitRelationalError (
          MergeTransactionError
            (DisconnectedTransactionNotAMergeHeadError _)))
              -> commitStagingRebase (tries - 1) branch conn trx
        _ -> pure (Left err)
      Right a -> pure (Right a)

execute :: DatabaseContextExpr -> Staging (Maybe RelationalError)
execute = liftToStaging executeDatabaseContextExpr

executeIO :: DatabaseContextIOExpr -> Staging (Maybe RelationalError)
executeIO = liftToStaging executeDatabaseContextIOExpr

query :: RelationalExpr -> Staging (Either RelationalError Relation)
query = liftToStaging executeRelationalExpr

-- | Like @execute@ but cancels the staging session immediately in case of error.
execute' :: DatabaseContextExpr -> Staging ()
execute' expr = maybeCancel =<< execute expr

-- | Like @executeIO@ but cancels the staging session immediately in case of error.
executeIO' :: DatabaseContextIOExpr -> Staging ()
executeIO' expr = maybeCancel =<< executeIO expr

-- | Like @query@ but cancels the staging session immediately in case of error.
query' :: RelationalExpr -> Staging Relation
query' expr = leftCancel =<< query expr

-- | Cancels a staging session and reports the reason via the given exception.
cancelWith :: Exception e => e -> Staging a
cancelWith e = Staging (liftIO $ throwIO $ StagingCanceled $ toException e)

-- | Cancels a staging session with a default reason of "unknown".
cancel :: Staging a
cancel = cancelWith StagingCancelReasonUnknown

-- | Helper function to cancel when an error occurs (via Just error)
maybeCancel :: Maybe RelationalError -> Staging ()
maybeCancel = maybe (pure ()) (cancelWith . CommitRelationalError)

-- | Helper function to cancel when an error occurs (via Left error)
leftCancel :: Either RelationalError a -> Staging a
leftCancel = either (cancelWith . CommitRelationalError) pure


-- Helpers

addStagingToSession :: SessionId -> Connection -> Staging a -> IO a
addStagingToSession sess conn (Staging act) = runReaderT act (conn, sess)

liftToStaging :: (SessionId -> Connection -> expr -> IO a) -> expr -> Staging a
liftToStaging runner expr = Staging $ do
  (conn, sess) <- ask
  liftIO $ runner sess conn expr

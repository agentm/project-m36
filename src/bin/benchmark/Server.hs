{-# LANGUAGE DerivingVia, DeriveGeneric #-}
import ProjectM36.Tupleable
import ProjectM36.Client

import Criterion.Main
import Codec.Winery
import Data.Text (Text)
import Data.Proxy
import GHC.Generics
import Control.Monad
import System.Random

handleIOError :: Show e => IO (Either e a) -> IO a
handleIOError m = do
  v <- m
  handleError v

handleError :: Show e => Either e a -> IO a
handleError eErr = case eErr of
    Left err -> print err >> error "Died due to errors."
    Right v -> pure v

--test local connection speeds of inserts, updates, and deletes to look for space leaks, etc.
main :: IO ()
main = do
  rando <- initStdGen
  conn <- handleIOError $ connectProjectM36 (InProcessConnectionInfo NoPersistence emptyNotificationCallback [] basicDatabaseContext rando adminRoleName)
  sess <- handleIOError $ createSessionAtHead conn "master"
  _ <- handleIOError $ executeDatabaseContextExpr sess conn (toDefineExpr (Proxy :: Proxy User) "user")

  let count = 100
  insertUsers sess conn count
  defaultMain [
    bgroup "inserts" [
        bench "insert" $ nfIO $ insertUsers sess conn count]
    {-,bgroup "updates" [
        bench "update" $ nfIO $ updateUsers sess conn count
        ]-}
    ,bgroup "delete" [
        bench "delete" $ nfIO $ deleteUsers sess conn count
        ]
    ]


insertUsers :: SessionId -> Connection -> Int -> IO ()
insertUsers sess conn count = 
  forM_ [1 .. count] $ \newId -> do
    let newUser = User { userId = newId
                       , userFirstName = "Steve"
                       , userLastName = "Stevens"
                       , userEmail = "bench@bench.com"
                       }
    newUserExpr <- handleError (toInsertExpr [newUser] "user")
    handleIOError $ executeDatabaseContextExpr sess conn newUserExpr

{-
updateUsers :: SessionId -> Connection -> Int -> IO ()
updateUsers sess conn count =
  forM_ [1 .. count] $ \uid -> do
    let changeUser = User { userId = uid
                          , userFirstName = "Steve"
                          , userLastName = "Stevens III"
                          , userEmail = "bench@bench.com"
                          }
        updateExpr = toUpdateExpr "user" ["userId"] changeUser
    updateUserExpr <- handleError updateExpr
    handleIOError $ executeDatabaseContextExpr sess conn updateUserExpr
-}
deleteUsers :: SessionId -> Connection -> Int -> IO ()
deleteUsers sess conn count =
  forM_ [1 .. count] $ \uid -> do
    let delUser = User { userId = uid
                       , userFirstName = "Steve"
                       , userLastName = "Stevens III"
                       , userEmail = "bench@bench.com"
                       }
    delUserExpr <- handleError (toDeleteExpr "user" ["userId"] delUser)
    handleIOError $ executeDatabaseContextExpr sess conn delUserExpr

data User = User
          { userId :: Int
          , userFirstName :: Text
          , userLastName :: Text
          , userEmail :: Text
          }
          deriving Generic
          deriving Serialise via WineryRecord User

instance Tupleable User

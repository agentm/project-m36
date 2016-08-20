{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, GADTs, EmptyDataDecls, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, RankNTypes, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- A simple example of using the persistent-ProjectM36 driver. -}
import Database.Persist.ProjectM36 hiding (executeDatabaseContextExpr)
import ProjectM36.Client
import ProjectM36.DataTypes.Primitive
import System.Exit
import System.IO
import Database.Persist
import Database.Persist.TH
import Language.Haskell.TH.Syntax
import Control.Monad.Reader

--1. declare the models with Template Haskell
let projectM36PersistSettings = mkPersistSettings (ConT ''ProjectM36Backend)
 in
  share [mkPersist projectM36PersistSettings] [persistLowerCase|
Person
    name String Eq
    age Int
    UniqueName name
    deriving Show
|]

errDie :: String -> IO ()
errDie out = hPutStrLn stderr out >> exitFailure

defPersonRel :: SessionId -> Connection -> IO ()
defPersonRel sessionId conn = do
  let attrList = [Attribute "name" TextAtomType, Attribute "age" IntAtomType, Attribute "id" TextAtomType]
  maybeErr <- executeDatabaseContextExpr sessionId conn (Define "person" (map NakedAttributeExpr attrList))
  case maybeErr of
    Nothing -> return ()
    Just err -> errDie (show err)

main :: IO ()
main = do
  --1. create the database connection
  let connInfo = InProcessConnectionInfo NoPersistence emptyNotificationCallback
  withProjectM36Conn connInfo $ runProjectM36Conn $ do
    --2. use the underlying client connection and session to define a relation variable
    (sessionId, conn) <- ask
    liftIO $ defPersonRel sessionId conn
    --3. insert some data
    _ <- insert $ Person "Michael" 33
    _ <- insert $ Person "Steve" 20
    --4. query on a unique key
    michael <- getBy $ UniqueName "Michael"
    liftIO $ putStrLn (show michael)
    --5. other queries
    youngerThan30Users <- selectList [PersonAge <. 30] []
    liftIO $ putStrLn (show youngerThan30Users)
    -- Updates and deletes operate by normal persistent APIs.
    -- Sorting results is not yet supported.
    

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import Database.Persist.ProjectM36
import ProjectM36.Base --shouldn't be here
import ProjectM36.Attribute --useful functions should be exported through the Client interface
import ProjectM36.Client
import qualified ProjectM36.Client as C
import System.Exit
import Database.Persist
import Database.Persist.TH
import Language.Haskell.TH.Syntax
import Control.Monad.Reader

let projectM36PersistSettings = mkPersistSettings (ConT ''ProjectM36Backend)
 in
  share [mkPersist projectM36PersistSettings] [persistLowerCase|
Person
    name String Eq
    age Int
    UniqueName name
    deriving Show
|]

testList :: Test
testList = TestList $ map mkInProcessTest [testGetBy, testDeleteBy]

main :: IO ()
main = do 
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

inProcSettings :: ConnectionInfo
inProcSettings = InProcessConnectionInfo NoPersistence

defPersonRel :: Connection -> IO ()
defPersonRel conn = do
    maybeErr <- C.executeDatabaseContextExpr conn (Define "person" (attributesFromList [Attribute "name" StringAtomType, Attribute "age" IntAtomType, Attribute "id" StringAtomType]))
    case maybeErr of
        Nothing -> return ()
        Just err -> assertFailure $ "defPersonRel" ++ show err

mkInProcessTest :: ReaderT C.Connection IO () -> Test
mkInProcessTest m = TestCase $ do
        withProjectM36Conn inProcSettings $ runProjectM36Conn $ do
            conn <- ask
            liftIO (defPersonRel conn)
            m
            
-- insert a person, getBy to check he is there
testGetBy :: ReaderT ProjectM36Backend IO ()
testGetBy = do
    let name = "Michael"
        age = 26
    m <- insert $ Person name age
    michael <- getBy $ UniqueName name
    case michael of
       Nothing -> liftIO $ assertFailure "no person found"
       Just michael' -> do
           liftIO $ assertEqual "name equality" (personName $ entityVal michael') name
           liftIO $ assertEqual "age equality" (personAge $ entityVal michael') age              
    return ()

--insert two people, delete one, check one remains
testDeleteBy :: ReaderT ProjectM36Backend IO ()
testDeleteBy = do
    let name1 = "Michael"
        age1 = 26
        name2 = "John"
        age2 = 30
    m1 <- insert $ Person name1 age1
    m2 <- insert $ Person name2 age2
    deleteBy $ UniqueName name1 --delete michael
    nom1 <- getBy $ UniqueName name1
    --check that m1 was deleted
    case nom1 of
        Just m -> liftIO $ assertFailure "deleteBy failure"
        Nothing -> return ()
    --check that m2 still exists
    m2check <- getBy $ UniqueName name2
    case m2check of
       Nothing -> liftIO $ assertFailure "deleteBy failure2"
       Just m -> return ()
    return ()
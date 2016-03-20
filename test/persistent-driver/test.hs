{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
import Test.HUnit
import Database.Persist.ProjectM36
import ProjectM36.Client
import ProjectM36.DataTypes.Primitive
import qualified ProjectM36.Client as C
import System.Exit
import Database.Persist
import Database.Persist.TH
import Language.Haskell.TH.Syntax
import Control.Monad.Reader
import Control.Exception (throwIO)

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
testList = TestList $ map mkInProcessTest [testGetBy, testDelete, testUpdate, testCount, testSelect, testSelectKeys]

main :: IO ()
main = do
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

inProcSettings :: ConnectionInfo
inProcSettings = InProcessConnectionInfo NoPersistence emptyNotificationCallback

defPersonRel :: SessionId -> Connection -> IO ()
defPersonRel sessionId conn = do
    let textTypCons = PrimitiveTypeConstructor "Text" textAtomType
        intTypCons = PrimitiveTypeConstructor "Int" intAtomType

    maybeErr <- C.executeDatabaseContextExpr sessionId conn (Define "person" [
                                                                AttributeAndTypeNameExpr "name" textTypCons,
                                                                AttributeAndTypeNameExpr "age" intTypCons,
                                                                AttributeAndTypeNameExpr "id" textTypCons])
    case maybeErr of
        Nothing -> return ()
        Just err -> assertFailure $ "defPersonRel" ++ show err

mkInProcessTest :: ReaderT ProjectM36Backend IO () -> Test
mkInProcessTest m = TestCase $ do
        withProjectM36Conn inProcSettings $ runProjectM36Conn $ do
            (sessionId, conn) <- ask
            liftIO (defPersonRel sessionId conn)
            m

entityFromMaybe :: (PersistEntity val) => Maybe (Entity val) -> IO (Entity val)
entityFromMaybe may = case may of
    Nothing -> assertFailure "expected entity not found" >> throwIO (PersistError "not found")
    Just ent -> return ent

valFromMaybe :: (PersistEntity val) => Maybe val -> IO val
valFromMaybe may = case may of
    Nothing -> assertFailure "expected value not found" >> throwIO (PersistError "not found")
    Just val -> return val
            
-- insert a person, getBy to check he is there
testGetBy :: ReaderT ProjectM36Backend IO ()
testGetBy = do
    let name = "Michael"
        age = 26
    _ <- insert $ Person name age
    michael <- getBy $ UniqueName name
    michael' <- liftIO $ entityFromMaybe michael
    liftIO $ assertEqual "name equality" name (personName $ entityVal michael')
    liftIO $ assertEqual "age equality" age (personAge $ entityVal michael')
    return ()

--insert two people, delete one, check one remains
testDelete :: ReaderT ProjectM36Backend IO ()
testDelete = do
    let name1 = "Michael"
        age1 = 26
        name2 = "John"
        age2 = 30
    _ <- insert $ Person name1 age1
    m2 <- insert $ Person name2 age2
    deleteBy $ UniqueName name1 --delete michael
    nom1 <- getBy $ UniqueName name1
    --check that m1 was deleted
    case nom1 of
        Just _ -> liftIO $ assertFailure "deleteBy failure"
        Nothing -> return ()
    --check that m2 still exists
    m2check <- getBy $ UniqueName name2
    case m2check of
       Nothing -> liftIO $ assertFailure "deleteBy failure2"
       Just _ -> return ()
    --delete m2
    deleteWhere [PersonName ==. name2]
    m2del <- get m2
    case m2del of
      Just _ -> liftIO $ assertFailure "deleteBy failure m2"
      Nothing -> return ()
    return ()

--insert two people, update one-at-a-time, confirm update
testUpdate :: ReaderT ProjectM36Backend IO ()
testUpdate = do
    let name1 = "Michael"
        age1 = 26
        name2 = "John"
        age2 = 30
        name3 = "Dracula"
    m1 <- insert $ Person name1 age1
    m2 <- insert $ Person name2 age2
    update m1 [PersonAge =. 35] -- update m1's age
    updateWhere [PersonName ==. name2] [PersonName =. name3] --update m2's name
    m1up <- get m1
    m1up' <- liftIO $ valFromMaybe m1up
    m2up <- get m2
    m2up' <- liftIO $ valFromMaybe m2up
    liftIO $ assertEqual "m1 update" name1 (personName m1up')
    liftIO $ assertEqual "m1 update" 35 (personAge m1up')
    liftIO $ assertEqual "m2 update" name3 (personName m2up')
    liftIO $ assertEqual "m2 update" age2 (personAge m2up')
    return ()

testCount :: ReaderT ProjectM36Backend IO ()
testCount = do
    let name1 = "Michael"
        age1 = 26
        name2 = "John"
        age2 = 30
    _ <- insert $ Person name1 age1
    _ <- insert $ Person name2 age2
    c1 <- count [PersonAge ==. 26]
    liftIO $ assertEqual "count age" 1 c1
    c2 <- count [PersonAge !=. 400]
    liftIO $ assertEqual "count age2" 2 c2

--currently, selection only supports filter/restriction and not sorting
--insert two people, test that restriction locates each and both
testSelect :: ReaderT ProjectM36Backend IO ()
testSelect = do
    let name1 = "Michael"
        age1 = 26
        name2 = "John"
        age2 = 30
    _ <- insert $ Person name1 age1
    _ <- insert $ Person name2 age2    
    name1List <- selectList [PersonName ==. name1] []
    liftIO $ assertEqual "select name1 len" 1 (length name1List)
    liftIO $ assertEqual "select name1 val" name1 ((personName . entityVal . head) name1List)
    namesList <- selectList [] []
    liftIO $ assertEqual "select names len" 2 (length (namesList :: [Entity Person]))

testSelectKeys :: ReaderT ProjectM36Backend IO ()
testSelectKeys = do
    let name1 = "Michael"
        age1 = 26
        name2 = "John"
        age2 = 30
    _ <- insert $ Person name1 age1
    m2 <- insert $ Person name2 age2
    keysList <- selectKeysList [PersonAge ==. age2] []
    liftIO $ assertEqual "select keysList len" 1 (length keysList)
    liftIO $ assertEqual "select keysList val" m2 (head keysList)
    
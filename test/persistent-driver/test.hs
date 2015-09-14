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
testList = TestList [testBasicInProcessConnection]

main :: IO ()
main = do 
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

testBasicInProcessConnection :: Test
testBasicInProcessConnection = TestCase $ do
    let connSettings = InProcessConnectionInfo NoPersistence
    withProjectM36Conn connSettings $ runProjectM36Conn $ do
        conn <- ask
        _ <- liftIO $ C.executeDatabaseContextExpr conn (Define "person" (attributesFromList [Attribute "name" StringAtomType, Attribute "age" IntAtomType, Attribute "id" StringAtomType]))
        m <- insert $ Person "Michael" 26
        michael <- getBy $ UniqueName "Michael"
        liftIO $ putStrLn (show michael)
        liftIO $ assertBool "poop" False
        return ()
                             
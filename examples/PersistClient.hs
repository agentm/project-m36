{-# LANGUAGE TemplateHaskell #-}
{- A simple example of using the persistent-ProjectM36 driver. -}
import Database.Persist.ProjectM36
import ProjectM36.Client

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

main :: IO ()
main = do
  --1. create the database connection
  pure ()
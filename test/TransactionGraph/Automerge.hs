import Test.HUnit
import ProjectM36.Client
import TutorialD.Interpreter.TestBase
import Data.UUID.V4 (nextRandom)

import System.Exit

main :: IO ()           
main = do 
  tcounts <- runTestTT testList
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess
  
testList :: Test
testList = TestList [testAutomergeSuccess,
                     testAutomergeFailure]

testAutomergeSuccess :: Test
testAutomergeFailure = TestCase $ do
  (sessionId, conn) <- dateExamplesConnection
  sessionPastId <- createSessionAtHead "master"
  executeTutorialD sessionId conn "insert s relation{tuple{city \"New City\", s# "S6", sname "Samuels", status 50}}"
  commit sessionId conn
  
  

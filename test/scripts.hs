-- read each .tutd file in the /scripts directory and execute it with the tutd interpreter
import Test.HUnit
import TutorialD.Interpreter.Import.TutorialD
import System.Directory
import Data.List (isSuffixOf)
import System.Exit

testList :: IO Test
testList = do
  let scriptsDir = "scripts/"
  dirFiles <- getDirectoryContents scriptsDir
  let scriptList = map (scriptsDir ++) $ filter (".tutd" `isSuffixOf`) dirFiles 
  pure (TestList $ map testScript scriptList)

main :: IO ()
main = do
  tests <- testList
  tcounts <- runTestTT tests
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess
  
testScript :: FilePath -> Test
testScript tutdFile = TestCase $ do
  eImport <- importTutorialD tutdFile 
  case eImport of
    Left err -> assertFailure ("tutd import failure in " ++ tutdFile ++ ": " ++ show err)
    Right _ -> pure ()
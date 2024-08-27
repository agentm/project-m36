{-# LANGUAGE CPP #-}
import TutorialD.Interpreter
import ProjectM36.Cli
import ProjectM36.DatabaseContext
import System.Directory
import System.FilePath

#if !defined(VERSION_project_m36) 
# error Failed to discover proper version from cabal_macros.h
# define VERSION_project_m36 "<unknown>"
#endif

printWelcome :: IO ()
printWelcome = do
  putStrLn $ "Project:M36 TutorialD Interpreter " ++ VERSION_project_m36
  putStrLn "Type \":help\" for more information."
  putStrLn "A full tutorial is available at:"
  putStrLn "https://github.com/agentm/project-m36/blob/master/docs/tutd_tutorial.markdown"

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  mainLoop printWelcome (homeDir </> ".tutd_history") runTutorialD promptText runTutorialD basicDatabaseContext

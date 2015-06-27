{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.TransactionGraph.Persist where
import ProjectM36.Base
import qualified Data.Binary as B
import System.Directory
import System.FilePath
import Data.List
import qualified Data.UUID as U
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.HashSet as HS
import ProjectM36.Transaction
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T

{-
The "m36v1" file at the top-level of the destination directory contains the the transaction graph as a set of UUIDs referencing their parents (1 or more)
Each Transaction is written to it own directory named by its UUID. Partially written transactions UUIDs are prefixed with a "." to indicate incompleteness in the graph.

-}

--as a first pass, just write to the directory as if it's empty
--next pass, make the writing incremental by only writing new transactions at the heads
transactionGraphPersist :: FilePath -> TransactionGraph -> IO ()
transactionGraphPersist destDirectory graph = do
  mapM_ (writeTransaction destDirectory) $ S.toList (transactionsForGraph graph)
  writeGraphUUIDFile destDirectory graph
  return ()
  
writeGraphUUIDFile :: FilePath -> TransactionGraph -> IO ()
writeGraphUUIDFile destDirectory (TransactionGraph _ transSet) = writeFile graphFile uuidInfo 
  where
    graphFile = destDirectory </> "m36v1"
    uuidInfo = intercalate "\n" graphLines
    graphLines = S.toList $ S.map graphLine transSet 
    graphLine trans = U.toString (transactionUUID trans) ++ " " ++ intercalate " " (S.toList (S.map U.toString $ transactionParentUUIDs trans))
    
readGraphUUIDFile :: FilePath -> IO ()
readGraphUUIDFile destDirectory = undefined

readTransaction :: FilePath -> U.UUID -> IO ()
readTransaction destDirectory transUUID = undefined

writeTransaction :: FilePath -> Transaction -> IO ()
writeTransaction destDirectory trans = do
  let tempDestDir = takeDirectory destDirectory </> "." ++ takeFileName destDirectory
      relvarsDir = tempDestDir </> "relvars"
      incDepsDir = tempDestDir </> "incdeps"
      atomFuncsDir = tempDestDir </> "atomfuncs"
      context = transactionContext trans
  --create sub directories
  mapM_ createDirectory [tempDestDir, relvarsDir, incDepsDir, atomFuncsDir]
  --write relvars
  mapM_ (writeRelVar relvarsDir) $ M.toList (relationVariables context)
  --write inclusion dependencies
  mapM_ (writeIncDep incDepsDir) $ M.toList (inclusionDependencies context)
  --write atom functions
  mapM_ (writeAtomFunc atomFuncsDir) $ HS.toList (atomFunctions context)
  --move the temp directory to final location
  renameDirectory tempDestDir destDirectory
  
writeRelVar :: FilePath -> (RelVarName, Relation) -> IO ()
writeRelVar relvarDestDir (relvarName, rel) = do
  let relvarPath = relvarDestDir </> T.unpack relvarName
  BS.writeFile relvarPath (B.encode rel)

--to write the atom functions, we really some bytecode to write (GHCi bytecode?)
writeAtomFunc :: FilePath -> AtomFunction -> IO ()
writeAtomFunc atomFuncDestDir func = do
  BS.appendFile (atomFuncDestDir </> "atomFuncs") $ BS.fromStrict (TE.encodeUtf8 (atomFuncName func `T.append` "\n"))
  
writeIncDep :: FilePath -> (IncDepName, InclusionDependency) -> IO ()  
writeIncDep incDepDir (incDepName, incDep) = do
  BS.writeFile (incDepDir </> T.unpack incDepName) $ B.encode incDep
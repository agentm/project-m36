{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.DateExamples
import ProjectM36.Error
import qualified ProjectM36.Attribute as A
import qualified Data.Text as T
import ProjectM36.Relation.Show.CSV
import ProjectM36.Relation.Show.HTML
import TutorialD.Interpreter.DatabaseContextExpr (interpretDatabaseContextExpr)
import ProjectM36.RelationalExpression
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.IntMap as IM
import qualified Data.Hashable as Hash
import qualified Data.Vector as V
import Options.Applicative
import qualified Data.Map as M
import qualified Data.Text.IO as TIO
import System.IO
import Control.Monad.State
import Control.DeepSeq
import Data.Text hiding (map)
import Data.Monoid

dumpcsv :: Relation -> IO ()
dumpcsv rel = case relationAsCSV rel of
  Left err -> hPutStrLn stderr (show err)
  Right bsData -> BS.putStrLn bsData

data BigrelArgs = BigrelArgs Int Int Text

parseAttributeCount :: Parser Int
parseAttributeCount = option auto (short 'a' <> long "attribute-count")

parseTupleCount :: Parser Int
parseTupleCount = option auto (short 't' <> long "tuple-count")

parseTutD :: Parser String
parseTutD = strOption (short 'd' <> long "tutoriald")

parseArgs :: Parser BigrelArgs
parseArgs =  BigrelArgs <$> parseAttributeCount <*> parseTupleCount <*> (pack <$> parseTutD)

main :: IO ()
main = do
  bigrelArgs <- execParser $ info (helper <*> parseArgs) fullDesc
  --matrixRestrictRun
  matrixRun bigrelArgs
    --vectorMatrixRun
    --intmapMatrixRun

matrixRun :: BigrelArgs -> IO ()
matrixRun (BigrelArgs attributeCount tupleCount tutd) = do
  case matrixRelation attributeCount tupleCount of
    Left err -> putStrLn (show err)
    Right rel -> if tutd == "" then
                   putStrLn "Done."
                 else do
                   let setx = Assign "x" (ExistingRelation (force rel))
                       (context,_,_) = execState (evalDatabaseContextExpr setx) (freshDatabaseState dateExamples)
                       interpreted = interpretDatabaseContextExpr context tutd
                       --plan = interpretRODatabaseContextOp context $ ":showplan " ++ tutd
                   --displayOpResult plan
                   case interpreted of
                     Right context' -> TIO.putStrLn $ relationAsHTML ((relationVariables context') M.! "x")
                     Left err -> hPutStrLn stderr (show err)


intmapMatrixRun :: IO ()
intmapMatrixRun = do
  let matrix = intmapMatrixRelation 100 100000
  putStrLn (show matrix)

--compare IntMap speed and size
--this is about 3 times faster (9 minutes) for 10x100000 and uses 800 MB
intmapMatrixRelation :: Int -> Int -> HS.HashSet (IM.IntMap Atom)
intmapMatrixRelation attributeCount tupleCount = HS.fromList $ map mapper [0..tupleCount]
  where
    mapper tupCount = IM.fromList $ map (\c-> (c, IntAtom tupCount)) [0..attributeCount]

instance Hash.Hashable (IM.IntMap Atom) where
  hashWithSalt salt tupMap = Hash.hashWithSalt salt (show tupMap)

vectorMatrixRun :: IO ()
vectorMatrixRun = do
  let matrix = vectorMatrixRelation 100 100000
  putStrLn (show matrix)

-- 20 s 90 MBs- a clear win- ideal size is 10 * 100000 * 8 bytes = 80 MB! without IntAtom wrapper
--with IntAtom wrapper: 1m12s 90 MB
vectorMatrixRelation :: Int -> Int -> HS.HashSet (V.Vector Atom)
vectorMatrixRelation attributeCount tupleCount = HS.fromList $ map mapper [0..tupleCount]
  where
    mapper tupCount = V.replicate attributeCount (IntAtom tupCount)

instance Hash.Hashable (V.Vector Atom) where
  hashWithSalt salt vec = Hash.hashWithSalt salt (show vec)

-- returns a relation with tupleCount tuples with a set of integer attributes attributesCount long
-- this is useful for performance and resource usage testing
matrixRelation :: Int -> Int -> Either RelationalError Relation
matrixRelation attributeCount tupleCount = do
  let attrs = A.attributesFromList $ map (\c-> Attribute (T.pack $ "a" ++ show c) IntAtomType) [0 .. attributeCount-1]
      tuple tupleX = RelationTuple attrs (V.generate attributeCount (\_ -> IntAtom tupleX))
      tuples = map (\c -> tuple c) [0 .. tupleCount]
  mkRelationDeferVerify attrs (RelationTupleSet tuples)


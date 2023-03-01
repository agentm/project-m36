{-# LANGUAGE DerivingVia, DeriveGeneric, DeriveAnyClass, TypeApplications #-}
-- create a simple, on-disk database of ~20 MB so that we can get a heap profile
import ProjectM36.Client
import TutorialD.Interpreter (parseRelationalExpr)
import Data.Text (Text)
import Codec.Winery
import Options.Applicative
import Data.Time.Clock
import GHC.Generics
import Control.DeepSeq
import ProjectM36.Tupleable
import Data.Time.Calendar
import Data.Proxy

data WeatherReading =
  WeatherReading
  { stamp :: UTCTime,
    temperature :: Integer,
    raining :: Bool,
    city :: Text,
    latitude :: Integer,
    longitude :: Integer
    }
   deriving (Generic, Show, Eq, NFData, Tupleable)
   deriving Serialise via WineryRecord WeatherReading

data Opts = Opts { datadir :: FilePath,
                   writeData :: Bool, --read or write mode
                   tupleCount :: Int,
                   execTutorialD :: Maybe Text
                 }

parseOptions :: Parser Opts
parseOptions = Opts <$>
  strOption (long "datadir" <> short 'd') <*>
  switch (long "write-data" <> short 'w') <*>
  option auto (long "tuple-count" <> short 'c' <> value 10000) <*>
  optional (strOption (long "exec-tutd" <> short 'e'))

main :: IO ()
main = do
  let parser = info (parseOptions <**> helper) (fullDesc <> progDesc "Read or write data for heap profiling.")
  opts <- execParser parser
  let connInfo = InProcessConnectionInfo (MinimalPersistence (datadir opts)) emptyNotificationCallback []
      eCheck v = do
        x <- v
        case x of 
          Left err -> error (show err)
          Right x' -> pure x'
  conn <- eCheck $ connectProjectM36 connInfo
  sessionId <- eCheck $ createSessionAtHead conn "master"
  if writeData opts then do
    putStrLn $ "writing " <> show (tupleCount opts) <> " tuples"
    let baseUTC = UTCTime { utctDay = fromGregorian 2022 2 22,
                            utctDayTime = secondsToDiffTime 0 }
    let addData = map (\i ->
                         WeatherReading { stamp = addUTCTime (secondsToNominalDiffTime (fromIntegral i)) baseUTC,
                                          temperature = i,
                                          raining = even i,
                                          city = "Mexico City",
                                          latitude = i,
                                          longitude = -i
                                        }) [1 .. fromIntegral (tupleCount opts)]
        defineExpr = toDefineExpr (Proxy @WeatherReading) "x"
    insertExpr <- eCheck (pure $ toInsertExpr addData "x")
    eCheck $ executeDatabaseContextExpr sessionId conn defineExpr    
    eCheck $ executeDatabaseContextExpr sessionId conn insertExpr
    eCheck $ commit sessionId conn
    else do
    {-
    --switch to extend to try to exercise parallel mapM (filter is not parallelized in streamly yet)
    putStrLn "reading"
    --read one row to see how heap is affected (will load all rows)
    let readOneRow = Restrict (AttributeEqualityPredicate "temperature" expensiveMatch) (RelationVariable "x" ())
        expensiveMatch = FunctionAtomExpr "test_bcrypt" [NakedAtomExpr (IntegerAtom 20), NakedAtomExpr (TextAtom "password"), NakedAtomExpr (IntegerAtom 900)] ()
        --simpleMatch = NakedAtomExpr (IntegerAtom 900)
    val <- eCheck $ executeRelationalExpr sessionId conn readOneRow
    print val
    -}
    let tutd = case execTutorialD opts of
          Just t -> t
          Nothing -> "x where temperature = 5"
    case parseRelationalExpr tutd of
      Left err -> error (show err)
      Right parsed -> do
        val <- eCheck $ executeRelationalExpr sessionId conn parsed
        print val
        
  


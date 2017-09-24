import ProjectM36.Base
import ProjectM36.Relation.Show.CSV
import ProjectM36.Relation.Parse.CSV
import qualified ProjectM36.Attribute as A
import ProjectM36.Relation
import ProjectM36.DataTypes.Basic
import ProjectM36.DataTypes.List
import ProjectM36.DataTypes.Interval

import System.Exit
import Test.HUnit
import Data.Time.Calendar
import Data.Time.Clock
import Data.Monoid
{-
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import ProjectM36.Relation.Show.Term
import qualified Data.ByteString.Lazy as BS
-}

main :: IO ()           
main = do 
  tcounts <- runTestTT $ TestList [testCSVExport, 
                                   testADTExport]
  if errors tcounts + failures tcounts > 0 then exitFailure else exitSuccess

assertEither :: (Show a) => IO (Either a b) -> IO b
assertEither x = do
  res <- x
  case res of
    Left err -> assertFailure (show err) >> undefined
    Right val -> pure val
    
testADTExport :: Test    
testADTExport = TestCase $ do
  let adtCSV = "a\nCons 4 (Cons 5 Empty)\n"
      attrs = A.attributesFromList [Attribute "a" (listAtomType IntegerAtomType)]
      expectedRel = mkRelationFromList attrs [[listCons IntegerAtomType [IntegerAtom 4, IntegerAtom 5]]]
  case csvAsRelation attrs basicTypeConstructorMapping adtCSV of
    Left err -> assertFailure ("import failure: " <> show err)
    Right rel -> assertEqual "import cons list" expectedRel (Right rel)
    
--round-trip various atom types through CSV export/import
testCSVExport :: Test
testCSVExport = TestCase $ do
  now <- getCurrentTime
  testInterval <- assertEither $ pure (createInterval 
                                (DateTimeAtom now) 
                                (DateTimeAtom (addUTCTime 86400 now))
                                True
                                False)
  let attrs = A.attributesFromList [Attribute "textattr" TextAtomType, 
                                    Attribute "integerattr" IntegerAtomType,
                                    Attribute "dayattr" DayAtomType,
                                    Attribute "datetimeattr" DateTimeAtomType,
                                    Attribute "bytestringattr" ByteStringAtomType,
                                    Attribute "listintegerattr" (listAtomType IntegerAtomType),
                                    Attribute "listtextattr" (listAtomType TextAtomType),
                                    Attribute "intervalattr" (IntervalAtomType DateTimeAtomType)
                                   ]
      sampleByteString = "\1\0\244\34\150"
      relOrErr = mkRelationFromList attrs [
        [TextAtom "text atom with \"quote\"", 
         IntegerAtom 123, 
         DayAtom (fromGregorian 2017 4 10),
         DateTimeAtom now,
         ByteStringAtom sampleByteString,
         listCons IntegerAtomType [IntegerAtom 5, IntegerAtom 6, IntegerAtom 7],
         listCons TextAtomType [TextAtom "text1", TextAtom "text2"],
         testInterval
         ],
        [TextAtom "second text atom", 
         IntegerAtom 314, 
         DayAtom (fromGregorian 1001 6 28),
         DateTimeAtom (addUTCTime 360 now),
         ByteStringAtom sampleByteString,         
         listCons IntegerAtomType [IntegerAtom 10, IntegerAtom 11, IntegerAtom 12],
         listCons TextAtomType [TextAtom "text5\"", TextAtom "text6\r\n"],
         testInterval
        ]]
        
  case relOrErr of 
    Left err -> assertFailure $ "export relation creation failure: " ++ show err
    Right rel -> 
      case relationAsCSV rel of
        Left err -> assertFailure $ "export failed: " ++ show err
        Right csvData -> 
          --BS.writeFile "/tmp/csv" csvData
          --putStrLn (TL.unpack (TE.decodeUtf8 csvData))
          case csvAsRelation attrs basicTypeConstructorMapping csvData of -- import csv data back to relation
            Left err -> assertFailure $ "re-import failed: " ++ show err
            Right rel' -> assertEqual "relation CSV comparison" rel rel'


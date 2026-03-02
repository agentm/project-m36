{-# LANGUAGE DeriveGeneric #-}
-- functions common to both SQL and TutorialD interpreters
module ProjectM36.Interpreter where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.DataFrame
import Text.Megaparsec
import Data.Void
import GHC.Generics
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import System.IO
import Control.Monad.Random
import ProjectM36.Relation.Show.Term
import ProjectM36.Relation

type Parser = Parsec Void T.Text
type ParserError = ParseErrorBundle T.Text Void
type PromptLength = Int

data SafeEvaluationFlag = SafeEvaluation | UnsafeEvaluation deriving (Eq)

data ConsoleResult = QuitResult |
                     DisplayResult StringType |
                     DisplayIOResult (IO ()) |
                     DisplayRelationResult Relation |
                     DisplayDataFrameResult DataFrame |
                     DisplayHintWith T.Text ConsoleResult |
                     DisplayErrorResult StringType |
                     DisplayRelationalErrorResult RelationalError |
                     DisplayParseErrorResult (Maybe PromptLength) ParserError | -- PromptLength refers to length of prompt text
                     QuietSuccessResult
                   deriving (Generic)

type InteractiveConsole = Bool

displayResult :: ConsoleResult -> IO ()
displayResult QuitResult = return ()
displayResult (DisplayResult out) = TIO.putStrLn out
displayResult (DisplayIOResult ioout) = ioout
displayResult (DisplayErrorResult err) = let outputf = if T.length err > 0 && T.last err /= '\n' then TIO.hPutStrLn else TIO.hPutStr in
  outputf stderr ("ERR: " <> err)
displayResult QuietSuccessResult = return ()
displayResult (DisplayRelationResult rel) = do
  gen <- newStdGen
  let randomlySortedRel = evalRand (randomizeTupleOrder rel) gen
  TIO.putStrLn (showRelation randomlySortedRel)
displayResult (DisplayParseErrorResult mPromptLength err) = do
  let errorIndent = errorOffset . NE.head . bundleErrors $ err
      errString = T.pack (parseErrorPretty . NE.head . bundleErrors $ err)
      pointyString len = T.justifyRight (len + fromIntegral errorIndent) '_' "^"
  maybe (pure ()) (TIO.putStrLn . pointyString) mPromptLength
  TIO.putStr ("ERR:" <> errString)
displayResult (DisplayDataFrameResult dFrame) = TIO.putStrLn (showDataFrame dFrame)
displayResult (DisplayRelationalErrorResult err) =
  TIO.putStrLn ("ERR:" <> T.pack (show err))
displayResult (DisplayHintWith hint result) = do
  displayResult (DisplayResult hint)
  displayResult result

{-# LANGUAGE DeriveGeneric #-}
-- functions common to both SQL and TutorialD interpreters
module ProjectM36.Interpreter where
import ProjectM36.Base
import ProjectM36.DataFrame
import Text.Megaparsec
import Data.Void
import Data.Text
import GHC.Generics

type Parser = Parsec Void Text
type ParserError = ParseErrorBundle Text Void
type PromptLength = Int

data SafeEvaluationFlag = SafeEvaluation | UnsafeEvaluation deriving (Eq)

data ConsoleResult = QuitResult |
                     DisplayResult StringType |
                     DisplayIOResult (IO ()) |
                     DisplayRelationResult Relation |
                     DisplayDataFrameResult DataFrame |
                     DisplayErrorResult StringType |
                     DisplayParseErrorResult (Maybe PromptLength) ParserError | -- PromptLength refers to length of prompt text
                     QuietSuccessResult
                   deriving (Generic)

type InteractiveConsole = Bool


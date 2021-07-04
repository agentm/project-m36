{-# LANGUAGE DeriveGeneric, CPP #-}
module TutorialD.Interpreter.Base (
  module TutorialD.Interpreter.Base,
  module Text.Megaparsec,
#if MIN_VERSION_megaparsec(6,0,0)
  module Text.Megaparsec.Char,
  module Control.Applicative
#else
  module Text.Megaparsec.Text
#endif
 )
  where
import ProjectM36.Base
import ProjectM36.AtomType
import ProjectM36.Attribute as A
import ProjectM36.Relation
import ProjectM36.DataFrame

#if MIN_VERSION_megaparsec(6,0,0)
import Text.Megaparsec.Char 
import qualified Text.Megaparsec.Char.Lexer as Lex
import Text.Megaparsec
import Data.Void
import Control.Applicative hiding (many, some)
#else
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as Lex
#endif

import Data.Text hiding (count)
import System.Random
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Text.IO as TIO
import System.IO
import ProjectM36.Relation.Show.Term
import GHC.Generics
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif
import qualified Data.UUID as U
import Control.Monad.Random
import Data.List.NonEmpty as NE
import Data.Time.Clock
import Data.Time.Format
import Control.Monad (void)
import Data.Char

#if !MIN_VERSION_megaparsec(7,0,0)
anySingle :: Parsec Void Text (Token Text)
anySingle = anyChar
#endif

displayOpResult :: TutorialDOperatorResult -> IO ()
displayOpResult QuitResult = return ()
displayOpResult (DisplayResult out) = TIO.putStrLn out
displayOpResult (DisplayIOResult ioout) = ioout
displayOpResult (DisplayErrorResult err) = let outputf = if T.length err > 0 && T.last err /= '\n' then TIO.hPutStrLn else TIO.hPutStr in
  outputf stderr ("ERR: " <> err)
displayOpResult QuietSuccessResult = return ()
displayOpResult (DisplayRelationResult rel) = do
  gen <- newStdGen
  let randomlySortedRel = evalRand (randomizeTupleOrder rel) gen
  TIO.putStrLn (showRelation randomlySortedRel)
displayOpResult (DisplayParseErrorResult mPromptLength err) = do
#if MIN_VERSION_megaparsec(7,0,0)
  let errorIndent = errorOffset . NE.head . bundleErrors $ err
      errString = T.pack (parseErrorPretty . NE.head . bundleErrors $ err)
#else
  let errorIndent = unPos (sourceColumn (NE.head (errorPos err)))
      errString = T.pack (parseErrorPretty err)
#endif
      pointyString len = T.justifyRight (len + fromIntegral errorIndent) '_' "^"
  maybe (pure ()) (TIO.putStrLn . pointyString) mPromptLength
  TIO.putStr ("ERR:" <> errString)
displayOpResult (DisplayDataFrameResult dFrame) = TIO.putStrLn (showDataFrame dFrame)

#if MIN_VERSION_megaparsec(6,0,0)
type Parser = Parsec Void Text
type ParseStr = Text
#else
type ParseStr = String
#endif

-- consumes only horizontal spaces
spaceConsumer :: Parser ()
spaceConsumer = Lex.space space1 (Lex.skipLineComment "--") (Lex.skipBlockComment "{-" "-}")

opChar :: Parser Char
opChar = oneOf (":!#$%&*+./<=>?\\^|-~" :: String)-- remove "@" so it can be used as attribute marker without spaces

reserved :: ParseStr -> Parser ()
reserved word = try (string word *> notFollowedBy opChar *> spaceConsumer)

reservedOp :: ParseStr -> Parser ()
reservedOp op = try (spaceConsumer *> string op *> notFollowedBy opChar *> spaceConsumer)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

identifier :: Parser Text
identifier = do
  istart <- letterChar <|> char '_'
  identifierRemainder istart

identifierRemainder :: Char -> Parser Text
identifierRemainder c = do
  rest <- many (alphaNumChar <|> char '_' <|> char '#')
  spaceConsumer
  pure (pack (c:rest))

symbol :: ParseStr -> Parser Text
#if MIN_VERSION_megaparsec(6,0,0)
symbol sym = Lex.symbol spaceConsumer sym
#else
symbol sym = pack <$> Lex.symbol spaceConsumer sym
#endif

comma :: Parser Text
comma = symbol ","

pipe :: Parser Text
pipe = symbol "|"

quote :: Parser Text
quote = symbol "\""

tripleQuote :: Parser Text
tripleQuote = symbol "\"\"\""

arrow :: Parser Text
arrow = symbol "->"

semi :: Parser Text
semi = symbol ";"

nline :: Parser Text
nline = (T.singleton <$> newline) <|> crlf

integer :: Parser Integer
#if MIN_VERSION_megaparsec(6,0,0)
integer = Lex.signed (pure ()) Lex.decimal <* spaceConsumer
#else
integer = Lex.signed (pure ()) Lex.integer <* spaceConsumer
#endif

natural :: Parser Integer
#if MIN_VERSION_megaparsec(6,0,0)
natural = Lex.decimal <* spaceConsumer
#else
natural = Lex.integer <* spaceConsumer
#endif

float :: Parser Double
float = Lex.float <* spaceConsumer

capitalizedIdentifier :: Parser Text
capitalizedIdentifier =
  upperChar >>= identifierRemainder

uncapitalizedIdentifier :: Parser Text
uncapitalizedIdentifier =
  lowerChar >>= identifierRemainder

showRelationAttributes :: Attributes -> Text
showRelationAttributes attrs = "{" <> T.concat (L.intersperse ", " $ L.map showAttribute attrsL) <> "}"
  where
    showAttribute (Attribute name atomType') = name <> " " <> prettyAtomType atomType'
    attrsL = A.toList attrs

type PromptLength = Int

#if MIN_VERSION_megaparsec(7,0,0)
type ParserError = ParseErrorBundle T.Text Void
#elif MIN_VERSION_megaparsec(6,0,0)
type ParserError = ParseError Char Void
#else
type ParserError = ParseError Char Dec
#endif

data TutorialDOperatorResult = QuitResult |
                               DisplayResult StringType |
                               DisplayIOResult (IO ()) |
                               DisplayRelationResult Relation |
                               DisplayDataFrameResult DataFrame |
                               DisplayErrorResult StringType |
                               DisplayParseErrorResult (Maybe PromptLength) ParserError | -- PromptLength refers to length of prompt text
                               QuietSuccessResult
                               deriving (Generic)

type TransactionGraphWasUpdated = Bool

--allow for python-style triple quoting because guessing the correct amount of escapes in different contexts is annoying
tripleQuotedString :: Parser Text
tripleQuotedString = do
  _ <- tripleQuote
  pack <$> manyTill anySingle (try (tripleQuote >> notFollowedBy quote))

normalQuotedString :: Parser Text
normalQuotedString = quote *> (T.pack <$> manyTill Lex.charLiteral quote)

quotedString :: Parser Text
quotedString = try tripleQuotedString <|> normalQuotedString

quoted :: Parser a -> Parser a
quoted = between quote quote

uuidP :: Parser U.UUID
uuidP = do
  uuidStart <- count 8 hexDigitChar
  _ <- char '-' -- min 28 with no dashes, maximum 4 dashes
  uuidMid1 <- count 4 hexDigitChar
  _ <- char '-'
  uuidMid2 <- count 4 hexDigitChar
  _ <- char '-'
  uuidMid3 <- count 4 hexDigitChar
  _ <- char '-'
  uuidEnd <- count 12 hexDigitChar
  let uuidStr = L.intercalate "-" [uuidStart, uuidMid1, uuidMid2, uuidMid3, uuidEnd]
  case U.fromString uuidStr of
    Nothing -> fail "Invalid uuid string"
    Just uuid -> return uuid

utcTimeP :: Parser UTCTime
utcTimeP = do
  timeStr <- quotedString
  case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack timeStr) of
    Nothing -> fail "invalid datetime input, use \"YYYY-MM-DD HH:MM:SS\""
    Just stamp' -> pure stamp'


colonOp :: Text -> Parser ()
colonOp opStr = do
  _ <- string opStr <* (void spaceChar <|> eof) <* spaceConsumer
  pure ()

hex :: Parser Text
hex = takeWhileP (Just "hexadecimal")
         (\c ->
             isDigit c
             || (c >= 'a' && c <= 'f'))
  

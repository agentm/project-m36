{-# LANGUAGE CPP #-}
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
import ProjectM36.Interpreter

#if MIN_VERSION_megaparsec(6,0,0)
import Text.Megaparsec.Char 
import qualified Text.Megaparsec.Char.Lexer as Lex
import Text.Megaparsec
import Control.Applicative hiding (many, some)
#else
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as Lex
#endif

import Data.Text hiding (count)
import qualified Data.Text as T
import qualified Data.List as L
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif
import qualified Data.UUID as U
import Control.Monad.Random
import Data.Time.Clock
import Data.Time.Format
import Data.Char

#if !MIN_VERSION_megaparsec(7,0,0)
anySingle :: Parsec Void Text (Token Text)
anySingle = anyChar
#endif


type ParseStr = Text

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

identifierP :: Parser Text
identifierP = identifier <* spaceConsumer

-- | Roles may include human names, so be generous in what we accept
roleNameP :: Parser Text
roleNameP = normalQuotedString <|> takeWhile1P (Just "non-whitespace character") (not . isSpace) <* spaceConsumer

permissionP :: Parser Text
permissionP = normalQuotedString <|> takeWhile1P (Just "non-whitespace character") (not . isSpace)
  
identifierRemainder :: Char -> Parser Text
identifierRemainder c = do
  rest <- many (alphaNumChar <|> char '_' <|> char '#')
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

backtick :: Parser Text
backtick = symbol "`"

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

-- | When an identifier is quoted, it can contain any string.
quotedIdentifier :: Parser Text
quotedIdentifier =
  T.pack <$> backticks (many (escapedBacktick <|> notBacktickChar))
  where
    escapedBacktick = char '\\' >> char '`'
    notBacktickChar = satisfy ('`' /=)

backticks :: Parser a -> Parser a
backticks = between backtick backtick
  
showRelationAttributes :: Attributes -> Text
showRelationAttributes attrs = "{" <> T.concat (L.intersperse ", " $ L.map showAttribute attrsL) <> "}"
  where
    showAttribute (Attribute name atomType') = name <> " " <> prettyAtomType atomType'
    attrsL = A.toList attrs


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
  

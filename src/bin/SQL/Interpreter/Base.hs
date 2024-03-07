module SQL.Interpreter.Base where
import ProjectM36.Interpreter
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex
import Data.Text as T (Text, singleton, pack, splitOn, toLower)

-- consumes only horizontal spaces
spaceConsumer :: Parser ()
spaceConsumer = Lex.space space1 (Lex.skipLineComment "--") (Lex.skipBlockComment "{-" "-}")

opChar :: Parser Char
opChar = oneOf (":!#$%&*+./<=>?\\^|-~" :: String)-- remove "@" so it can be used as attribute marker without spaces

-- parse case-insensitive keyword
reserved :: Text -> Parser ()
reserved word = do
  try (string' word *> spaceConsumer)

reserveds :: Text -> Parser ()
reserveds words' = do
  let words'' = T.splitOn " " words'
  reserveds' words''

reserveds' :: [Text] -> Parser ()
reserveds' words' =
  sequence_ (map reserved words')
  
-- does not consume trailing spaces
qualifiedNameSegment :: Text -> Parser Text
qualifiedNameSegment sym = T.toLower <$> string' sym

reservedOp :: Text -> Parser ()
reservedOp op = try (spaceConsumer *> string op *> notFollowedBy opChar *> spaceConsumer)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

identifier :: Parser Text
identifier = do
  istart <- letterChar <|> char '_'
  toLower <$> identifierRemainder istart

identifierRemainder :: Char -> Parser Text
identifierRemainder c = do
  rest <- many (alphaNumChar <|> char '_' <|> char '#')
  spaceConsumer
  pure (pack (c:rest))

symbol :: Text -> Parser Text
symbol sym = Lex.symbol spaceConsumer sym

comma :: Parser Text
comma = symbol ","

sepByComma1 :: Parser a -> Parser [a]
sepByComma1 p = sepBy1 p comma

sepByComma :: Parser a -> Parser [a]
sepByComma p = sepBy p comma

pipe :: Parser Text
pipe = symbol "|"

semi :: Parser Text
semi = symbol ";"

nline :: Parser Text
nline = (T.singleton <$> newline) <|> crlf

integer :: Parser Integer
integer = Lex.signed (pure ()) Lex.decimal <* spaceConsumer

natural :: Parser Integer
natural = Lex.decimal <* spaceConsumer

double :: Parser Double
double = Lex.float <* spaceConsumer

-- | When an identifier is quoted, it can contain any string.
quotedIdentifier :: Parser Text
quotedIdentifier =
  T.pack <$> (doubleQuote *> many (escapedDoubleQuote <|> notDoubleQuote) <* doubleQuote)
  where
    doubleQuote = char '"'
    escapedDoubleQuote = chunk "\"\"" *> pure '"'
    notDoubleQuote = satisfy ('"' /=)

    

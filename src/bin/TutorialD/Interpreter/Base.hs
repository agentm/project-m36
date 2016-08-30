{-# LANGUAGE DeriveGeneric #-}
module TutorialD.Interpreter.Base where
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as Lex
import ProjectM36.Base
import ProjectM36.AtomType
import Data.Text
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Text.IO as TIO
import System.IO
import ProjectM36.Relation.Show.Term
import GHC.Generics
import Data.Monoid
import Control.Monad (void)

displayOpResult :: TutorialDOperatorResult -> IO ()
displayOpResult QuitResult = return ()
displayOpResult (DisplayResult out) = TIO.putStrLn out
displayOpResult (DisplayIOResult ioout) = ioout
displayOpResult (DisplayErrorResult err) = TIO.hPutStrLn stderr ("ERR: " <> err)
displayOpResult QuietSuccessResult = return ()
displayOpResult (DisplayRelationResult rel) = TIO.putStrLn (showRelation rel)

spaceConsumer :: Parser ()
spaceConsumer = Lex.space (void spaceChar) (Lex.skipLineComment "--") (Lex.skipBlockComment "{-" "-}")
  
opChar :: Parser Char
opChar = oneOf (":!#$%&*+./<=>?\\^|-~"::String)-- remove "@" so it can be used as attribute marker without spaces

reserved :: String -> Parser ()
reserved word = try (string word *> notFollowedBy opChar *> spaceConsumer)

reservedOp :: String -> Parser ()
reservedOp op = try (string op *> notFollowedBy opChar *> spaceConsumer)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

identifier :: Parser Text
identifier = do
  istart <- letterChar <|> char '_'
  irest <- many (alphaNumChar <|> char '_' <|> char '#')
  spaceConsumer
  pure (pack (istart:irest))

symbol :: String -> Parser Text
symbol sym = pack <$> Lex.symbol spaceConsumer sym 

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

{-
whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer
-}

integer :: Parser Integer
integer = Lex.integer

float :: Parser Double
float = Lex.float

capitalizedIdentifier :: Parser Text
capitalizedIdentifier = do
  fletter <- upperChar
  rest <- option "" identifier 
  spaceConsumer
  pure (T.cons fletter rest)
  
uncapitalizedIdentifier :: Parser Text
uncapitalizedIdentifier = do
  fletter <- lowerChar
  rest <- option "" identifier
  spaceConsumer
  pure (T.cons fletter rest)

showRelationAttributes :: Attributes -> Text
showRelationAttributes attrs = "{" <> T.concat (L.intersperse ", " $ L.map showAttribute attrsL) <> "}"
  where
    showAttribute (Attribute name atomType) = name <> " " <> prettyAtomType atomType
    attrsL = V.toList attrs

data TutorialDOperatorResult = QuitResult |
                               DisplayResult StringType |
                               DisplayIOResult (IO ()) |
                               DisplayRelationResult Relation |
                               DisplayErrorResult StringType |
                               QuietSuccessResult
                               deriving (Generic)
                               
type TransactionGraphWasUpdated = Bool

escapedChar :: Parser Char
escapedChar = do
  _ <- char '\\'
  let lookupAssoc = [('n','\n'), ('\"','\"'), ('\\', '\\')]
  c <- oneOf ("\\\"n"::String) -- all the characters which can be escaped
  case lookup c lookupAssoc of
    Just v -> pure v
    Nothing -> fail "failed to handle escaped character"
                   
--http://stackoverflow.com/questions/24106314/parser-for-quoted-string-using-parsec
nonEscapedChar :: Parser Char
nonEscapedChar = noneOf ("\\\""::String)

quotedChar :: Parser Char
quotedChar = escapedChar <|> nonEscapedChar

--allow for python-style triple quoting because guessing the correct amount of escapes in different contexts is annoying
tripleQuotedString :: Parser Text
tripleQuotedString = do
  _ <- tripleQuote
  pack <$> manyTill anyChar (try (tripleQuote >> notFollowedBy quote))
  
normalQuotedString :: Parser Text
normalQuotedString = quote *> (T.pack <$> manyTill quotedChar quote)

quotedString :: Parser Text
quotedString = try tripleQuotedString <|> normalQuotedString
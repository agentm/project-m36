{-# LANGUAGE DeriveGeneric #-}
module TutorialD.Interpreter.Base where
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as Lex
import ProjectM36.Base
import ProjectM36.AtomType
import Data.Text
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Text.IO as TIO
import System.IO
import ProjectM36.Relation.Show.Term
import GHC.Generics
import Data.Monoid

displayOpResult :: TutorialDOperatorResult -> IO ()
displayOpResult QuitResult = return ()
displayOpResult (DisplayResult out) = TIO.putStrLn out
displayOpResult (DisplayIOResult ioout) = ioout
displayOpResult (DisplayErrorResult err) = TIO.hPutStrLn stderr ("ERR: " <> err)
displayOpResult QuietSuccessResult = return ()
displayOpResult (DisplayRelationResult rel) = TIO.putStrLn (showRelation rel)

spaceConsumer = space spaceChar (skipLineComment "--") (skipBlockComment "{-" "-}")
  
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

reservedOp :: Text -> Parser ()
reservedOp = Token.reservedOp lexer

reserved :: Text -> Parser ()
reserved = Token.reserved lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

identifier :: Parser Text
identifier = Token.identifier lexer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

comma :: Parser Text
comma = Token.comma lexer

pipe :: Parser Text
pipe = Token.symbol lexer "|"

quote :: Parser Text
quote = Token.symbol lexer "\""

tripleQuote :: Parser Text
tripleQuote = Token.symbol lexer "\"\"\""

arrow :: Parser Text
arrow = Token.symbol lexer "->"

semi :: Parser Text
semi = Token.semi lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

capitalizedIdentifier :: Parser Text
capitalizedIdentifier = do
  fletter <- upper
  rest <- option "" identifier 
  spaces
  pure $ fletter:rest
  
uncapitalizedIdentifier :: Parser Text
uncapitalizedIdentifier = do
  fletter <- lower
  rest <- option "" identifier
  spaces
  pure (fletter:rest)

showRelationAttributes :: Attributes -> Text
showRelationAttributes attrs = "{" <> T.concat (L.intersperse ", " $ map showAttribute attrsL) <> "}"
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

escapedString :: Parser Text
escapedString = do
  _ <- char '\\'
  let lookupAssoc = [('n',"\n"), ('\"',"\""), ('\\', "\\")]
  c <- oneOf "\\\"n" -- all the characters which can be escaped
  case lookup c lookupAssoc of
    Just v -> pure v
    Nothing -> fail "failed to handle escaped character"
                   
--http://stackoverflow.com/questions/24106314/parser-for-quoted-string-using-parsec
nonEscapedChar :: Parser Char
nonEscapedChar = noneOf "\\\""

character :: Parser Text
character = fmap return nonEscapedChar <|> escapedString

--allow for python-style triple quoting because guessing the correct amount of escapes in different contexts is annoying
tripleQuotedString :: Parser Text
tripleQuotedString = do
  _ <- tripleQuote
  manyTill anyChar (try (tripleQuote >> notFollowedBy quote))
  
normalQuotedString :: Parser Text
normalQuotedString = do
  _ <- quote
  strings <- many character
  _ <- quote
  pure (concat strings)

quotedString :: Parser Text
quotedString = try tripleQuotedString <|> normalQuotedString
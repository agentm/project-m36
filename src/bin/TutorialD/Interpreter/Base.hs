{-# LANGUAGE DeriveGeneric #-}
module TutorialD.Interpreter.Base where
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language
import ProjectM36.Base
import ProjectM36.AtomType
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Text.IO as TIO
import System.IO
import ProjectM36.Relation.Show.Term
import GHC.Generics

displayOpResult :: TutorialDOperatorResult -> IO ()
displayOpResult QuitResult = return ()
displayOpResult (DisplayResult out) = TIO.putStrLn out
displayOpResult (DisplayIOResult ioout) = ioout
displayOpResult (DisplayErrorResult err) = TIO.hPutStrLn stderr ("ERR: " `T.append` err)
displayOpResult QuietSuccessResult = return ()
displayOpResult (DisplayRelationResult rel) = TIO.putStrLn (showRelation rel)

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser tutD
        where tutD = emptyDef {
                Token.reservedOpNames = ["join", "where", "union", "group", "ungroup"],
                Token.reservedNames = [],
                Token.identStart = letter <|> char '_',
                Token.opLetter = oneOf ":!#$%&*+./<=>?\\^|-~", -- remove "@" so it can be used as attribute marker without spaces
                Token.identLetter = alphaNum <|> char '_' <|> char '#'} -- # needed for Date examples
                     
parens :: Parser a -> Parser a
parens = Token.parens lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

identifier :: Parser String
identifier = Token.identifier lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

comma :: Parser String
comma = Token.comma lexer

pipe :: Parser String
pipe = Token.symbol lexer "|"

quote :: Parser String
quote = Token.symbol lexer "\""

tripleQuote :: Parser String
tripleQuote = Token.symbol lexer "\"\"\""

arrow :: Parser String
arrow = Token.symbol lexer "->"

semi :: Parser String
semi = Token.semi lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

capitalizedIdentifier :: Parser String
capitalizedIdentifier = do
  fletter <- upper
  rest <- option "" identifier 
  spaces
  pure $ fletter:rest
  
uncapitalizedIdentifier :: Parser String  
uncapitalizedIdentifier = do
  fletter <- lower
  rest <- option "" identifier
  spaces
  pure (fletter:rest)

showRelationAttributes :: Attributes -> T.Text
showRelationAttributes attrs = "{" `T.append` T.concat (L.intersperse ", " $ map showAttribute attrsL) `T.append` "}"
  where
    showAttribute (Attribute name atomType) = name `T.append` " " `T.append` prettyAtomType atomType
    attrsL = V.toList attrs

data TutorialDOperatorResult = QuitResult |
                               DisplayResult StringType |
                               DisplayIOResult (IO ()) |
                               DisplayRelationResult Relation |
                               DisplayErrorResult StringType |
                               QuietSuccessResult
                               deriving (Generic)
                               
type TransactionGraphWasUpdated = Bool

{-
quotedChar :: Parser Char
quotedChar = noneOf "\""
           <|> try (string "\"\"" >> return '"')

quotedString :: Parser String
quotedString = string "\"" *> many quotedChar <* string "\"" <* whiteSpace
-}
escapedString :: Parser String
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

character :: Parser String
character = fmap return nonEscapedChar <|> escapedString

--allow for python-style triple quoting because guessing the correct amount of escapes in different contexts is annoying
tripleQuotedString :: Parser String
tripleQuotedString = do
  _ <- tripleQuote
  manyTill anyChar (try (tripleQuote >> notFollowedBy quote))
  
normalQuotedString :: Parser String
normalQuotedString = do
  _ <- quote
  strings <- many character
  _ <- quote
  pure (concat strings)

quotedString :: Parser String
quotedString = try tripleQuotedString <|> normalQuotedString
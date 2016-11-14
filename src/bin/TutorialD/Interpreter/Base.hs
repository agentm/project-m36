{-# LANGUAGE DeriveGeneric #-}
module TutorialD.Interpreter.Base where
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as Lex
import ProjectM36.Base
import ProjectM36.AtomType
import Data.Text hiding (count)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Text.IO as TIO
import System.IO
import ProjectM36.Relation.Show.Term
import GHC.Generics
import Data.Monoid
import Control.Monad (void)
import qualified Data.UUID as U

displayOpResult :: TutorialDOperatorResult -> IO ()
displayOpResult QuitResult = return ()
displayOpResult (DisplayResult out) = TIO.putStrLn out
displayOpResult (DisplayIOResult ioout) = ioout
displayOpResult (DisplayErrorResult err) = let outputf = if T.length err > 0 && T.last err /= '\n' then TIO.hPutStrLn else TIO.hPutStr in 
  outputf stderr ("ERR: " <> err)
displayOpResult QuietSuccessResult = return ()
displayOpResult (DisplayRelationResult rel) = TIO.putStrLn (showRelation rel)
displayOpResult (DisplayParseErrorResult promptLength err) = TIO.putStrLn pointyString >> TIO.putStrLn ("ERR:" <> T.pack (show err))
  where
    pointyString = T.justifyRight (promptLength + (sourceColumn (errorPos err))) '_' "^"

spaceConsumer :: Parser ()
spaceConsumer = Lex.space (void spaceChar) (Lex.skipLineComment "--") (Lex.skipBlockComment "{-" "-}")
  
opChar :: Parser Char
opChar = oneOf (":!#$%&*+./<=>?\\^|-~" :: String)-- remove "@" so it can be used as attribute marker without spaces

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
                               DisplayParseErrorResult Int ParseError | -- Int refers to length of prompt text
                               QuietSuccessResult
                               deriving (Generic)
                               
type TransactionGraphWasUpdated = Bool

--allow for python-style triple quoting because guessing the correct amount of escapes in different contexts is annoying
tripleQuotedString :: Parser Text
tripleQuotedString = do
  _ <- tripleQuote
  pack <$> manyTill anyChar (try (tripleQuote >> notFollowedBy quote))
  
normalQuotedString :: Parser Text
normalQuotedString = quote *> (T.pack <$> manyTill Lex.charLiteral quote)

quotedString :: Parser Text
quotedString = try tripleQuotedString <|> normalQuotedString

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

{-# LANGUAGE OverloadedStrings #-}
module TutorialD.Interpreter.Base where
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language
import ProjectM36.Base
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Text.IO as TIO
import System.IO
import ProjectM36.Atom

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

comma :: Parser String
comma = Token.comma lexer

semi :: Parser String
semi = Token.semi lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

atomTypeToTutDType :: AtomType -> Maybe T.Text
atomTypeToTutDType atomType = case atomType of
  aType@(AtomType _) -> lookup aType [(stringAtomType, "char"),
                                      (intAtomType, "int"),
                                      (boolAtomType, "bool"),
                                      (dateTimeAtomType, "datetime"),
                                      (doubleAtomType, "double")]
  (RelationAtomType attrs) -> Just $ "relation" `T.append` showRelationAttributes attrs
  _ -> Nothing
    
showRelationAttributes :: Attributes -> T.Text
showRelationAttributes attrs = "{" `T.append` T.concat (L.intersperse ", " $ map showAttribute attrsL) `T.append` "}"
  where
    showAttribute (Attribute name atomType) = name `T.append` " " `T.append` case atomTypeToTutDType atomType of
      Just t -> t
      Nothing -> "unknown"
    attrsL = V.toList attrs

data TutorialDOperatorResult = QuitResult |
                               DisplayResult StringType |
                               DisplayIOResult (IO ()) |
                               DisplayErrorResult StringType |
                               QuietSuccessResult
                               
type TransactionGraphWasUpdated = Bool

displayOpResult :: TutorialDOperatorResult -> IO ()
displayOpResult QuitResult = return ()
displayOpResult (DisplayResult out) = TIO.putStrLn out
displayOpResult (DisplayIOResult ioout) = ioout
displayOpResult (DisplayErrorResult err) = TIO.hPutStrLn stderr ("ERR: " `T.append` err)
displayOpResult QuietSuccessResult = return ()

quotedChar :: Parser Char
quotedChar = noneOf "\""
           <|> try (string "\"\"" >> return '"')

quotedString :: Parser String
quotedString = string "\"" *> many quotedChar <* string "\"" <* whiteSpace


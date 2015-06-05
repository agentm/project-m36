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

--convert Tutorial D type to AtomType
tutDTypeToAtomType :: String -> Maybe AtomType
tutDTypeToAtomType tutDType = case tutDType of
  "char" -> Just StringAtomType
  "int" -> Just IntAtomType
  _ -> Nothing

atomTypeToTutDType :: AtomType -> Maybe T.Text
atomTypeToTutDType atomType = case atomType of
  StringAtomType -> Just "char"
  IntAtomType -> Just "int"
  RelationAtomType attrs -> Just $ "relation" `T.append` showRelationAttributes attrs
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
                               DisplayErrorResult StringType |
                               QuietSuccessResult


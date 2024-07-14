{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module TutorialD.Interpreter.TransGraphRelationalOperator where
import ProjectM36.TransGraphRelationalExpression
import ProjectM36.TransactionGraph
import ProjectM36.Interpreter
import TutorialD.Interpreter.Types
import qualified ProjectM36.Client as C

import TutorialD.Interpreter.Base
import TutorialD.Interpreter.RelationalExpr

import qualified Data.Text as T

instance RelationalMarkerExpr TransactionIdLookup where
  parseMarkerP = string "@" *> transactionIdLookupP

newtype TransGraphRelationalOperator = ShowTransGraphRelation TransGraphRelationalExpr
                                     deriving Show

transactionIdLookupP :: Parser TransactionIdLookup
transactionIdLookupP =  (TransactionIdLookup <$> uuidP) <|>
                        (TransactionIdHeadNameLookup <$> identifierP <*> many transactionIdHeadBacktrackP)
                        
transactionIdHeadBacktrackP :: Parser TransactionIdHeadBacktrack                        
transactionIdHeadBacktrackP = (string "~" *> (TransactionIdHeadParentBacktrack <$> backtrackP)) <|>
                              (string "^" *> (TransactionIdHeadBranchBacktrack <$> backtrackP)) <|>
                              (string "@" *> (TransactionStampHeadBacktrack <$> utcTimeP))
                              
backtrackP :: Parser Int
backtrackP = fromIntegral <$> integer <|> pure 1
  
transGraphRelationalOpP :: Parser TransGraphRelationalOperator                     
transGraphRelationalOpP = showTransGraphRelationalOpP
  
showTransGraphRelationalOpP :: Parser TransGraphRelationalOperator
showTransGraphRelationalOpP = do
  reservedOp ":showtransgraphexpr"
  ShowTransGraphRelation <$> relExprP  
  
evalTransGraphRelationalOp :: C.SessionId -> C.Connection -> TransGraphRelationalOperator -> IO ConsoleResult
evalTransGraphRelationalOp sessionId conn (ShowTransGraphRelation expr) = do
  res <- C.executeTransGraphRelationalExpr sessionId conn expr
  case res of
    Left err -> pure $ DisplayErrorResult $ T.pack (show err)
    Right rel -> pure $ DisplayRelationResult rel
    

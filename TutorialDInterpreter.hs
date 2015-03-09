{-# LANGUAGE GADTs #-}
module TutorialDInterpreter where
import Relation
import RelationType
import RelationTuple
import RelationTupleSet
import RelationalError
import RelationExpr
import RelationTerm
import RelationalTransaction
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token
import qualified Data.Set as S
import qualified Data.HashSet as HS
import qualified Data.Map as M
import qualified Data.List as L
import Control.Applicative (liftA, (<*), (*>))
import Control.Monad.State
import System.Console.Haskeline
import System.IO
import System.Directory (getHomeDirectory)
import qualified Data.UUID as U
import Data.UUID.V4 (nextRandom)

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser tutD
        where tutD = emptyDef {
                Token.reservedOpNames = ["join", "where", "union", "group", "ungroup"],
                Token.reservedNames = [],
                Token.identStart = letter <|> char '_' <|> char '#',
                Token.identLetter = alphaNum <|> char '_' <|> char '#'}

parens = Token.parens lexer
reservedOp = Token.reservedOp lexer
reserved = Token.reserved lexer
braces = Token.braces lexer
identifier = Token.identifier lexer
comma = Token.comma lexer
semi = Token.semi lexer

uuidP :: Parser U.UUID
uuidP = do
  uuidStr <- many (alphaNum <|> char '-')
  case U.fromString uuidStr of
    Nothing -> fail "Invalid uuid string"
    Just uuid -> return uuid
                    
--used in projection
attributeList :: Parser [AttributeName]
attributeList = sepBy identifier comma

makeRelation :: Parser RelationalExpr
makeRelation = do
  reservedOp "relation"
  attrs <- (try makeAttributes <|> return M.empty)
  if not (M.null attrs) then do
    return $ MakeStaticRelation attrs emptyTupleSet
    else do
    tuples <- braces (sepBy tupleP comma)
    return $ MakeStaticRelation (tupleAttributes (head tuples)) (HS.fromList tuples)

--used in relation creation
makeAttributes :: Parser Attributes
makeAttributes = do
   attrList <- braces (sepBy attributeAndType comma)
   return $ M.fromList $ map toAttributeAssocList attrList
     where
       toAttributeAssocList attr@(Attribute attrName _) = (attrName, attr)

attributeAndType :: Parser Attribute
attributeAndType = do
  attrName <- identifier
  attrTypeName <- identifier
  --convert type name into type
  case tutDTypeToAtomType attrTypeName of
    Just t -> return $ Attribute attrName t
    Nothing -> fail (attrTypeName ++ " is not a valid type name.")
    
tupleP :: Parser RelationTuple    
tupleP = do
  reservedOp "tuple"
  attrValues <- braces (sepBy tupleAtomP comma)
  return $ RelationTuple (M.fromList attrValues)
  
tupleAtomP :: Parser (AttributeName, Atom)
tupleAtomP = do
  attributeName <- identifier
  attrType <- identifier
  atom <- parens (stringAtomP <|> intAtomP)
  case tutDTypeToAtomType attrType of
    Nothing -> fail (attrType ++ " is not a valid type name.")
    Just typeA -> if typeA == atomTypeForAtom atom then
                    return $ (attributeName, atom)
                  else
                    fail "type mismatch in tuple generation"
  
--convert Tutorial D type to AtomType
tutDTypeToAtomType :: String -> Maybe AtomType
tutDTypeToAtomType tutDType = case tutDType of
  "char" -> Just StringAtomType
  "int" -> Just IntAtomType
  _ -> Nothing

atomTypeToTutDType :: AtomType -> Maybe String
atomTypeToTutDType atomType = case atomType of
  StringAtomType -> Just "char"
  IntAtomType -> Just "int"
  --RelationAtomType rel -> 
  _ -> Nothing

relVarP :: Parser RelationalExpr
relVarP = liftA RelationVariable identifier

relTerm = parens relExpr
          <|> makeRelation
          <|> relVarP
          
projectOp = do
  attrs <- braces attributeList
  return $ Project (S.fromList attrs)
  
assignP :: Parser DatabaseExpr
assignP = do
  relVarName <- try $ do
    relVarName <- identifier
    reservedOp ":="
    return relVarName
  expr <- relExpr
  return $ Assign relVarName expr
  
renameClause = do
  oldAttr <- identifier 
  reservedOp "as"
  newAttr <- identifier
  return $ (oldAttr, newAttr)
  
renameP :: Parser (RelationalExpr -> RelationalExpr)
renameP = do
  reservedOp "rename"
  (oldAttr, newAttr) <- braces renameClause
  return $ Rename oldAttr newAttr 
  
whereClauseP = do
  reservedOp "where"  
  boolExpr <- restrictionPredicateP
  return $ Restrict boolExpr
  
groupClause = do  
  attrs <- braces attributeList
  reservedOp "as"
  newAttrName <- identifier
  return $ (attrs, newAttrName)
  
groupP :: Parser (RelationalExpr -> RelationalExpr)
groupP = do
  reservedOp "group"
  (groupAttrList, groupAttrName) <- parens groupClause
  return $ Group (S.fromList groupAttrList) groupAttrName
  
--in "Time and Relational Theory" (2014), Date's Tutorial D grammar for ungroup takes one attribute, while in previous books, it take multiple arguments. Let us assume that nested ungroups are the same as multiple attributes.
ungroupP :: Parser (RelationalExpr -> RelationalExpr)
ungroupP = do
  reservedOp "ungroup"
  rvaAttrName <- identifier
  return $ Ungroup rvaAttrName
  
relOperators = [
  [Postfix projectOp],
  [Postfix renameP],
  [Postfix whereClauseP],
  [Postfix groupP],
  [Postfix ungroupP],
  [Infix (reservedOp "join" >> return Join) AssocLeft],
  [Infix (reservedOp "union" >> return Union) AssocLeft],
  [Infix (reservedOp "=" >> return Equals) AssocNone]
  ]

relExpr :: Parser RelationalExpr
relExpr = buildExpressionParser relOperators relTerm

databaseExpr :: Parser DatabaseExpr
databaseExpr = insertP
            <|> deleteP
            <|> updateP
            <|> constraintP
            <|> defineP
            <|> assignP
            
multipleDatabaseExpr :: Parser DatabaseExpr
multipleDatabaseExpr = do
  exprs <- sepBy1 databaseExpr semi
  return $ MultipleExpr exprs
  
insertP :: Parser DatabaseExpr
insertP = do
  reservedOp "insert"
  relvar <- identifier
  expr <- relExpr
  return $ Insert relvar expr
  
defineP :: Parser DatabaseExpr
defineP = do
  relVarName <- try $ do
    relVarName <- identifier
    reservedOp "::"
    return relVarName
  attributes <- makeAttributes
  return $ Define relVarName attributes
  
deleteP :: Parser DatabaseExpr  
deleteP = do
  reservedOp "delete"
  relVarName <- identifier
  return $ Delete relVarName
  
updateP :: Parser DatabaseExpr
updateP = do
  reservedOp "update"
  relVarName <- identifier  
  predicate <- option TruePredicate (reservedOp "where" *> restrictionPredicateP <* spaces)
  attributeAssignments <- liftM M.fromList $ parens (sepBy attributeAssignment comma)
  return $ Update relVarName attributeAssignments predicate
  
constraintP :: Parser DatabaseExpr
constraintP = do
  reservedOp "constraint"
  constraintName <- identifier
  subset <- relExpr
  reservedOp "in"
  superset <- relExpr
  return $ AddInclusionDependency (InclusionDependency constraintName subset superset)
  
attributeAssignment :: Parser (String, Atom)
attributeAssignment = do
  attrName <- identifier
  reservedOp ":="
  atom <- stringAtomP <|> intAtomP
  return $ (attrName, atom)
  
parseString :: String -> Either RelationalError DatabaseExpr
parseString str = case parse multipleDatabaseExpr "" str of
  Left err -> Left $ ParseError (show err)
  Right r -> Right r

--operators which only rely on database context reading
data ContextOperator where
  ShowRelation :: RelationalExpr -> ContextOperator
  ShowRelationType :: RelationalExpr -> ContextOperator
  ShowConstraint :: String -> ContextOperator
  Quit :: ContextOperator
  deriving (Show)

--operators which manipulate a transaction graph
data GraphOperator where
  JumpToHead :: String -> GraphOperator
  JumpToTransaction :: U.UUID -> GraphOperator
  Branch :: HeadName -> GraphOperator
  Commit :: GraphOperator
  Rollback :: GraphOperator
  deriving (Show)
  
typeP :: Parser ContextOperator  
typeP = do
  reservedOp ":type"
  expr <- relExpr
  return $ ShowRelationType expr
  
showRelP :: Parser ContextOperator
showRelP = do
  reservedOp ":show"
  expr <- relExpr
  return $ ShowRelation expr
  
quitP :: Parser ContextOperator
quitP = do
  reservedOp ":quit"
  return Quit
  
showConstraintsP :: Parser ContextOperator
showConstraintsP = do
  reservedOp ":constraints"
  constraintName <- option "" identifier
  return $ ShowConstraint constraintName
  
jumpToHeadP :: Parser GraphOperator
jumpToHeadP = do
  reservedOp ":jumphead"
  head <- identifier
  return $ JumpToHead head
  
jumpToTransactionP :: Parser GraphOperator  
jumpToTransactionP = do
  reservedOp ":jump"
  uuid <- uuidP 
  return $ JumpToTransaction uuid
  
branchTransactionP :: Parser GraphOperator  
branchTransactionP = do
  reservedOp ":branch"
  branchName <- identifier
  return $ Branch branchName
  
commitTransactionP :: Parser GraphOperator  
commitTransactionP = do
  reservedOp ":commit"
  return $ Commit
  
rollbackTransactionP :: Parser GraphOperator
rollbackTransactionP = do
  reservedOp ":rollback"
  return $ Rollback
  
transactionGraphOps = do
  jumpToHeadP
  <|> jumpToTransactionP
  <|> branchTransactionP
  <|> commitTransactionP
  <|> rollbackTransactionP

contextOps :: Parser ContextOperator
contextOps = typeP 
             <|> showRelP
             <|> showConstraintsP
             <|> quitP
             
interpreterOps :: Parser (Either ContextOperator GraphOperator)             
interpreterOps = liftM Left contextOps <|> liftM Right transactionGraphOps

showRelationAttributes :: Relation -> String
showRelationAttributes rel = "{" ++ concat (L.intersperse ", " $ map showAttribute attrs) ++ "}"
  where
    showAttribute (Attribute name atomType) = name ++ " " ++ case atomTypeToTutDType atomType of
      Just t -> show t
      Nothing -> "unknown"
    attrs = values (attributes rel)
    values m = map snd (M.toAscList m)
    
data TutorialDOperatorResult = QuitResult |
                               DisplayResult String |
                               DisplayErrorResult String |
                               NoActionResult
    
evalContextOp :: DatabaseContext -> ContextOperator -> TutorialDOperatorResult
evalContextOp context (ShowRelationType expr) = case runState (typeForRelationalExpr expr) context of
  (Right rel, _) -> DisplayResult $ showRelationAttributes rel
  (Left err, _) -> DisplayErrorResult $ show err
  
evalContextOp context (ShowRelation expr) = do
  case runState (evalRelationalExpr expr) context of 
    (Left err, _) -> DisplayErrorResult $ show err
    (Right rel, _) -> DisplayResult $ showRelation rel
    
evalContextOp context (ShowConstraint name) = do
  DisplayResult $ show filteredDeps
  where
    deps = inclusionDependencies context
    filteredDeps = case name of
      "" -> deps
      name -> HS.filter (\(InclusionDependency n _ _) -> n == name) deps
      
--I need to pass the transaction graph around instead of the singular context :/
evalContextOp context (Quit) = QuitResult

-- returns the new "current" transaction, updated graph, and tutorial d result
-- the current transaction is not part of the transaction graph until it is committed
evalGraphOp :: U.UUID -> DisconnectedTransaction -> TransactionGraph -> GraphOperator -> Either RelationalError (DisconnectedTransaction, TransactionGraph)

--affects only disconncted transaction
evalGraphOp newUUID trans graph (JumpToTransaction jumpUUID) = case transactionForUUID jumpUUID graph of
  Nothing -> Left $ NoSuchTransactionError jumpUUID
  Just parentTrans -> Right (newTrans, graph)
    where
      newTrans = DisconnectedTransaction jumpUUID (transactionContext parentTrans)
  
-- switch from one head to another
-- affects only disconnectedtransaction 
evalGraphOp newUUID currentTransaction graph (JumpToHead headName) = 
  case transactionForHead headName graph of
    Just newHeadTransaction -> let disconnectedTrans = newDisconnectedTransaction (transactionUUID newHeadTransaction) (transactionContext newHeadTransaction) in
      Right (disconnectedTrans, graph)
    Nothing -> Left $ NoSuchHeadNameError headName
-- add new head pointing to branchPoint
-- repoint the disconnected transaction to the new branch commit (with a potentially different disconnected context)
-- affects transactiongraph and the disconnectedtransaction is recreated based off the branch
    {-
evalGraphOp newUUID discon@(DisconnectedTransaction parentUUID disconContext) graph (Branch newBranchName) = case transactionForUUID parentUUID graph of
  Nothing -> (discon, graph, DisplayErrorResult "Failed to find parent transaction.")
  Just parentTrans -> case addBranch newBranchName parentTrans graph of
    Nothing -> (discon, graph, DisplayErrorResult "Failed to add branch.")
    Just newGraph -> (newDiscon, newGraph, DisplayResult "Branched.")
     where
       newDiscon = DisconnectedTransaction (transactionUUID parentTrans) disconContext
-}
       
-- create a new commit and add it to the heads
-- technically, the new head could be added to an existing commit, but by adding a new commit, the new head is unambiguously linked to a new commit (with a context indentical to its parent)
evalGraphOp newUUID discon@(DisconnectedTransaction parentUUID disconContext) graph (Branch newBranchName) = case transactionForUUID parentUUID graph of
  Nothing -> Left $ NoSuchTransactionError parentUUID
  Just parentTrans -> case addBranch newUUID newBranchName parentTrans graph of
    Nothing -> Left $ NoSuchTransactionError parentUUID -- improve error
    Just (newTrans, newGraph) -> Right (newDiscon, newGraph)
     where
      newDiscon = DisconnectedTransaction newUUID disconContext

-- add the disconnected transaction to the graph
-- affects graph and disconnectedtransaction- the new disconnectedtransaction's parent is the freshly committed transaction
evalGraphOp newTransUUID discon@(DisconnectedTransaction parentUUID context) graph Commit = case transactionForUUID parentUUID graph of
  Nothing -> Left $ NoSuchTransactionError parentUUID
  Just parentTransaction -> case headNameForTransaction parentTransaction graph of 
    Nothing -> Left $ TransactionIsNotAHead parentUUID
    Just headName -> case maybeUpdatedGraph of
      Nothing -> Left $ TransactionIsNotAHead parentUUID -- add better error
      Just (_, updatedGraph) -> Right (newDisconnectedTrans, updatedGraph)
      where
        newDisconnectedTrans = newDisconnectedTransaction newTransUUID context
        --parentTransaction = transactionForUUID parentUUID graph
        maybeUpdatedGraph = addDisconnectedTransaction newTransUUID headName discon graph
        
interpret :: DatabaseContext -> String -> (Maybe RelationalError, DatabaseContext)
interpret context tutdstring = case parseString tutdstring of
                                    Left err -> (Just err, context)
                                    Right parsed -> runState (evalContextExpr parsed) context
                                    
-- for interpreter-specific operations                               
interpretOps :: U.UUID -> DisconnectedTransaction -> TransactionGraph -> String -> (DisconnectedTransaction, TransactionGraph, TutorialDOperatorResult)
interpretOps newUUID trans@(DisconnectedTransaction parentUUID context) transGraph instring = case parse interpreterOps "" instring of
  Left err -> (trans, transGraph, NoActionResult)
  Right ops -> case ops of
    Left contextOp -> (trans, transGraph, (evalContextOp context contextOp))
    Right graphOp -> case evalGraphOp newUUID trans transGraph graphOp of
      Left err -> (trans, transGraph, DisplayErrorResult $ show err)
      Right (newDiscon, newGraph) -> (newDiscon, newGraph, DisplayResult "Done.")
  
promptText :: DisconnectedTransaction -> TransactionGraph -> String  
promptText (DisconnectedTransaction parentUUID _) graph = "TutorialD (" ++ transInfo ++ "): "
  where
    transInfo = case transactionForUUID parentUUID graph of 
        Nothing -> "unknown"
        Just parentTrans -> case headNameForTransaction parentTrans graph of
          Nothing -> show $ transactionUUID parentTrans
          Just headName -> headName
  
reprLoop :: DisconnectedTransaction -> TransactionGraph -> IO ()
reprLoop currentTransaction@(DisconnectedTransaction currentParentUUID currentContext) graph = do
  homeDirectory <- getHomeDirectory
  let settings = defaultSettings {historyFile = Just (homeDirectory ++ "/.tutd_history")}
        
  maybeLine <- runInputT settings $ getInputLine (promptText currentTransaction graph)

  let roloop = reprLoop currentTransaction graph
  case maybeLine of
    Nothing -> return ()
    Just line -> do 
    newUUID <- nextRandom
    case interpretOps newUUID currentTransaction graph line of
      (_, _, QuitResult) -> return ()
      (_, _, DisplayErrorResult err) -> hPutStrLn stderr ("ERR: " ++ err) >> roloop
      (updatedTrans, updatedGraph, DisplayResult out) -> do
        putStrLn out
        reprLoop updatedTrans updatedGraph
      (_, _, NoActionResult) -> do
        let (value, contextup) = interpret currentContext line 
        let updatedTransaction = DisconnectedTransaction currentParentUUID contextup
        case value of
          Nothing -> reprLoop updatedTransaction graph
          Just err -> do
                hPutStrLn stderr ("ERR:" ++ show err)
                reprLoop currentTransaction graph
      
restrictionPredicateP :: Parser RestrictionPredicateExpr
restrictionPredicateP = buildExpressionParser predicateOperators predicateTerm
  where
    predicateOperators = [
      [Prefix (reservedOp "not" >> return NotPredicate)],
      [Infix (reservedOp "and" >> return AndPredicate) AssocLeft],
      [Infix (reservedOp "or" >> return OrPredicate) AssocLeft]
      ]
    predicateTerm = parens restrictionPredicateP
                    <|> try restrictionAttributeEqualityP
                    <|> try relationalBooleanExpr

                    
relationalBooleanExpr :: Parser RestrictionPredicateExpr                   
relationalBooleanExpr = do
  relexpr <- relExpr
  return $ RelationalExprPredicate relexpr

restrictionAttributeEqualityP :: Parser RestrictionPredicateExpr
restrictionAttributeEqualityP = do
  attributeName <- identifier
  reservedOp "="
  atom <- stringAtomP <|> intAtomP
  return $ AttributeEqualityPredicate attributeName atom
  
quotedChar = noneOf "\""
           <|> try (string "\"\"" >> return '"')
           
stringAtomP = liftA StringAtom (string "\"" *> many quotedChar <* string "\"")

intAtomP = do
  intstr <- many digit
  return $ IntAtom (read intstr)
  
--used by :dumpGraph
displayTransactionGraph :: TransactionGraph -> String
displayTransactionGraph (TransactionGraph heads transSet) = L.intercalate "\n" $ S.foldr (\(Transaction tUUID pUUID _ ) acc -> acc ++ [show tUUID ++ " " ++ show pUUID]) [] transSet
    

{-# LANGUAGE GADTs,OverloadedStrings #-}
module TutorialD.Interpreter where
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.Tuple
import ProjectM36.TupleSet
import ProjectM36.Error
import qualified ProjectM36.Attribute as A
import ProjectM36.RelationalExpression
import ProjectM36.Relation.Show.Term
import ProjectM36.StaticOptimizer
import ProjectM36.Transaction
import ProjectM36.Transaction.Show
import ProjectM36.Atom
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token
import qualified Data.Set as S
import qualified Data.HashSet as HS
import qualified Data.Map as M
import qualified Data.List as L
import Control.Applicative (liftA, (<*), (*>), (<$>))
import Control.Monad.State
import System.Console.Haskeline
import System.IO
import System.Directory (getHomeDirectory)
import qualified Data.UUID as U
import Data.UUID.V4 (nextRandom)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

{-
List of Tutorial D operators:
show relation expression type :: relexpr -> context -> opresult
show relvar :: relvar name -> context -> opresult
quit :: ()
show graph :: graph -> DisconnectedTransaction -> opresult
show constraints :: context -> opresult
jump to head :: head name -> graph -> DisconnectedTransaction (graph unchanged)
jump to UUID :: uuid -> graph -> DisconnectedTransaction (graph unchanged)
branch :: uuid -> branch name -> graph -> (DisconnectedTransation, graph)
commit :: uuid -> DisconnectedTransaction -> graph -> (DisconnectedTransaction, graph)
rollback :: DisconnectedTransaction -> graph -> (DisconnectedTransaction, graph)


-}
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser tutD
        where tutD = emptyDef {
                Token.reservedOpNames = ["join", "where", "union", "group", "ungroup"],
                Token.reservedNames = [],
                Token.identStart = letter <|> char '_',
                Token.opLetter = oneOf ":!#$%&*+./<=>?\\^|-~", -- remove "@" so it can be used as attribute marker without spaces
                Token.identLetter = alphaNum <|> char '_' <|> char '#'} -- # needed for Date examples

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
attributeList = do 
  attrs <- sepBy identifier comma
  return $ map T.pack attrs

makeRelation :: Parser RelationalExpr
makeRelation = do
  reservedOp "relation"
  attrs <- (try makeAttributes <|> return A.emptyAttributes)
  if not (A.null attrs) then do
    return $ MakeStaticRelation attrs emptyTupleSet
    else do
    tuples <- braces (sepBy tupleP comma)
    return $ MakeStaticRelation (tupleAttributes (head tuples)) (HS.fromList tuples)

--used in relation creation
makeAttributes :: Parser Attributes
makeAttributes = do
   attrList <- braces (sepBy attributeAndType comma)
   return $ A.attributesFromList attrList

attributeAndType :: Parser Attribute
attributeAndType = do
  attrName <- identifier
  attrTypeName <- identifier
  --convert type name into type
  case tutDTypeToAtomType attrTypeName of
    Just t -> return $ Attribute (T.pack attrName) t
    Nothing -> fail (attrTypeName ++ " is not a valid type name.")
    
tupleP :: Parser RelationTuple    
tupleP = do
  reservedOp "tuple"
  attrAssocs <- braces (sepBy tupleAtomP comma)
  let attrs = A.attributesFromList $ map (\(attrName, atom) -> Attribute attrName (atomTypeForAtom atom)) attrAssocs
  let atoms = V.fromList $ map snd attrAssocs
  return $ mkRelationTuple attrs atoms
  
tupleAtomP :: Parser (AttributeName, Atom)
tupleAtomP = do
  attributeName <- identifier
  attrType <- identifier
  atom <- parens (stringAtomP <|> intAtomP)
  case tutDTypeToAtomType attrType of
    Nothing -> fail (attrType ++ " is not a valid type name.")
    Just typeA -> if typeA == atomTypeForAtom atom then
                    return $ (T.pack attributeName, atom)
                  else
                    fail "type mismatch in tuple generation"
  
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

relVarP :: Parser RelationalExpr
relVarP = liftA (RelationVariable . T.pack) identifier

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
  return $ Assign (T.pack relVarName) expr
  
renameClause = do
  oldAttr <- identifier 
  reservedOp "as"
  newAttr <- identifier
  return $ (oldAttr, newAttr)
  
renameP :: Parser (RelationalExpr -> RelationalExpr)
renameP = do
  reservedOp "rename"
  (oldAttr, newAttr) <- braces renameClause
  return $ Rename (T.pack oldAttr) (T.pack newAttr)
  
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
  return $ Group (S.fromList groupAttrList) (T.pack groupAttrName)
  
--in "Time and Relational Theory" (2014), Date's Tutorial D grammar for ungroup takes one attribute, while in previous books, it take multiple arguments. Let us assume that nested ungroups are the same as multiple attributes.
ungroupP :: Parser (RelationalExpr -> RelationalExpr)
ungroupP = do
  reservedOp "ungroup"
  rvaAttrName <- identifier
  return $ Ungroup (T.pack rvaAttrName)
  
extendP :: Parser (RelationalExpr -> RelationalExpr)
extendP = do
  reservedOp ":"
  tupleExpr <- braces tupleExpressionP
  return $ Extend tupleExpr
  
{-
rewrite summarize as extend as evaluate 

summarize SP per (SP{SNO}) add (sum(@QTY) as SUMQ) === ((SP rename {QTY as _}) group ({_} as x)) : {QTY:=sum(@x)}{S#,QTY}
so, in general,
summarize rX per (relExpr) add (func(@attr) as funcd) === ((rX rename {attr as _}) group ({_} as _)) : {QTY:=sum(@_)}{relExpr, _}
-}
{-
summarizeP :: Parser (RelationalExpr -> RelationalExpr)  
summarizeP = do
  reservedOp "per"
  -- "summarize in Tutorial D requires the heading of the per relation to be a subset  of that of the relation to be summarized"
  perExpr <- parens relExpr
  reservedOp "add"
  (funcAtomExpr, newAttrName) <- parens summaryP
  --rewrite summarize as extend
  --return $ Summarize funcAtomExpr newAttrName perExpr
  
summaryP :: Parser (AtomExpr, AttributeName)
summaryP = do
  expr <- atomExprP
  reserved "as"
  attrName <- identifier
  return (expr, T.pack attrName)
-}  

relOperators = [
  [Postfix projectOp],
  [Postfix renameP],
  [Postfix whereClauseP],
  [Postfix groupP],
  [Postfix ungroupP],
  [Infix (reservedOp "join" >> return Join) AssocLeft],
  [Infix (reservedOp "union" >> return Union) AssocLeft],
  [Infix (reservedOp "=" >> return Equals) AssocNone],
  [Postfix extendP]
  ]

relExpr :: Parser RelationalExpr
relExpr = buildExpressionParser relOperators relTerm

databaseExpr :: Parser DatabaseExpr
databaseExpr = insertP
            <|> deleteP
            <|> updateP
            <|> constraintP
            <|> defineP
            <|> undefineP
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
  return $ Insert (T.pack relvar) expr
  
defineP :: Parser DatabaseExpr
defineP = do
  relVarName <- try $ do
    relVarName <- identifier
    reservedOp "::"
    return relVarName
  attributeSet <- makeAttributes
  return $ Define (T.pack relVarName) attributeSet
  
undefineP :: Parser DatabaseExpr
undefineP = do
  reservedOp "undefine"
  relVarName <- identifier
  return $ Undefine (T.pack relVarName)
  
deleteP :: Parser DatabaseExpr  
deleteP = do
  reservedOp "delete"
  relVarName <- identifier
  predicate <- option TruePredicate (reservedOp "where" *> restrictionPredicateP)
  return $ Delete (T.pack relVarName) predicate
  
updateP :: Parser DatabaseExpr
updateP = do
  reservedOp "update"
  relVarName <- identifier  
  predicate <- option TruePredicate (reservedOp "where" *> restrictionPredicateP <* spaces)
  attributeAssignments <- liftM M.fromList $ parens (sepBy attributeAssignment comma)
  return $ Update (T.pack relVarName) (M.mapKeys T.pack $ attributeAssignments) predicate
  
constraintP :: Parser DatabaseExpr
constraintP = do
  reservedOp "constraint"
  constraintName <- identifier
  subset <- relExpr
  reservedOp "in"
  superset <- relExpr
  return $ AddInclusionDependency (InclusionDependency (T.pack constraintName) subset superset)
  
attributeAssignment :: Parser (String, Atom)
attributeAssignment = do
  attrName <- identifier
  reservedOp ":="
  atom <- stringAtomP <|> intAtomP
  return $ (attrName, atom)
  
parseString :: String -> Either RelationalError DatabaseExpr
parseString str = case parse (multipleDatabaseExpr <* eof) "" str of
  Left err -> Left $ ParseError (T.pack (show err))
  Right r -> Right r

--operators which only rely on database context reading
data ContextOperator where
  ShowRelation :: RelationalExpr -> ContextOperator
  ShowRelationType :: RelationalExpr -> ContextOperator
  ShowConstraint :: StringType -> ContextOperator
  ShowPlan :: DatabaseExpr -> ContextOperator
  Quit :: ContextOperator
  deriving (Show)

--operators which manipulate a transaction graph
data GraphOperator where
  JumpToHead :: HeadName -> GraphOperator
  JumpToTransaction :: U.UUID -> GraphOperator
  Branch :: HeadName -> GraphOperator
  Commit :: GraphOperator
  Rollback :: GraphOperator
  ShowGraph :: GraphOperator
  
typeP :: Parser ContextOperator  
typeP = do
  reservedOp ":type"
  expr <- relExpr
  return $ ShowRelationType expr
  
showRelP :: Parser ContextOperator
showRelP = do
  reservedOp ":showexpr"
  expr <- relExpr
  return $ ShowRelation expr
  
showPlanP :: Parser ContextOperator
showPlanP = do
  reservedOp ":showplan"
  expr <- databaseExpr
  return $ ShowPlan expr
  
quitP :: Parser ContextOperator
quitP = do
  reservedOp ":quit"
  return Quit
  
showConstraintsP :: Parser ContextOperator
showConstraintsP = do
  reservedOp ":constraints"
  constraintName <- option "" identifier
  return $ ShowConstraint (T.pack constraintName)
  
jumpToHeadP :: Parser GraphOperator
jumpToHeadP = do
  reservedOp ":jumphead"
  headid <- identifier
  return $ JumpToHead (T.pack headid)
  
jumpToTransactionP :: Parser GraphOperator  
jumpToTransactionP = do
  reservedOp ":jump"
  uuid <- uuidP 
  return $ JumpToTransaction uuid
  
branchTransactionP :: Parser GraphOperator  
branchTransactionP = do
  reservedOp ":branch"
  branchName <- identifier
  return $ Branch (T.pack branchName)
  
commitTransactionP :: Parser GraphOperator  
commitTransactionP = do
  reservedOp ":commit"
  return $ Commit
  
rollbackTransactionP :: Parser GraphOperator
rollbackTransactionP = do
  reservedOp ":rollback"
  return $ Rollback
  
showGraphP :: Parser GraphOperator
showGraphP = do
  reservedOp ":showgraph"
  return $ ShowGraph
  
transactionGraphOps :: Parser GraphOperator
transactionGraphOps = do
  jumpToHeadP
  <|> jumpToTransactionP
  <|> branchTransactionP
  <|> commitTransactionP
  <|> rollbackTransactionP
  <|> showGraphP

contextOps :: Parser ContextOperator
contextOps = typeP 
             <|> showRelP
             <|> showConstraintsP
             <|> showPlanP
             <|> quitP
             
interpreterOps :: Parser (Either ContextOperator GraphOperator)             
interpreterOps = liftM Left contextOps <|> liftM Right transactionGraphOps

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
                               QuietSuccessResult |
                               NoActionResult --refactor to make this dead- there should be only one parser
    
evalContextOp :: DatabaseContext -> ContextOperator -> TutorialDOperatorResult
evalContextOp context (ShowRelationType expr) = case runState (typeForRelationalExpr expr) context of
  (Right rel, _) -> DisplayResult $ showRelationAttributes (attributes rel)
  (Left err, _) -> DisplayErrorResult $ T.pack (show err)
  
evalContextOp context (ShowRelation expr) = do
  case runState (evalRelationalExpr expr) context of 
    (Left err, _) -> DisplayErrorResult $ T.pack (show err)
    (Right rel, _) -> DisplayResult $ showRelation rel
    
evalContextOp context (ShowConstraint name) = do
  DisplayResult $ T.pack (show filteredDeps)
  where
    deps = inclusionDependencies context
    filteredDeps = case name of
      "" -> deps
      name2 -> HS.filter (\(InclusionDependency n _ _) -> n == name2) deps
      
evalContextOp context (ShowPlan dbExpr) = do
  DisplayResult $ T.pack (show plan)
  where
    plan = evalState (applyStaticDatabaseOptimization dbExpr) context
      
evalContextOp _ (Quit) = QuitResult

-- returns the new "current" transaction, updated graph, and tutorial d result
-- the current transaction is not part of the transaction graph until it is committed
evalGraphOp :: U.UUID -> DisconnectedTransaction -> TransactionGraph -> GraphOperator -> Either RelationalError (DisconnectedTransaction, TransactionGraph, TutorialDOperatorResult)

--affects only disconncted transaction
evalGraphOp _ _ graph (JumpToTransaction jumpUUID) = case transactionForUUID jumpUUID graph of
  Left err -> Left err
  Right parentTrans -> Right (newTrans, graph, QuietSuccessResult)
    where
      newTrans = DisconnectedTransaction jumpUUID (transactionContext parentTrans)
  
-- switch from one head to another
-- affects only disconnectedtransaction 
evalGraphOp _ _ graph (JumpToHead headName) = 
  case transactionForHead headName graph of
    Just newHeadTransaction -> let disconnectedTrans = newDisconnectedTransaction (transactionUUID newHeadTransaction) (transactionContext newHeadTransaction) in
      Right (disconnectedTrans, graph, QuietSuccessResult)
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
evalGraphOp newUUID (DisconnectedTransaction parentUUID disconContext) graph (Branch newBranchName) = case transactionForUUID parentUUID graph of
  Left err -> Left err
  Right parentTrans -> case addBranch newUUID newBranchName parentTrans graph of
    Left err -> Left err
    Right (_, newGraph) -> Right (newDiscon, newGraph, QuietSuccessResult)
     where
      newDiscon = DisconnectedTransaction newUUID disconContext

-- add the disconnected transaction to the graph
-- affects graph and disconnectedtransaction- the new disconnectedtransaction's parent is the freshly committed transaction
evalGraphOp newTransUUID discon@(DisconnectedTransaction parentUUID context) graph Commit = case transactionForUUID parentUUID graph of
  Left err -> Left err
  Right parentTransaction -> case headNameForTransaction parentTransaction graph of 
    Nothing -> Left $ TransactionIsNotAHeadError parentUUID
    Just headName -> case maybeUpdatedGraph of
      Left err-> Left err
      Right (_, updatedGraph) -> Right (newDisconnectedTrans, updatedGraph, QuietSuccessResult)
      where
        newDisconnectedTrans = newDisconnectedTransaction newTransUUID context
        maybeUpdatedGraph = addDisconnectedTransaction newTransUUID headName discon graph
        
-- refresh the disconnected transaction, return the same graph        
evalGraphOp _ (DisconnectedTransaction parentUUID _) graph Rollback = case transactionForUUID parentUUID graph of
  Left err -> Left err
  Right parentTransaction -> Right (newDiscon, graph, QuietSuccessResult)
    where
      newDiscon = newDisconnectedTransaction parentUUID (transactionContext parentTransaction)
      
--display transaction graph as relation
evalGraphOp _ discon graph ShowGraph = do
  graphStr <- graphAsRelation discon graph
  return (discon, graph, DisplayResult $ showRelation graphStr)
        
--shouldn't this be Either RelationalError DatabaseContext?
interpret :: DatabaseContext -> String -> (Maybe RelationalError, DatabaseContext)
interpret context tutdstring = case parseString tutdstring of
                                    Left err -> (Just err, context)
                                    Right parsed -> runState (evaluator parsed) context
                               where 
                                 evaluator expr = do                                   
                                   optExpr <- applyStaticDatabaseOptimization expr 
                                   --case optExpr of
                                   case optExpr of
                                     Left err -> return $ Just err
                                     Right optExprA -> evalContextExpr optExprA

--no optimization
interpretNO :: DatabaseContext -> String -> (Maybe RelationalError, DatabaseContext)
interpretNO context tutdstring = case parseString tutdstring of
                                    Left err -> (Just err, context)
                                    Right parsed -> runState (evalContextExpr parsed) context
                                    
-- for interpreter-specific operations
interpretOps :: U.UUID -> DisconnectedTransaction -> TransactionGraph -> String -> (DisconnectedTransaction, TransactionGraph, TutorialDOperatorResult)
interpretOps newUUID trans@(DisconnectedTransaction _ context) transGraph instring = case parse interpreterOps "" instring of
  Left _ -> (trans, transGraph, NoActionResult)
  Right ops -> case ops of
    Left contextOp -> (trans, transGraph, (evalContextOp context contextOp))
    Right graphOp -> case evalGraphOp newUUID trans transGraph graphOp of
      Left err -> (trans, transGraph, DisplayErrorResult $ T.pack (show err))
      Right (newDiscon, newGraph, result) -> (newDiscon, newGraph, result)
  
promptText :: DisconnectedTransaction -> TransactionGraph -> StringType
promptText (DisconnectedTransaction parentUUID _) graph = "TutorialD (" `T.append` transInfo `T.append` "): "
  where
    transInfo = case transactionForUUID parentUUID graph of 
      Left _ -> "unknown"
      Right parentTrans -> case headNameForTransaction parentTrans graph of
          Nothing -> T.pack (show $ transactionUUID parentTrans)
          Just headName -> headName
  
reprLoop :: DisconnectedTransaction -> TransactionGraph -> IO ()
reprLoop currentTransaction@(DisconnectedTransaction currentParentUUID currentContext) graph = do
  homeDirectory <- getHomeDirectory
  let settings = defaultSettings {historyFile = Just (homeDirectory ++ "/.tutd_history")}
        
  maybeLine <- runInputT settings $ getInputLine (T.unpack (promptText currentTransaction graph))

  let roloop = reprLoop currentTransaction graph
  case maybeLine of
    Nothing -> return ()
    Just line -> do 
    newUUID <- nextRandom
    case interpretOps newUUID currentTransaction graph line of
      (_, _, QuitResult) -> return ()
      (_, _, DisplayErrorResult err) -> TIO.hPutStrLn stderr ("ERR: " `T.append` err) >> roloop
      (updatedTrans, updatedGraph, DisplayResult out) -> do
        TIO.putStrLn out
        reprLoop updatedTrans updatedGraph
      (updatedTrans, updatedGraph, QuietSuccessResult) -> do
        TIO.putStrLn "Done."
        reprLoop updatedTrans updatedGraph
      (_, _, NoActionResult) -> do
        let (value, contextup) = interpret currentContext line 
        let updatedTransaction = DisconnectedTransaction currentParentUUID contextup
        case value of
          Nothing -> reprLoop updatedTransaction graph
          Just err -> do
                TIO.hPutStrLn stderr ("ERR:" `T.append` T.pack (show err))
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
  atomexpr <- atomExprP
  return $ AttributeEqualityPredicate (T.pack attributeName) atomexpr
  
quotedChar = noneOf "\""
           <|> try (string "\"\"" >> return '"')
           
stringAtomP = liftA (StringAtom . T.pack) (string "\"" *> many quotedChar <* string "\"")

intAtomP = do
  intstr <- many1 digit
  return $ IntAtom (read intstr)
  
--used by :dumpGraph
{-
displayTransactionGraph :: TransactionGraph -> String
displayTransactionGraph (TransactionGraph _ transSet) = L.intercalate "\n" $ S.foldr (\(Transaction tUUID pUUID _ ) acc -> acc ++ [show tUUID ++ " " ++ show pUUID]) [] transSet
-}    

multiTupleExpressionP :: Parser [TupleExpr]
multiTupleExpressionP = sepBy tupleExpressionP comma

tupleExpressionP :: Parser TupleExpr
tupleExpressionP = attributeTupleExpressionP

attributeTupleExpressionP :: Parser TupleExpr
attributeTupleExpressionP = do
  newAttr <- identifier
  reservedOp ":="
  atom <- atomExprP
  return $ AttributeTupleExpr (T.pack newAttr) atom
                   
atomExprP :: Parser AtomExpr
atomExprP = try functionAtomExprP <|>
  attributeAtomExprP <|>  
  nakedAtomExprP <|>
  relationalAtomExprP

attributeAtomExprP :: Parser AtomExpr            
attributeAtomExprP = do
  _ <- string "@"
  attrName <- identifier
  return $ AttributeAtomExpr (T.pack attrName)

nakedAtomExprP :: Parser AtomExpr
nakedAtomExprP = NakedAtomExpr <$> (stringAtomP <|> intAtomP)

functionAtomExprP :: Parser AtomExpr
functionAtomExprP = do
  funcName <- identifier
  argList <- parens (sepBy atomExprP comma)
  return $ FunctionAtomExpr (T.pack funcName) argList
  
relationalAtomExprP :: Parser AtomExpr  
relationalAtomExprP = RelationAtomExpr <$> relExpr

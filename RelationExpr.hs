{-# LANGUAGE GADTs #-}
module RelationExpr where
import Relation
import RelationTuple
import RelationType
import RelationHTML
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.HashSet as HS
import Control.Monad.State hiding (join)
import Debug.Trace

type RelVarContext = M.Map String Relation

type AtomName = String

data RelationalExpr where
  MakeStaticRelation :: Attributes -> RelationTupleSet -> RelationalExpr
  --MakeFunctionalRelation (creates a relation from a tuple-generating function, potentially infinite)
  --in Tutorial D, relational variables pick up the type of the first relation assigned to them
  --relational variables should also be able to be explicitly-typed like in Haskell
  RelationVariable :: String -> RelationalExpr
  Project :: S.Set AttributeName -> RelationalExpr -> RelationalExpr
  Union :: RelationalExpr -> RelationalExpr -> RelationalExpr
  Join :: RelationalExpr -> RelationalExpr -> RelationalExpr
  Assign :: String -> RelationalExpr -> RelationalExpr
  MultipleExpr :: [RelationalExpr] -> RelationalExpr
  Rename :: AttributeName -> AttributeName -> RelationalExpr -> RelationalExpr
  --Restrict :: RExpr.RestrictionExpr -> RelationalExpr -> RelationalExpr
{- maybe break this into multiple steps:
1. check relational types for match (attribute counts) (typechecking step
2. create an execution plan (another system of nodes, another GADT)
3. execute the plan
-}
  deriving (Show)

type RelVarState a = State RelVarContext a

eval :: RelationalExpr -> RelVarState (Either RelationalError Relation)
eval (Assign relVarName expr) = do
  relvarTable <- get
  value <- eval expr
  case value of 
    Right rel -> (put $ M.insert relVarName rel relvarTable) >>
                 (return $ Right rel)
    Left err -> return $ Left err

eval (RelationVariable name) = do
  relvarTable <- get
  return $ case M.lookup name relvarTable of
    Just res -> Right res
    Nothing -> Left (RelationalError 1 (name ++ " has not been defined."))

eval (Project attrNameSet expr) = do
    rel <- eval expr
    case rel of 
      Right rel -> return $ project attrNameSet rel
      Left err -> return $ Left err

eval (Union exprA exprB) = do
  relA <- eval exprA
  relB <- eval exprB
  case relA of
    Left err -> return $ Left err
    Right relA -> case relB of
      Left err -> return $ Left err
      Right relB -> return $ union relA relB

eval (Join exprA exprB) = do
  relA <- eval exprA
  relB <- eval exprB
  case relA of
    Left err -> return $ Left err
    Right relA -> case relB of
      Left err -> return $ Left err
      Right relB -> return $ join relA relB
      
eval (MultipleExpr exprs) = do      
  evald <- mapM eval exprs
  return $ last evald
  
eval (MakeStaticRelation attributes tupleSet) = do
  case mkRelation attributes tupleSet of
    Right rel -> return $ Right rel
    Left err -> return $ Left err
    
eval (Rename oldAttrName newAttrName relExpr) = do
  evald <- eval relExpr
  case evald of
    Right rel -> return $ rename oldAttrName newAttrName rel
    Left err -> return $ Left err
      
emptyRelVarContext :: RelVarContext
emptyRelVarContext = M.empty

basicRelVarContext :: RelVarContext           
basicRelVarContext = M.fromList [("true", relationTrue),
                                 ("false", relationFalse)]

dateExamples :: RelVarContext
dateExamples = M.union basicRelVarContext dateRelVars
  where
    dateRelVars = M.fromList [("S", suppliers),
                              ("P", products),
                              ("SP", supplierProducts)]
    suppliers = suppliersRel
    products = productsRel
    supplierProducts = supplierProductsRel
      
suppliersRel = case mkRelation attributes tupleSet of
  Right rel -> rel
  where
    attributes = M.fromList [("S#", Attribute "S#" StringAtomType), 
                                 ("SNAME", Attribute "SNAME" StringAtomType), 
                                 ("STATUS", Attribute "STATUS" StringAtomType), 
                                 ("CITY", Attribute "CITY" StringAtomType)] 
    tupleSet = HS.fromList $ mkRelationTuples attributes [
      M.fromList [("S#", StringAtom "S1") , ("SNAME", StringAtom "Smith"), ("STATUS", IntAtom 20) , ("CITY", StringAtom "London")],
      M.fromList [("S#", StringAtom "S2"), ("SNAME", StringAtom "Jones"), ("STATUS", IntAtom 10), ("CITY", StringAtom "Paris")],
      M.fromList [("S#", StringAtom "S3"), ("SNAME", StringAtom "Blake"), ("STATUS", IntAtom 30), ("CITY", StringAtom "Paris")],
      M.fromList [("S#", StringAtom "S4"), ("SNAME", StringAtom "Clark"), ("STATUS", IntAtom 20), ("CITY", StringAtom "London")],
      M.fromList [("S#", StringAtom "S5"), ("SNAME", StringAtom "Adams"), ("STATUS", IntAtom 30), ("CITY", StringAtom "Athens")]]
      
productsRel = case mkRelation attributes tupleSet of
  Right rel -> rel
  where
    attributes = M.fromList [("P#", Attribute "P#" StringAtomType), 
                             ("PNAME", Attribute "PNAME" StringAtomType),
                             ("COLOR", Attribute "COLOR" StringAtomType), 
                             ("WEIGHT", Attribute "WEIGHT" StringAtomType), 
                             ("CITY", Attribute "CITY" StringAtomType)]
    tupleSet = HS.fromList $ mkRelationTuples attributes [
      M.fromList [("P#", StringAtom "P1"), ("PNAME", StringAtom "Nut"), ("COLOR", StringAtom "Red"), ("WEIGHT", IntAtom 12), ("CITY", StringAtom "London")],
      M.fromList [("P#", StringAtom "P2"), ("PNAME", StringAtom "Bolt"), ("COLOR", StringAtom "Green"), ("WEIGHT", IntAtom 17), ("CITY", StringAtom "Paris")],
      M.fromList [("P#", StringAtom "P3"), ("PNAME", StringAtom "Screw"), ("COLOR", StringAtom "Blue"), ("WEIGHT", IntAtom 17), ("CITY", StringAtom "Oslo")],      
      M.fromList [("P#", StringAtom "P4"), ("PNAME", StringAtom "Screw"), ("COLOR", StringAtom "Red"), ("WEIGHT", IntAtom 14), ("CITY", StringAtom "London")],
      M.fromList [("P#", StringAtom "P5"), ("PNAME", StringAtom "Cam"), ("COLOR", StringAtom "Blue"), ("WEIGHT", IntAtom 12), ("CITY", StringAtom "Paris")],
      M.fromList [("P#", StringAtom "P6"), ("PNAME", StringAtom "Cog"), ("COLOR", StringAtom "Red"), ("WEIGHT", IntAtom 19), ("CITY", StringAtom "London")]

      ]
                              
supplierProductsRel = case mkRelation attributes tupleSet of
  Right rel -> rel
  where
      attributes = M.fromList [("S#", Attribute "S#" StringAtomType), 
                               ("P#", Attribute "P#" StringAtomType), 
                               ("QTY", Attribute "QTY" StringAtomType)]                 
      tupleSet = HS.fromList $ mkRelationTuples attributes [
        M.fromList [("S#", StringAtom "S1"), ("P#", StringAtom "P1"), ("QTY", IntAtom 300)],
        M.fromList [("S#", StringAtom "S1"), ("P#", StringAtom "P2"), ("QTY", IntAtom 200)],
        M.fromList [("S#", StringAtom "S1"), ("P#", StringAtom "P3"), ("QTY", IntAtom 400)],
        M.fromList [("S#", StringAtom "S1"), ("P#", StringAtom "P4"), ("QTY", IntAtom 200)],    
        M.fromList [("S#", StringAtom "S1"), ("P#", StringAtom "P5"), ("QTY", IntAtom 100)],   
        M.fromList [("S#", StringAtom "S1"), ("P#", StringAtom "P6"), ("QTY", IntAtom 100)],
        
        M.fromList [("S#", StringAtom "S2"), ("P#", StringAtom "P1"), ("QTY", IntAtom 300)],
        M.fromList [("S#", StringAtom "S2"), ("P#", StringAtom "P2"), ("QTY", IntAtom 400)],

        M.fromList [("S#", StringAtom "S3"), ("P#", StringAtom "P2"), ("QTY", IntAtom 200)],  
        
        M.fromList [("S#", StringAtom "S4"), ("P#", StringAtom "P2"), ("QTY", IntAtom 200)],    
        M.fromList [("S#", StringAtom "S4"), ("P#", StringAtom "P4"), ("QTY", IntAtom 300)],
        M.fromList [("S#", StringAtom "S4"), ("P#", StringAtom "P5"), ("QTY", IntAtom 400)]   
        ]

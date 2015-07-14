{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.TransactionGraph.Show where
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.Tuple
import ProjectM36.TupleSet
import ProjectM36.Error
import ProjectM36.TransactionGraph
import qualified ProjectM36.Attribute as A
import qualified Data.HashSet as HS
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Text as T

showTransactionStructure :: Transaction -> TransactionGraph -> String
showTransactionStructure trans graph = headInfo ++ " " ++ show (transactionUUID trans) ++ " " ++ parentTransactionsInfo
  where
    headInfo = maybe "" show (headNameForTransaction trans graph)
    parentTransactionsInfo = if isRootTransaction trans graph then "root" else case parentTransactions trans graph of
      Left err -> show err
      Right parentTransSet -> concat $ S.toList $ S.map (show . transactionUUID) parentTransSet
  
showGraphStructure :: TransactionGraph -> String
showGraphStructure graph@(TransactionGraph _ transSet) = S.foldr folder "" transSet
  where
    folder trans acc = acc ++ showTransactionStructure trans graph ++ "\n"
    
--present a transaction graph as a relation showing the uuids, parentuuids, and flag for the current location of the disconnected transaction    
graphAsRelation :: DisconnectedTransaction -> TransactionGraph -> Either RelationalError Relation    
graphAsRelation (DisconnectedTransaction parentUUID _) graph@(TransactionGraph _ transSet) = do
  tupleMatrix <- mapM tupleGenerator (S.toList transSet)
  mkRelationFromList attrs tupleMatrix
  where
    attrs = A.attributesFromList [Attribute "id" StringAtomType,
                                  Attribute "parents" (RelationAtomType parentAttributes),
                                  Attribute "current" IntAtomType,
                                  Attribute "head" StringAtomType
                                 ]
    parentAttributes = A.attributesFromList [Attribute "id" StringAtomType]
    tupleGenerator transaction = case transactionParentsRelation transaction graph of
      Left err -> Left err
      Right parentTransRel -> Right [StringAtom $ T.pack $ show (transactionUUID transaction),
                                     RelationAtom parentTransRel,
                                     IntAtom $ if parentUUID == transactionUUID transaction then 1 else 0,
                                     StringAtom $ case headNameForTransaction transaction graph of
                                       Just headName -> headName
                                       Nothing -> ""
                                      ]
                              
transactionParentsRelation :: Transaction -> TransactionGraph -> Either RelationalError Relation
transactionParentsRelation trans graph = do
  if isRootTransaction trans graph then do
    mkRelation attrs emptyTupleSet
    else do
      parentTransSet <- parentTransactions trans graph
      let tupleSet = HS.fromList $ map trans2tuple (S.toList parentTransSet)
      mkRelation attrs tupleSet
  where
    attrs = A.attributesFromList [Attribute "id" StringAtomType]
    trans2tuple trans2 = mkRelationTuple attrs $ V.singleton (StringAtom (T.pack (show $ transactionUUID trans2)))


                             
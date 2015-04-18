module RelationalTransactionShow where
import RelationType
import Relation
import RelationTuple
import RelationTupleSet
import RelationalError
import RelationalTransaction
import qualified Data.Map as M
import qualified Data.HashSet as HS
import qualified Data.Set as S

showTransactionStructure :: Transaction -> TransactionGraph -> String
showTransactionStructure trans graph = headInfo ++ " " ++ show (transactionUUID trans) ++ " " ++ parentTransactionsInfo
  where
    headInfo = maybe "" show (headNameForTransaction trans graph)
    parentTransactionsInfo = if isRootTransaction trans graph then "root" else case parentTransactions trans graph of
      Left err -> show err
      Right parentTransSet -> concat $ S.toList $ S.map (show . transactionUUID) parentTransSet
  
showGraphStructure :: TransactionGraph -> String
showGraphStructure graph@(TransactionGraph heads transSet) = S.foldr folder "" transSet
  where
    folder trans acc = acc ++ showTransactionStructure trans graph ++ "\n"
    
--present a transaction graph as a relation showing the uuids, parentuuids, and flag for the current location of the disconnected transaction    
graphAsRelation :: DisconnectedTransaction -> TransactionGraph -> Either RelationalError Relation    
graphAsRelation discon@(DisconnectedTransaction parentUUID _) graph@(TransactionGraph heads transSet) = do
  tupleSet <- mapM tupleGenerator (S.toList transSet)
  mkRelation attributes (HS.fromList tupleSet)
  where
    attributes = M.fromList [("id", Attribute "id" StringAtomType), 
                             ("parents", Attribute "parents" (RelationAtomType parentAttributes)),
                             ("current", Attribute "current" IntAtomType),
                             ("head", Attribute "head" StringAtomType)
                             ]
    parentAttributes = M.fromList [("id", Attribute "id" StringAtomType)]
    tupleGenerator transaction = case transactionParentsRelation transaction graph of
      Left err -> Left err
      Right parentTransRel -> Right $ mkRelationTuple (S.fromList ["id", "parents", "current", "head"]) $ 
                              (M.fromList 
                               [("id", StringAtom $ show (transactionUUID transaction)),
                                ("parents", RelationAtom parentTransRel),
                                ("current", IntAtom $ if parentUUID == transactionUUID transaction then 1 else 0),
                                ("head", StringAtom $ case headNameForTransaction transaction graph of
                                    Just headName -> headName
                                    Nothing -> "_")
                                      ])
                              
transactionParentsRelation :: Transaction -> TransactionGraph -> Either RelationalError Relation
transactionParentsRelation trans graph = do
  if isRootTransaction trans graph then do
    mkRelation attributes emptyTupleSet
    else do
      parentTransSet <- parentTransactions trans graph
      let tupleSet = HS.fromList $ map trans2tuple (S.toList parentTransSet)
      mkRelation attributes tupleSet
  where
    attributes = M.fromList [("id", Attribute "id" StringAtomType)]
    trans2tuple trans = mkRelationTuple (S.fromList ["id"]) $ M.singleton "id" (StringAtom (show $ transactionUUID trans))


                             
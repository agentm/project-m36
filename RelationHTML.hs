module RelationHTML where
import RelationType
import Relation
import RelationTuple
import qualified Data.HashSet as HS
import qualified Data.List as L
import RelationAttribute

attributesAsHTML :: Attributes -> String
attributesAsHTML attrs = "<tr>" ++ (foldr folder "" attrNameList) ++ "</tr>"
  where 
    folder attrName acc = acc ++ "<th>" ++ attrName ++ "</th>"
    attrNameList = sortedAttributeNameList (attributeNameSet attrs)

relationAsHTML :: Relation -> String
relationAsHTML rel@(Relation attrNameSet tupleSet) = "<table border=\"1\">" ++ (attributesAsHTML attrNameSet) ++ (tupleSetAsHTML tupleSet) ++ "<tfoot>" ++ tablefooter ++ "</tfoot></table>"
  where
    tablefooter = "<tr><td colspan=\"100%\">" ++ show (cardinality rel) ++ " tuples</td></tr>"

writeHTML :: String -> IO ()
writeHTML = writeFile "/home/agentm/rel.html"

writeRel :: Relation -> IO ()
writeRel = writeHTML . relationAsHTML 

atomAsHTML :: Atom -> String
atomAsHTML (StringAtom s) = s
atomAsHTML (IntAtom i) = show i
atomAsHTML (RelationAtom rel) = relationAsHTML rel

tupleAsHTML :: RelationTuple -> String                          
tupleAsHTML tuple = "<tr>" ++ L.foldr folder "" (tupleSortedAssocs tuple) ++ "</tr>"
  where
    folder tup acc = acc ++ "<td>" ++ (atomAsHTML (snd tup)) ++ "</td>"

tupleSetAsHTML :: RelationTupleSet -> String
tupleSetAsHTML tupSet = HS.foldr folder "" tupSet
  where
    folder tuple acc = acc ++ tupleAsHTML tuple

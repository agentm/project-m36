{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.Relation.Show.HTML where
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.Tuple
import qualified Data.HashSet as HS
import qualified Data.List as L
import ProjectM36.Attribute
import Data.Text (append, Text, pack)
import qualified Data.Text.IO as TIO

attributesAsHTML :: Attributes -> Text
attributesAsHTML attrs = "<tr>" `append` (foldr folder "" attrNameList) `append` "</tr>"
  where 
    folder attrName acc = acc `append` "<th>" `append` attrName `append` "</th>"
    attrNameList = sortedAttributeNameList (attributeNameSet attrs)

relationAsHTML :: Relation -> Text
relationAsHTML rel@(Relation attrNameSet tupleSet) = "<table border=\"1\">" `append` (attributesAsHTML attrNameSet) `append` (tupleSetAsHTML tupleSet) `append` "<tfoot>" `append` tablefooter `append` "</tfoot></table>"
  where
    tablefooter = "<tr><td colspan=\"100%\">" `append` (pack $ show (cardinality rel)) `append` " tuples</td></tr>"

writeHTML :: Text -> IO ()
writeHTML = TIO.writeFile "/home/agentm/rel.html"

writeRel :: Relation -> IO ()
writeRel = writeHTML . relationAsHTML 

atomAsHTML :: Atom -> Text
atomAsHTML (StringAtom atom) = atom
atomAsHTML (IntAtom int) = pack (show int)
atomAsHTML (RelationAtom rel) = relationAsHTML rel

tupleAsHTML :: RelationTuple -> Text
tupleAsHTML tuple = "<tr>" `append` L.foldr folder "" (tupleSortedAssocs tuple) `append` "</tr>"
  where
    folder tup acc = acc `append` "<td>" `append` (atomAsHTML (snd tup)) `append` "</td>"

tupleSetAsHTML :: RelationTupleSet -> Text
tupleSetAsHTML tupSet = HS.foldr folder "" tupSet
  where
    folder tuple acc = acc `append` tupleAsHTML tuple

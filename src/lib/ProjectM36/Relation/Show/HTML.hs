module ProjectM36.Relation.Show.HTML where
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.Tuple
import ProjectM36.Atom
import ProjectM36.AtomType
import qualified Data.List as L
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Monoid
import qualified Data.Vector as V

attributesAsHTML :: Attributes -> Text
attributesAsHTML attrs = "<tr>" <> T.concat (map oneAttrHTML (V.toList attrs)) <> "</tr>"
  where
    oneAttrHTML attr = "<th>" <> prettyAttribute attr <> "</th>"

relationAsHTML :: Relation -> Text
-- web browsers don't display tables with empty cells or empty headers, so we have to insert some placeholders- it's not technically the same, but looks as expected in the browser
relationAsHTML rel@(Relation attrNameSet tupleSet) 
  | rel == relationTrue = pm36relcss <>
                          tablestart <>
                          "<tr><th></th></tr>" <>
                          "<tr><td></td></tr>" <> 
                          tablefooter <> "</table>"
  | rel == relationFalse = pm36relcss <>
                           tablestart <>
                           "<tr><th></th></tr>" <>
                           tablefooter <> 
                           "</table>"
  | otherwise = pm36relcss <>
                tablestart <> 
                attributesAsHTML attrNameSet <> 
                tupleSetAsHTML tupleSet <> 
                tablefooter <> 
                "</table>"
  where
    pm36relcss = "<style>.pm36relation {empty-cells: show;} .pm36relation tbody td, .pm36relation th { border: 1px solid black;}</style>"
    tablefooter = "<tfoot><tr><td colspan=\"100%\">" <> pack (show (cardinality rel)) <> " tuples</td></tr></tfoot>"
    tablestart = "<table class=\"pm36relation\"\">"

writeHTML :: Text -> IO ()
writeHTML = TIO.writeFile "/home/agentm/rel.html"

writeRel :: Relation -> IO ()
writeRel = writeHTML . relationAsHTML

tupleAsHTML :: RelationTuple -> Text
tupleAsHTML tuple = "<tr>" <> T.concat (L.map tupleFrag (tupleAssocs tuple)) <> "</tr>"
  where
    tupleFrag tup = "<td>" <> atomAsHTML (snd tup) <> "</td>"
    atomAsHTML (RelationAtom rel) = relationAsHTML rel
    atomAsHTML atom = atomToText atom

tupleSetAsHTML :: RelationTupleSet -> Text
tupleSetAsHTML tupSet = foldr folder "" (asList tupSet)
  where
    folder tuple acc = acc <> tupleAsHTML tuple


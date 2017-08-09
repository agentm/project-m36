module ProjectM36.Atom where
import ProjectM36.Base
import ProjectM36.Error
import qualified Data.Text as T
--import Data.Time.Calendar
--import Data.Time.Clock
--import Data.ByteString (ByteString)
import Text.Read
import Data.Monoid

relationForAtom :: Atom -> Either RelationalError Relation
relationForAtom (RelationAtom rel) = Right rel
relationForAtom _ = Left $ AttributeIsNotRelationValuedError ""

makeAtomFromText :: AttributeName -> AtomType -> T.Text -> Either RelationalError Atom
makeAtomFromText _ IntAtomType textIn = maybe ((Left . ParseError) textIn) (Right . IntAtom) (readMaybe (T.unpack textIn))
makeAtomFromText _ DoubleAtomType textIn = maybe ((Left . ParseError) textIn) (Right . DoubleAtom) (readMaybe (T.unpack textIn))
makeAtomFromText _ TextAtomType textIn = maybe ((Left . ParseError) textIn) (Right . TextAtom) (readMaybe (T.unpack textIn))
makeAtomFromText _ DayAtomType textIn = maybe ((Left . ParseError) textIn) (Right . DayAtom) (readMaybe (T.unpack textIn))
makeAtomFromText _ DateTimeAtomType textIn = maybe ((Left . ParseError) textIn) (Right . DateTimeAtom) (readMaybe (T.unpack textIn))
makeAtomFromText _ ByteStringAtomType textIn = maybe ((Left . ParseError) textIn) (Right . ByteStringAtom) (readMaybe (T.unpack textIn))
makeAtomFromText _ BoolAtomType textIn = maybe ((Left . ParseError) textIn) (Right . BoolAtom) (readMaybe (T.unpack textIn))
makeAtomFromText attrName _ _ = Left $ AtomTypeNotSupported attrName

atomToText :: Atom -> T.Text
atomToText (IntAtom i) = (T.pack . show) i
atomToText (DoubleAtom i) = (T.pack . show) i
atomToText (TextAtom i) = (T.pack . show) i --does this break quoting in CSV export?
atomToText (DayAtom i) = (T.pack . show) i
atomToText (DateTimeAtom i) = (T.pack . show) i
atomToText (ByteStringAtom i) = (T.pack . show) i
atomToText (BoolAtom i) = (T.pack . show) i
atomToText (IntervalAtom b e bo be) = beginp <> begin <> "," <> end <> endp
  where beginp = if bo then "(" else "["
        begin = atomToText b
        end = atomToText e
        endp = if be then ")" else "]"

atomToText (RelationAtom i) = (T.pack . show) i
atomToText (ConstructedAtom dConsName _ atoms) = dConsName `T.append` T.intercalate " " (map atomToText atoms)




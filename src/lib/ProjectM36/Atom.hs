module ProjectM36.Atom where
import ProjectM36.Base
import ProjectM36.Error
import qualified Data.Text as T
--import Data.Time.Calendar
--import Data.Time.Clock
--import Data.ByteString (ByteString)
import Data.Monoid

relationForAtom :: Atom -> Either RelationalError Relation
relationForAtom (RelationAtom rel) = Right rel
relationForAtom _ = Left $ AttributeIsNotRelationValuedError ""

atomToText :: Atom -> T.Text
atomToText (IntegerAtom i) = (T.pack . show) i
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
atomToText (ConstructedAtom dConsName _ atoms) = dConsName <> dConsArgs
  where
    parensAtomToText a@(ConstructedAtom _ _ []) = atomToText a
    parensAtomToText a@(ConstructedAtom _ _ _) = "(" <> atomToText a <> ")"
    parensAtomToText a = atomToText a
    
    dConsArgs = case atoms of
      [] -> ""
      args -> " " <> T.intercalate " " (map parensAtomToText args)




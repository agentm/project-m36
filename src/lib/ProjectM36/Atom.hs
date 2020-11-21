module ProjectM36.Atom where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.DataTypes.Interval
import qualified Data.Text as T
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif

relationForAtom :: Atom -> Either RelationalError Relation
relationForAtom (RelationAtom rel) = Right rel
relationForAtom _ = Left $ AttributeIsNotRelationValuedError ""

atomToText :: Atom -> T.Text
atomToText (IntegerAtom i) = (T.pack . show) i
atomToText (IntAtom i) = (T.pack . show) i
atomToText (DoubleAtom i) = (T.pack . show) i
atomToText (TextAtom i) = (T.pack . show) i --quotes necessary for ConstructedAtom subatoms
atomToText (DayAtom i) = (T.pack . show) i
atomToText (DateTimeAtom i) = (T.pack . show) i
atomToText (ByteStringAtom i) = (T.pack . show) i
atomToText (BoolAtom i) = (T.pack . show) i
atomToText (RelationalExprAtom re) = (T.pack . show) re

atomToText (RelationAtom i) = (T.pack . show) i
atomToText (ConstructedAtom dConsName typ atoms) 
  | isIntervalAtomType typ = case atoms of --special handling for printing intervals
    [b, e, BoolAtom bo, BoolAtom be] -> 
      let beginp = if bo then "(" else "["
          begin = atomToText b
          end = atomToText e 
          endp = if be then ")" else "]" in 
      beginp <> begin <> "," <> end <> endp
    _ -> "invalid interval"
  | otherwise = dConsName <> dConsArgs
  where
    parensAtomToText a@(ConstructedAtom _ _ []) = atomToText a
    parensAtomToText a@ConstructedAtom{} = "(" <> atomToText a <> ")"
    parensAtomToText a = atomToText a
    
    dConsArgs = case atoms of
      [] -> ""
      args -> " " <> T.intercalate " " (map parensAtomToText args)




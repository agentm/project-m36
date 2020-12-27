module ProjectM36.DataTypes.Sorting where
import ProjectM36.Base

compareAtoms :: Atom -> Atom -> Ordering
compareAtoms (IntegerAtom i1) (IntegerAtom i2) = compare i1 i2
compareAtoms (IntAtom i1) (IntAtom i2) = compare i1 i2
compareAtoms (DoubleAtom d1) (DoubleAtom d2) = compare d1 d2
compareAtoms (TextAtom t1) (TextAtom t2) = compare t1 t2
compareAtoms (DayAtom d1) (DayAtom d2) = compare d1 d2
compareAtoms (DateTimeAtom d1) (DateTimeAtom d2) = compare d1 d2
compareAtoms (ByteStringAtom b1) (ByteStringAtom b2) = compare b1 b2
compareAtoms (BoolAtom b1) (BoolAtom b2) = compare b1 b2
compareAtoms (RelationAtom _) _ = EQ
compareAtoms ConstructedAtom{} _ = EQ
compareAtoms _ _ = EQ

isSortableAtomType :: AtomType -> Bool
isSortableAtomType typ = case typ of
  IntAtomType -> True
  IntegerAtomType -> True
  DoubleAtomType -> True
  TextAtomType -> True
  DayAtomType -> True
  DateTimeAtomType -> True
  ByteStringAtomType -> False
  BoolAtomType -> True
  RelationalExprAtomType -> False
  RelationAtomType _ -> False
  ConstructedAtomType _ _ -> False
  TypeVariableType _ -> False
  

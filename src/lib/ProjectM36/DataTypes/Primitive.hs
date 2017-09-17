module ProjectM36.DataTypes.Primitive where
import ProjectM36.Base

primitiveTypeConstructorMapping :: TypeConstructorMapping
primitiveTypeConstructorMapping = map (\(name, aType) ->
                                  (PrimitiveTypeConstructorDef name aType, [])) prims
  where
    prims = [("Integer", IntegerAtomType),
             ("Int", IntAtomType),
             ("Text", TextAtomType),
             ("Double", DoubleAtomType),
             ("Bool", BoolAtomType),
             ("ByteString", ByteStringAtomType)
            ]
            
intTypeConstructor :: TypeConstructor            
intTypeConstructor = PrimitiveTypeConstructor "Int" IntAtomType

doubleTypeConstructor :: TypeConstructor
doubleTypeConstructor = PrimitiveTypeConstructor "Double" DoubleAtomType

textTypeConstructor :: TypeConstructor
textTypeConstructor = PrimitiveTypeConstructor "Text" TextAtomType

dayTypeConstructor :: TypeConstructor
dayTypeConstructor = PrimitiveTypeConstructor "Day" DayAtomType

-- | Return the type of an 'Atom'.
atomTypeForAtom :: Atom -> AtomType
atomTypeForAtom (IntAtom _) = IntAtomType
atomTypeForAtom (IntegerAtom _) = IntegerAtomType
atomTypeForAtom (DoubleAtom _) = DoubleAtomType
atomTypeForAtom (TextAtom _) = TextAtomType
atomTypeForAtom (DayAtom _) = DayAtomType
atomTypeForAtom (DateTimeAtom _) = DateTimeAtomType
atomTypeForAtom (ByteStringAtom _) = ByteStringAtomType
atomTypeForAtom (BoolAtom _) = BoolAtomType
atomTypeForAtom (IntervalAtom a _ _ _) = IntervalAtomType (atomTypeForAtom a)
atomTypeForAtom (RelationAtom (Relation attrs _)) = RelationAtomType attrs
atomTypeForAtom (ConstructedAtom _ aType _) = aType

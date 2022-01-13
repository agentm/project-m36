module ProjectM36.DataTypes.Primitive where
import ProjectM36.Base

primitiveTypeConstructorMapping :: TypeConstructorMapping
primitiveTypeConstructorMapping = boolMapping : map (\(name, aType) ->
                                  (PrimitiveTypeConstructorDef name aType, [])) prims
  where
    prims = [("Integer", IntegerAtomType),
             ("Int", IntAtomType),
             ("Text", TextAtomType),
             ("Double", DoubleAtomType),
             ("UUID", UUIDAtomType),
             ("ByteString", ByteStringAtomType),
             ("DateTime", DateTimeAtomType),
             ("Day", DayAtomType)
            ]
    boolMapping = (PrimitiveTypeConstructorDef "Bool" BoolAtomType,
                   [DataConstructorDef "True" [],
                    DataConstructorDef "False" []])
            
intTypeConstructor :: TypeConstructor            
intTypeConstructor = PrimitiveTypeConstructor "Int" IntAtomType

doubleTypeConstructor :: TypeConstructor
doubleTypeConstructor = PrimitiveTypeConstructor "Double" DoubleAtomType

textTypeConstructor :: TypeConstructor
textTypeConstructor = PrimitiveTypeConstructor "Text" TextAtomType

dayTypeConstructor :: TypeConstructor
dayTypeConstructor = PrimitiveTypeConstructor "Day" DayAtomType

dateTimeTypeConstructor :: TypeConstructor
dateTimeTypeConstructor = PrimitiveTypeConstructor "DateTime" DayAtomType

uUIDTypeConstructor :: TypeConstructor
uUIDTypeConstructor = PrimitiveTypeConstructor "UUID" UUIDAtomType

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
atomTypeForAtom (UUIDAtom _) = UUIDAtomType
atomTypeForAtom (RelationAtom (Relation attrs _)) = RelationAtomType attrs
atomTypeForAtom (ConstructedAtom _ aType _) = aType
atomTypeForAtom (RelationalExprAtom _) = RelationalExprAtomType

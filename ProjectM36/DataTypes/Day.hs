module ProjectM36.DataTypes.Day where
import ProjectM36.Base
import ProjectM36.AtomFunctionBody
import ProjectM36.DataTypes.Primitive
import ProjectM36.AtomFunctions.Primitive
import qualified Data.HashSet as HS
import Data.Time.Calendar
import qualified Data.Map as M

dayAtom :: Day -> Atom
dayAtom d = Atom d

dayAtomType :: AtomType
dayAtomType = ConstructedAtomType "Day" M.empty

dayAtomFunctions :: AtomFunctions
dayAtomFunctions = HS.fromList [
  AtomFunction { atomFuncName = "fromGregorian",
                 atomFuncType = [intAtomType, intAtomType, intAtomType, dayAtomType],
                 atomFuncBody = compiledAtomFunctionBody $ \(year:month:day:_) -> ConstructedAtom "Day" dayAtomType [Atom (fromGregorian (fromIntegral (unsafeCast year::Int)) (unsafeCast month) (unsafeCast day))]
                   },
  AtomFunction { atomFuncName = "dayEarlierThan",
                 atomFuncType = [dayAtomType, dayAtomType, boolAtomType],
                 atomFuncBody = compiledAtomFunctionBody $ \((ConstructedAtom _ _ (dayA:_)):(ConstructedAtom _ _ (dayB:_)):_) -> Atom (unsafeCast dayA < (unsafeCast dayB :: Day))          
               }
  ]

dayTypeConstructorMapping :: TypeConstructorMapping
dayTypeConstructorMapping = [(ADTypeConstructorDef "Day" [],
                  [DataConstructorDef "Day" [DataConstructorDefTypeConstructorArg (PrimitiveTypeConstructor "Int" intAtomType)]])]

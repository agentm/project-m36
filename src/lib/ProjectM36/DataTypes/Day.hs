module ProjectM36.DataTypes.Day where
import ProjectM36.Base
import ProjectM36.AtomFunctionBody
import qualified Data.HashSet as HS
import Data.Time.Calendar

dayAtomFunctions :: AtomFunctions
dayAtomFunctions = HS.fromList [
  AtomFunction { atomFuncName = "fromGregorian",
                 atomFuncType = [IntAtomType, IntAtomType, IntAtomType, DayAtomType],
                 atomFuncBody = compiledAtomFunctionBody $ \(IntAtom year:IntAtom month:IntAtom day:_) -> pure $ DayAtom (fromGregorian (fromIntegral year) month day)
                 },
  AtomFunction { atomFuncName = "dayEarlierThan",
                 atomFuncType = [DayAtomType, DayAtomType, BoolAtomType],
                 atomFuncBody = compiledAtomFunctionBody $ \(ConstructedAtom _ _ (IntAtom dayA:_):ConstructedAtom _ _ (IntAtom dayB:_):_) -> pure (BoolAtom (dayA < dayB))
               }
  ]

dayTypeConstructorMapping :: TypeConstructorMapping
dayTypeConstructorMapping = [] -- use fromGregorian

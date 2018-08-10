module ProjectM36.DataTypes.Day where
import ProjectM36.Base
import ProjectM36.AtomFunctionBody
import qualified Data.HashSet as HS
import Data.Time.Calendar

dayAtomFunctions :: AtomFunctions
dayAtomFunctions = HS.fromList [
  AtomFunction { atomFuncName = "fromGregorian",
                 atomFuncType = [IntegerAtomType, IntegerAtomType, IntegerAtomType, DayAtomType],
                 atomFuncBody = compiledAtomFunctionBody $ \(IntegerAtom year:IntegerAtom month:IntegerAtom day:_) -> pure $ DayAtom (fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day))
                 },
  AtomFunction { atomFuncName = "dayEarlierThan",
                 atomFuncType = [DayAtomType, DayAtomType, BoolAtomType],
                 atomFuncBody = compiledAtomFunctionBody $ \(ConstructedAtom _ _ (IntAtom dayA:_):ConstructedAtom _ _ (IntAtom dayB:_):_) -> pure (BoolAtom (dayA < dayB))
               }
  ]

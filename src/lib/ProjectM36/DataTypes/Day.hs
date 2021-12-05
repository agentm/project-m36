module ProjectM36.DataTypes.Day where
import ProjectM36.Base
import ProjectM36.AtomFunctionBody
import qualified Data.HashSet as HS
import Data.Time.Calendar

dayAtomFunctions :: AtomFunctions
dayAtomFunctions = HS.fromList [
  Function { funcName = "fromGregorian",
                 funcType = [IntegerAtomType, IntegerAtomType, IntegerAtomType, DayAtomType],
                 funcBody = compiledAtomFunctionBody $ \(IntegerAtom year:IntegerAtom month:IntegerAtom day:_) -> pure $ DayAtom (fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day))
                 },
  Function { funcName = "dayEarlierThan",
                 funcType = [DayAtomType, DayAtomType, BoolAtomType],
                 funcBody = compiledAtomFunctionBody $ \(ConstructedAtom _ _ (IntAtom dayA:_):ConstructedAtom _ _ (IntAtom dayB:_):_) -> pure (BoolAtom (dayA < dayB))
               }
  ]

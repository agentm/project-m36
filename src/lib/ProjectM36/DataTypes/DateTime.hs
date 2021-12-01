module ProjectM36.DataTypes.DateTime where
import ProjectM36.Base
import ProjectM36.AtomFunctionBody
import qualified Data.HashSet as HS
import Data.Time.Clock.POSIX

dateTimeAtomFunctions :: AtomFunctions
dateTimeAtomFunctions = HS.fromList [ Function {
                                     funcName = "dateTimeFromEpochSeconds",
                                     funcType = [IntegerAtomType, DateTimeAtomType],
                                     funcBody = compiledAtomFunctionBody $ \(IntegerAtom epoch:_) -> pure (DateTimeAtom (posixSecondsToUTCTime (realToFrac epoch)))
                                                                                                       }]

                                                 

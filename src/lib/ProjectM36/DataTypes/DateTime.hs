module ProjectM36.DataTypes.DateTime where
import ProjectM36.Base
import ProjectM36.AtomFunctionBody
import qualified Data.HashSet as HS
import Data.Time.Clock.POSIX
import Data.Time.Clock

dateTimeAtomFunctions :: AtomFunctions
dateTimeAtomFunctions = HS.fromList [ AtomFunction {
                                     atomFuncName = "dateTimeFromEpochSeconds",
                                     atomFuncType = [IntAtomType, DateTimeAtomType],
                                     atomFuncBody = compiledAtomFunctionBody $ \((IntAtom epoch):_) -> pure (DateTimeAtom (posixSecondsToUTCTime (realToFrac epoch)))
                                                                                                       }]

                                                 

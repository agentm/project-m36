--atom functions on primitive atom values plus the basic atom functions
module ProjectM36.AtomFunctions.Basic where
import ProjectM36.Base
import ProjectM36.DataTypes.Day
import ProjectM36.DataTypes.Either
import ProjectM36.DataTypes.Maybe
import ProjectM36.AtomFunctions.Primitive
import ProjectM36.AtomFunction
import qualified Data.HashSet as HS

basicAtomFunctions :: AtomFunctions
basicAtomFunctions = HS.unions [primitiveAtomFunctions, 
                                dayAtomFunctions,
                                eitherAtomFunctions,
                                maybeAtomFunctions]

--these special atom functions aren't scripted so they can't be serialized normally. Instead, the body remains in the binary and the serialization/deserialization happens by name only.
precompiledAtomFunctions :: AtomFunctions
precompiledAtomFunctions = HS.filter (not . isScriptedAtomFunction) basicAtomFunctions
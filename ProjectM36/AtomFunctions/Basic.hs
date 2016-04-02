{-# LANGUAGE OverloadedStrings #-}
--atom functions on primitive atom values plus the basic atom functions
module ProjectM36.AtomFunctions.Basic where
import ProjectM36.Base
import ProjectM36.DataTypes.Day
import ProjectM36.DataTypes.Either
import ProjectM36.DataTypes.Maybe
import ProjectM36.AtomFunctions.Primitive
import qualified Data.HashSet as HS

basicAtomFunctions :: AtomFunctions
basicAtomFunctions = HS.unions [primitiveAtomFunctions, 
                                dayAtomFunctions,
                                eitherAtomFunctions,
                                maybeAtomFunctions]
       
{-# LANGUAGE OverloadedStrings #-}
-- wraps up primitives plus other basic data types
module ProjectM36.DataTypes.Basic where
import ProjectM36.DataTypes.Primitive
import qualified Data.Map as M
import ProjectM36.Base

basicAtomTypes :: AtomTypes
basicAtomTypes = M.union primitiveAtomTypes moreTypes
  where
    moreTypes = M.fromList [("Day", (ConstructedAtomType "Day",
                                     AtomConstructor (M.singleton "Day" ["Int"])))]
                
                
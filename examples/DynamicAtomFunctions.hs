{-# LANGUAGE OverloadedStrings #-}
--compile with `cabal exec ghc -- examples/DynamicAtomFunctions.hs -package project-m36`
--load with `loadatomfunctions "DynamicAtomFunctions" "someAtomFunctions" "examples/DynamicAtomFunctions.o"`

module DynamicAtomFunctions where
import ProjectM36.Base

someAtomFunctions :: [AtomFunction]
someAtomFunctions = [AtomFunction{
                    atomFuncName = "constTrue",
                    atomFuncType = [TypeVariableType "a", BoolAtomType],
                    atomFuncBody = AtomFunctionBody Nothing (\(x:_) -> pure (BoolAtom True))}]

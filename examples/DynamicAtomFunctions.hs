{-# LANGUAGE OverloadedStrings #-}
--compile with `cabal exec ghc -- examples/DynamicAtomFunctions.hs -package project-m36`
-- for persistent databases, copy "DynamicAtomFunctions.o" to "<db dir>/compiled_modules", then
--    load with `loadatomfunctions "DynamicAtomFunctions" "someAtomFunctions" "DynamicAtomFunctions.o"`
-- for transient databases,
--    load with `loadatomfunctions "DynamicAtomFunctions" "someAtomFunctions" "examples/DynamicAtomFunctions.o"`

module DynamicAtomFunctions where
import ProjectM36.Base
import ProjectM36.AtomFunction

someAtomFunctions :: [AtomFunction]
someAtomFunctions = [AtomFunction{
                    atomFuncName = "constTrue",
                    atomFuncType = [TypeVariableType "a", BoolAtomType],
                    atomFuncBody = externalAtomFunction (\(x:_) -> pure (BoolAtom True))}]

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
someAtomFunctions = [Function{
                    funcName = "constTrue",
                    funcType = [TypeVariableType "a", BoolAtomType],
                    funcBody = externalAtomFunction (\(x:_) -> pure (BoolAtom True))}]

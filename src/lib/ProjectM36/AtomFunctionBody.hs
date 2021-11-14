{-# LANGUAGE ScopedTypeVariables #-}
--tools to execute an atom function body
module ProjectM36.AtomFunctionBody where
import ProjectM36.Base

compiledAtomFunctionBody :: AtomFunctionBodyType -> AtomFunctionBody  
compiledAtomFunctionBody = FunctionBuiltInBody

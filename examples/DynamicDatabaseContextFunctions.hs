{-# LANGUAGE OverloadedStrings #-}
--compile with `cabal exec ghc -- examples/DynamicDatabaseContextFunctions.hs -package project-m36`
--load with `loaddatabasecontextfunctions "DynamicDatabaseContextFunctions" "someDBCFunctions" "examples/DynamicDatabaseContextFunctions.o"`
module DynamicDatabaseContextFunctions where
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.DatabaseContextFunctionError
import qualified ProjectM36.Attribute as A

import qualified Data.Map as M


someDBCFunctions :: [DatabaseContextFunction]
someDBCFunctions = [DatabaseContextFunction {
                       dbcFuncName = "addtestrel",
                       dbcFuncType = [],
                       dbcFuncBody = DatabaseContextFunctionBody Nothing addTestRel
                       }]
  where
    addTestRel _ ctx = do
      let attrs = A.attributesFromList [Attribute "word" TextAtomType]
          eRel = mkRelationFromList attrs [[TextAtom "nice"]] 
      case eRel of 
        Left err -> Left (DatabaseContextFunctionUserError (show err))
        Right testRel ->
          pure $ ctx { relationVariables = M.insert "testRel" testRel (relationVariables ctx) }

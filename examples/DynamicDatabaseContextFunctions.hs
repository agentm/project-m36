{-# LANGUAGE OverloadedStrings #-}
--compile with `cabal exec ghc -- examples/DynamicDatabaseContextFunctions.hs -package project-m36`
--load with `loaddatabasecontextfunctions "DynamicDatabaseContextFunctions" "someDBCFunctions" "examples/DynamicDatabaseContextFunctions.o"`
module DynamicDatabaseContextFunctions where
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.DatabaseContextFunction
import qualified ProjectM36.Attribute as A

import qualified Data.Map as M


someDBCFunctions :: [DatabaseContextFunction]
someDBCFunctions = [Function {
                       funcName = "addtestrel",
                       funcType = [],
                       funcBody = externalDatabaseContextFunction addTestRel
                       }]
  where
    addTestRel _ ctx = do
      let attrExprs = [NakedAttributeExpr (Attribute "word" TextAtomType)]
          newRelExpr = MakeRelationFromExprs (Just attrExprs) (TupleExprs UncommittedContextMarker [TupleExpr (M.singleton "word" (NakedAtomExpr (TextAtom "nice")))])
      pure $ ctx { relationVariables =
                       M.insert "testRel" newRelExpr (relationVariables ctx) }      



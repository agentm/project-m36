{-# LANGUAGE TypeApplications #-}
module TutorialD.Interpreter.TH where
import TutorialD.Interpreter.RelationalExpr
import TutorialD.Interpreter.DatabaseContextExpr

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Megaparsec
import qualified Data.Text as T

-- | Return a `RelationalExpr` via template haskell.
relationalExpr :: QuasiQuoter
relationalExpr = QuasiQuoter {quoteExp = parseRelExpr,
                               quotePat = notRelevant,
                               quoteType = notRelevant,
                               quoteDec = notRelevant}
  where       
    parseRelExpr relExprString =
      case parse (relExprP @()) "<th>" (T.pack relExprString)  of
        Left err -> fail (errorBundlePretty err)
        Right expr -> lift expr

notRelevant :: String -> a
notRelevant e = error $ e <> "not relevant for this template"
        

-- | Return a `DatabaseContextExpr` via template haskell.
dbContextExpr :: QuasiQuoter
dbContextExpr = QuasiQuoter {quoteExp = parsedbcExpr,
                              quotePat = notRelevant,
                              quoteType = notRelevant,
                              quoteDec = notRelevant}
  where
    parsedbcExpr dbcExprString =
      case parse databaseContextExprP "<th>" (T.pack dbcExprString) of
        Left err -> fail (errorBundlePretty err)
        Right expr -> lift expr

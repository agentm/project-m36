module SQL.Interpreter.Update where
import SQL.Interpreter.Select
import ProjectM36.SQL.Update
import SQL.Interpreter.Base
import ProjectM36.Interpreter
import Control.Applicative
import Text.Megaparsec

updateP :: Parser Update
updateP = do
  reserved "update"
  tname <- tableNameP
  --mTAlias <- try (reserved "as" *> (Just <$> tableAliasP)) <|> pure Nothing
  reserved "set"
  setCols <- sepByComma1 $ do
    calias <- unqualifiedColumnNameP
    reserved "="
    sexpr <- scalarExprP
    pure (calias, sexpr)
  mWhere <- try (Just <$> whereP) <|> pure Nothing
  pure (Update {
           target = tname,
--           targetAlias = mTAlias,
           setColumns = setCols,
           mRestriction = mWhere
           })

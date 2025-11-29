module TutorialD.Interpreter.LoginRolesOperator where
import ProjectM36.Client as C
import qualified ProjectM36.LoginRoles as LoginRoles
import ProjectM36.Interpreter as I
import qualified Data.Text as T
import Data.Functor

import TutorialD.Interpreter.Base

alterLoginRolesExprP :: Parser AlterLoginRolesExpr
alterLoginRolesExprP =
  showAllRolesExprP <|>
  alterRoleNameExprP <|>
  alterRoleMayLoginExprP <|>
  showRolesForRoleExprP <|>
  addLoginRoleToRoleExprP <|>  
  addLoginRoleExprP <|>
  removeLoginRoleExprP <|>
  removeLoginRoleFromRoleExprP <|>
  addPermissionToRoleExprP <|>
  removePermissionFromRoleExprP
  where
    mayGrantP = do
      (reserved "maygrant" $> True) <|> (reserved "nogrant" $> False)
    mayLoginP = do
      (reserved "maylogin" $> True) <|> (reserved "maynotlogin" $> False)
    showRolesForRoleExprP = do
      colonOp ":showloginrole" 
      ShowRolesForRoleExpr <$> roleNameP
    addLoginRoleExprP = do
      colonOp ":addloginrole"
      AddLoginRoleExpr <$> roleNameP <*> mayLoginP
    removeLoginRoleExprP = do
      colonOp ":removeloginrole"
      RemoveLoginRoleExpr <$> roleNameP
    showAllRolesExprP = do
      colonOp ":showloginroles"
      pure ShowAllRolesExpr
    addLoginRoleToRoleExprP = do
      colonOp ":addloginroletorole"
      AddRoleToRoleExpr <$> roleNameP <*> roleNameP <*> mayGrantP
    removeLoginRoleFromRoleExprP = do
      colonOp ":removeloginrolefromrole"
      RemoveRoleFromRoleExpr <$> roleNameP <*> roleNameP
    addPermissionToRoleExprP = do
      colonOp ":addpermissiontologinrole"
      AddPermissionToRoleExpr <$> permissionP <*> roleNameP <*> mayGrantP
    removePermissionFromRoleExprP = do
      colonOp ":removepermissionfromloginrole"
      RemovePermissionFromRoleExpr <$> roleNameP <*> permissionP
    alterRoleNameExprP = do
      colonOp ":alterrolename"
      AlterLoginRoleExpr <$> roleNameP <*> (Just <$> roleNameP) <*> pure Nothing
    alterRoleMayLoginExprP = do
      colonOp ":alterrolemaylogin"
      AlterLoginRoleExpr <$> roleNameP <*> pure Nothing <*> (Just <$> mayLoginP)

evalAlterLoginRolesExpr :: SessionId -> Connection -> AlterLoginRolesExpr -> IO ConsoleResult
evalAlterLoginRolesExpr sessionId conn expr = do
  result <- C.executeAlterLoginRolesExpr sessionId conn expr
  case result of
    Left err -> pure (DisplayErrorResult (T.pack (show err)))
    Right LoginRoles.QuietSuccessResult -> pure I.QuietSuccessResult
    Right (LoginRoles.InfoResult info) -> pure (DisplayResult info)

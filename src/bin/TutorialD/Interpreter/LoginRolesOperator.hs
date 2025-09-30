module TutorialD.Interpreter.LoginRolesOperator where
import ProjectM36.Client as C
import ProjectM36.LoginRoles as LoginRoles
import ProjectM36.Interpreter
import qualified Data.Text as T

import TutorialD.Interpreter.Base

alterLoginRolesExprP :: Parser AlterLoginRolesExpr
alterLoginRolesExprP =
  showAllRolesExprP <|>  
  showRolesForRoleExprP <|>
  addLoginRoleExprP <|>
  removeLoginRoleExprP <|>
  addLoginRoleToRoleExprP <|>
  removeLoginRoleFromRoleExprP <|>
  addPermissionToRoleExprP <|>
  removePermissionFromRoleExprP
  where
    mayGrantP = do
      (reserved "maygrant" *> pure True) <|> pure False
    showRolesForRoleExprP = do
      colonOp ":showloginrole" 
      ShowRolesForRoleExpr <$> roleNameP
    addLoginRoleExprP = do
      colonOp ":addloginrole"
      AddLoginRoleExpr <$> roleNameP
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

evalAlterLoginRolesExpr :: SessionId -> Connection -> AlterLoginRolesExpr -> IO ConsoleResult
evalAlterLoginRolesExpr sessionId conn expr = do
  result <- C.executeAlterLoginRolesExpr sessionId conn expr
  case result of
    Left err -> pure (DisplayErrorResult (T.pack (show err)))
    Right msg -> pure (DisplayResult msg)

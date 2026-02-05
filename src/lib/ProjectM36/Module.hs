-- | Utility module for importing scripted atom and database context functions.
module ProjectM36.Module where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.AccessControlList
import ProjectM36.DatabaseContext.Types as DBCT
import Control.Monad.Trans.Writer
import Control.Monad.RWS.Strict (RWST, get, put, ask, runRWST)
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Data.Functor.Identity

declareAtomFunction :: FunctionName -> EntryPoints ()
declareAtomFunction nam = tell [DeclareAtomFunction nam]

declareDatabaseContextFunction :: FunctionName -> DBCFunctionAccessControlList -> EntryPoints ()
declareDatabaseContextFunction nam acl' = tell [DeclareDatabaseContextFunction nam acl']

type EntryPoints = Writer [DeclareFunction]

runEntryPoints :: EntryPoints () -> [DeclareFunction]
runEntryPoints = execWriter

data DeclareFunctionBase a = DeclareAtomFunction a |
                             DeclareDatabaseContextFunction a DBCFunctionAccessControlList
  deriving (Show)

type DeclareFunction = DeclareFunctionBase FunctionName

type DatabaseContextFunctionMonad a = RWST DatabaseContextFunctionMonadEnv () DatabaseContext (ExceptT RelationalError Identity) a

newtype DatabaseContextFunctionMonadEnv =
  DatabaseContextFunctionMonadEnv
  {
    utils :: DatabaseContextFunctionUtils
  }

executeRelationalExpr :: RelationalExpr -> DatabaseContextFunctionMonad Relation
executeRelationalExpr expr = do
  env <- ask
  ctx <- get
  case DBCT.executeRelationalExpr (utils env) ctx expr of
    Left err -> throwError err
    Right rel -> pure rel

executeDatabaseContextExpr :: DatabaseContextExpr -> DatabaseContextFunctionMonad ()
executeDatabaseContextExpr expr = do
  env <- ask
  ctx <- get
  case DBCT.executeDatabaseContextExpr (utils env) ctx expr of
    Left err -> throwError err
    Right ctx' ->
      put ctx'

runDatabaseContextFunctionMonad ::
  DatabaseContextFunctionMonadEnv ->
  DatabaseContext ->
  DatabaseContextFunctionMonad () ->
  Either RelationalError DatabaseContext
runDatabaseContextFunctionMonad env ctx m = do
  (_,ctx',_) <- runIdentity $ runExceptT $ runRWST m env ctx
  pure ctx'

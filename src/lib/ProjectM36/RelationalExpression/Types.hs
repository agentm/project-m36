{-# LANGUAGE RankNTypes #-}
module ProjectM36.RelationalExpression.Types where
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.TransactionGraph.Types
import ProjectM36.DatabaseContext.Types
import ProjectM36.ScriptSession
import ProjectM36.AccessControlList

import Data.Functor.Identity
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.Except (ExceptT, runExceptT)
import qualified Data.Map as M

type GraphRefRelationalExprM a = ReaderT GraphRefRelationalExprEnv (ExceptT RelationalError Identity) a

-- | The context is optionally passed down along in cases where the current context is uncommitted.
data GraphRefRelationalExprEnv =
  GraphRefRelationalExprEnv {
  gre_context :: Maybe DatabaseContext,
  gre_graph :: TransactionGraph,
  gre_extra :: Maybe (Either RelationTuple Attributes)
  }

data DatabaseContextEvalState = DatabaseContextEvalState {
  dbc_context :: DatabaseContext,
  dbc_accum :: M.Map ResultAccumName ResultAccum
  }

type NormalizeRelationalExprF = DatabaseContext -> TransactionGraph -> RelationalExprBase (AttributeNamesExprBase ()) () -> Either RelationalError NormalizedRelationalExpr
  
data DatabaseContextEvalEnv = DatabaseContextEvalEnv
  { dce_transId :: TransactionId,
    dce_graph :: TransactionGraph,
    dce_dbcfuncutils :: DatabaseContextFunctionUtils,
    dce_normalizeRelationalExpr :: NormalizeRelationalExprF
  }

--used to eval relationalexpr
type RelationalExprM a = ReaderT RelationalExprEnv (ExceptT RelationalError Identity) a

-- we need to pass around a higher level RelationTuple and Attributes in order to solve #52
data RelationalExprEnv = RelationalExprEnv {
  re_context :: DatabaseContext, 
  re_graph :: TransactionGraph,
  re_extra :: Maybe (Either RelationTuple Attributes)
  }

type ResultAccumName = StringType

type ResultAccumFunc = (RelationTuple -> Relation -> Relation) -> Relation -> Relation

data ResultAccum = ResultAccum { resultAccumFunc :: ResultAccumFunc,
                                 resultAccumResult :: Relation
                                 }

data DatabaseContextIOEvalEnv = DatabaseContextIOEvalEnv
  { dbcio_transId :: TransactionId,
    dbcio_graph :: TransactionGraph,
    dbcio_mScriptSession :: Maybe ScriptSession,
    dbcio_roleId :: RoleId,
    resolveRoleNameACL :: forall x. AccessControlList RoleName x -> IO (Either RelationalError (AccessControlList RoleId x)),
    dbcio_mModulesDirectory :: Maybe FilePath, -- ^ when running in persistent mode, this must be a Just value to a directory containing .o/.so/.dynlib files which the user has placed there for access to compiled functions
    dbcio_dbcfunctionUtils :: DatabaseContextFunctionUtils,
    dbcio_normalizeRelationalExpr :: NormalizeRelationalExprF
  }

freshGraphRefRelationalExprEnv :: Maybe DatabaseContext -> TransactionGraph -> GraphRefRelationalExprEnv
freshGraphRefRelationalExprEnv mctx graph = GraphRefRelationalExprEnv {
  gre_context = mctx,
  gre_graph = graph,
  gre_extra = Nothing
  }

runGraphRefRelationalExprM :: GraphRefRelationalExprEnv -> GraphRefRelationalExprM a -> Either RelationalError a
runGraphRefRelationalExprM env m = runIdentity (runExceptT (runReaderT m env))

gfGraph :: GraphRefRelationalExprM TransactionGraph
gfGraph = asks gre_graph

envContext :: RelationalExprEnv -> DatabaseContext
envContext = re_context

type CheckConstraints =
  NormalizeRelationalExprF -> DatabaseContext -> TransactionId -> TransactionGraph -> Either RelationalError ()  

instance Show RelationalExprEnv where
  show e@RelationalExprEnv{} = "RelationalExprEnv " ++ show (re_extra e)



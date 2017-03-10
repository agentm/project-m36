module ProjectM36.DatabaseContextState where
import ProjectM36.Base
import ProjectM36.Error
import Control.Monad.State

type ConstraintValidator = DatabaseContextExpr -> DatabaseContext -> Maybe RelationalError

data DatabaseContextEvalState = DatabaseContextEvalState {
  dbcontext :: DatabaseContext,
  constraintValidator :: ConstraintValidator
  }
                                
type DatabaseState a = State DatabaseContextEvalState a

getDatabaseContext :: DatabaseState DatabaseContext
getDatabaseContext = liftM dbcontext get

getConstraintValidator :: DatabaseState ConstraintValidator
getConstraintValidator = liftM constraintValidator get

putDatabaseContext :: DatabaseContext -> DatabaseState ()
putDatabaseContext context = do
  dbstate <- get
  put (dbstate {dbcontext = context})

getThunks :: DatabaseState DatabaseContextExpr
getThunks = do 
  context <- getDatabaseContext
  pure (thunks context)
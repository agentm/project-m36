module ProjectM36.DatabaseContextState where
import ProjectM36.Base
import ProjectM36.Error
import Control.Monad.State

type ConstraintValidator = DatabaseContextExpr -> DatabaseContext -> Maybe RelationalError


type ValidationNeeded = Bool

--returns True if validation is needed for this context expression
--requiring validation can remove the possibility to store the update as a thunk
type IncDepValidationNeeded = DatabaseContextExpr -> InclusionDependencies -> Either RelationalError ValidationNeeded

data DatabaseContextEvalState = DatabaseContextEvalState {
  dbcontext :: DatabaseContext,
  constraintValidator :: ConstraintValidator,
  incDepValidationNeeded :: IncDepValidationNeeded,
  allowNewThunks ::
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
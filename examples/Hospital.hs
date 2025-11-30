{-# LANGUAGE DeriveGeneric, DerivingVia, TypeApplications #-}
import ProjectM36.Client
import Data.Typeable
import ProjectM36.Relation
import GHC.Generics
import ProjectM36.Tuple

-- define a custom data type to use in both the client and database
data AgeType = PreciseAge Int |
               ForgotToAsk |
               RefusedToDisclose |
               NotApplicable |
               ApproximateAge Int Int
               deriving (Eq, Show, Generic)

-- define an automatically-derived Atomable instance
instance Atomable AgeType

failFast :: Show a => IO (Either a b) -> IO b
failFast m = do
  ret <- m
  case ret of
    Left err -> error (show err)
    Right val -> pure val

ageAtomType :: AtomType
ageAtomType = toAtomType (Proxy :: Proxy AgeType)
               
main :: IO ()
main = do
  let bob_relation_attrs = attributesFromList [Attribute "name" TextAtomType,
                                               Attribute "age" ageAtomType]
      relvar_name = "hospital_patient"
      age_value_in = ApproximateAge 30 40 
      mk_bob_relation = mkRelationFromList 
                     bob_relation_attrs 
                     [[TextAtom "Bob",
                       toAtom age_value_in]]

  -- create the database
  conn <- failFast $ connectProjectM36 (InProcessConnectionInfo NoPersistence emptyNotificationCallback [] basicDatabaseContext "admin")

  -- create the session at the head of master branch
  sessionId <- failFast $ createSessionAtHead conn "master"

  -- create the data type in the database
  failFast $ executeDatabaseContextExpr sessionId conn (toAddTypeExpr (Proxy :: Proxy AgeType))
  
  -- create the patient data in a relation
  bob_relation <- failFast (pure mk_bob_relation)

  -- save the data to the database
  failFast $ executeDatabaseContextExpr sessionId conn (Assign relvar_name (ExistingRelation bob_relation))

  -- retrieve the data from the database
  result <- failFast $ executeRelationalExpr sessionId conn (RelationVariable relvar_name ())

  -- print the data
  case singletonTuple result of
    Nothing -> error "not a singleton relation!"
    Just tuple -> do
      case atomForAttributeName "age" tuple of
        Left err -> error (show err)
        Right age_value_out -> do
          print age_value_in
          print (fromAtom @AgeType age_value_out)
      
            

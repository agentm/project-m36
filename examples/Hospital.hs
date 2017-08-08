{-# LANGUAGE DeriveAnyClass,DeriveGeneric #-}
module Hospital where

import ProjectM36.Client
import ProjectM36.Atom
import Data.Typeable
import ProjectM36.Relation
import Data.Binary
import Control.DeepSeq
import Data.Text
import GHC.Generics
import Data.Hashable
import ProjectM36.Tuple

data AgeType = PreciseAge Int |
               ForgotToAsk |
               RefusedToDisclose |
               NotApplicable |
               ApproximateAge Int Int
             deriving (Eq,Show,Read,Hashable,Binary,Typeable,NFData,Generic)
                      
instance Atomable AgeType

failFastMaybe :: (Show a) => Maybe a -> IO ()
failFastMaybe (Just err) = error (show err)
failFastMaybe Nothing = return ()

failFastEither :: Show a => Either a b -> IO b
failFastEither (Left err) = error (show err)
failFastEither (Right val) = return val

ageAtomType :: AtomType
ageAtomType = atomTypeForProxy (Proxy :: Proxy AgeType)
               
runExample :: IO ()
runExample = do
  let bob_relation_attrs = attributesFromList [Attribute "name" stringAtomType,
                                               Attribute "age" ageAtomType]
      relvar_name = "hospital_patient"
      age_value_in = ApproximateAge 30 40 
      bob_relation_err = mkRelationFromList 
                     bob_relation_attrs 
                     [[Atom ("Bob"::Text), Atom age_value_in]]

  connerr <- connectProjectM36 (InProcessConnectionInfo NoPersistence)
  conn <- failFastEither connerr
  bob_relation <- failFastEither bob_relation_err
  merr <- executeDatabaseContextExpr conn (Assign relvar_name (ExistingRelation bob_relation))
  failFastMaybe merr
  result_err <- executeRelationalExpr conn (RelationVariable relvar_name)
  result <- failFastEither result_err
  case singletonTuple result of
    Nothing -> error "not a singleton relation!"
    Just tuple -> do
      (Atom age_value_out) <- failFastEither $ atomForAttributeName "age" tuple
      print age_value_in
      print age_value_out
      case cast age_value_out of
        Nothing -> error "wrong datatype"
        Just age_value_out' -> 
          print (age_value_in == age_value_out')
      
            

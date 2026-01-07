{-# LANGUAGE DeriveGeneric, DerivingVia, DeriveAnyClass, OverloadedStrings #-}
import ProjectM36.Client
import ProjectM36.Relation
import ProjectM36.Tupleable
import ProjectM36.TupleSet
import ProjectM36.Relation.Show.Term

import Data.Typeable
import GHC.Generics
import System.Random (initStdGen)
import Data.Time.Calendar
import qualified Data.Text.IO as T

data Ticket = Ticket
  { ticketId   :: Integer
  , visitorAge :: Integer    -- years
  , basePrice  :: Integer  -- base price before adjustments
  , visitDate  :: Day
}
 deriving (Generic, Tupleable)


main :: IO ()
main = do
  let ticketAttrs = toAttributes (Proxy :: Proxy Ticket)
      ticket1 = Ticket { ticketId = 1,
                         visitorAge = 8,
                         basePrice = 20,
                         visitDate = fromGregorian 2025 10 01 }
      ticket2 = Ticket { ticketId = 2,
                         visitorAge = 25,
                         basePrice = 20,
                         visitDate = fromGregorian 2025 12 25 }
      Right ticketTupleSet = mkTupleSet ticketAttrs [toTuple ticket1,
                                          toTuple ticket2]
      Right ticketRel = mkRelation ticketAttrs ticketTupleSet
  rando <- initStdGen
  -- connect to the database
  conn <- failFast $ connectProjectM36 (InProcessConnectionInfo NoPersistence emptyNotificationCallback [] basicDatabaseContext rando "admin")

  -- create a session on the master branch
  sessionId <- failFast $ createSessionAtHead conn "master"

  -- add the ticket discount function
  let func_type = [integerTypeCons, integerTypeCons, ADTypeConstructor "Either" [ADTypeConstructor "AtomFunctionError" [], integerTypeCons]]
      integerTypeCons = PrimitiveTypeConstructor "Integer" IntegerAtomType
      func_body = "(\\[IntegerAtom age,IntegerAtom price] -> pure (IntegerAtom (if age < 10 then price `div` 2 else price))) :: [Atom] -> Either AtomFunctionError Atom"
  failFast $ executeDatabaseContextIOExpr sessionId conn (AddAtomFunction "apply_discount" func_type func_body)

  -- calculate the proper discount per ticket and add it to the database
  let discountedTicketRel = Extend (AttributeExtendTupleExpr "discounted_price" func_apply_discount) (ExistingRelation ticketRel)
      func_apply_discount = FunctionAtomExpr "apply_discount" [AttributeAtomExpr "visitorAge", AttributeAtomExpr "basePrice"] ()
  failFast $ executeDatabaseContextExpr sessionId conn (Assign "ticket_sales" discountedTicketRel)

  -- print out the resultant relation
  Right ticketSalesRelation <- executeRelationalExpr sessionId conn (RelationVariable "ticket_sales" ())
  T.putStrLn $ showRelation ticketSalesRelation

 


-- | Apply a 50% discount for kids under 10 years old. Arguments: age, base price
applyDiscount :: Integer -> Integer -> Integer
applyDiscount age base_price =
  if age < 10 then base_price `div` 2 else base_price

failFast :: Show a => IO (Either a b) -> IO b
failFast m = do
  ret <- m
  case ret of
    Left err -> error (show err)
    Right val -> pure val

  

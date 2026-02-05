-- imported by the Haskell interpreter for the Module.hs test
module TestModule where
import ProjectM36.Module
import ProjectM36.AccessControlList
import ProjectM36.Base
import qualified Data.Map as M
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.ByteString (ByteString)
import Data.UUID (UUID)
import Data.Scientific (Scientific)


projectM36Functions :: EntryPoints ()
projectM36Functions = do
  declareAtomFunction "multiTypesAtomFunc"  
  declareAtomFunction "applyDiscount" 
  declareDatabaseContextFunction "addSale" (allPermissionsForRoleId adminRoleId)
  declareDatabaseContextFunction "multiTypesDBCFunc" (allPermissionsForRoleId adminRoleId)  

applyDiscount :: Integer -> Integer -> Integer
applyDiscount age price =
  if age <= 10 then
    price `div` 2
    else
    price

addSale :: Integer -> Integer -> Integer -> Day -> DatabaseContextFunctionMonad ()
addSale ticketId age price purchaseDay = do
  let tuples = [TupleExpr (M.fromList [("ticketId", i ticketId),
                                       ("visitorAge", i age),
                                       ("basePrice", FunctionAtomExpr "applyDiscount" [i age, i price] ()),
                                       ("visitDate", NakedAtomExpr (DayAtom purchaseDay))])]
      i = NakedAtomExpr . IntegerAtom
  executeDatabaseContextExpr (Insert "ticket_sales" (MakeRelationFromExprs Nothing (TupleExprs () tuples)))

-- | A throwaway atom function just to test that we can use various types.
multiTypesAtomFunc ::
  Scientific ->
  UTCTime ->
  ByteString ->
  UUID ->
  Day ->
  Text ->
  Bool ->
  Int ->
  Double ->
  Integer
multiTypesAtomFunc a b c d e f g h i = 3

multiTypesDBCFunc ::
    Scientific ->
  UTCTime ->
  ByteString ->
  UUID ->
  Day ->
  Text ->
  Bool ->
  Int ->
  Double ->
  DatabaseContextFunctionMonad ()
multiTypesDBCFunc a b c d e f g h i = pure ()

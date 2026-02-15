{-# LANGUAGE OverloadedStrings #-}
-- | A variant of the zoo module example to test compilation.
--module Zoo where
import ProjectM36.Module
import ProjectM36.AccessControlList
import Data.Time.Calendar
import ProjectM36.Base
import qualified Data.Map as M

apply_discount :: Integer -> Integer -> Integer
apply_discount age price =
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


projectM36Functions :: EntryPoints ()
projectM36Functions = do
  declareAtomFunction "apply_discount"
  declareDatabaseContextFunction "addSale" (permissionForRole ExecuteDBCFunctionPermission "ticket_seller" <> allPermissionsForRole "admin")

main :: IO ()
main = pure ()

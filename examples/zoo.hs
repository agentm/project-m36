{-# LANGUAGE OverloadedStrings #-}
-- | A variant of the zoo module example to test compilation.
--module Zoo where
import ProjectM36.Module
import ProjectM36.AccessControlList
import Data.Time.Calendar
import ProjectM36.Base
import qualified Data.Map as M

{- Setup
:addloginrole ticket_seller maylogin
ticket_sales := relation{ticketId Integer, visitorAge Integer, price Integer, visitDate Day}
-}

type Age = Integer
type Price = Integer

applyDiscount :: Age -> Price -> Price
applyDiscount age price =
  if age <= 10 then
    price `div` 2
    else
    price

{-
applyDiscount :: Age -> Price -> Day -> Integer
applyDiscount age price day =
  if age <= 10 && not isNewYearsDay then
    price `div` 2
    else
    price
 where
  isNewYearsDay =
    case toGregorian day of
      (_, m, d) -> m == 1 && d == 1
-}

addSale :: Integer -> Integer -> Integer -> Day -> DatabaseContextFunctionMonad ()
addSale ticketId age price purchaseDay = do
  let tuples = [TupleExpr (M.fromList [("ticketId", i ticketId),
                                       ("visitorAge", i age),
                                       ("price", FunctionAtomExpr "applyDiscount" [i age, i price] ()),
                                       ("visitDate", NakedAtomExpr (DayAtom purchaseDay))])]
      i = NakedAtomExpr . IntegerAtom
  executeDatabaseContextExpr (Insert "ticket_sales" (MakeRelationFromExprs Nothing (TupleExprs () tuples)))


projectM36Functions :: EntryPoints ()
projectM36Functions = do
  declareAtomFunction "applyDiscount"
--  declareDatabaseContextFunction "addSale" (permissionForRole ExecuteDBCFunctionPermission "ticket_seller" <> allPermissionsForRole "admin")

main :: IO ()
main = pure ()

-- imported by the Haskell interpreter for the Module.hs test
module TestFuncs (projectM36Functions, apply_discount) where
import ProjectM36.Module
import ProjectM36.Base
import Data.Time.Calendar
import qualified Data.Map as M

projectM36Functions :: EntryPoints ()
projectM36Functions = do
  declareAtomFunction "apply_discount"
  declareDatabaseContextFunction "add_sale"

apply_discount :: Integer -> Integer -> Integer
apply_discount age price =
  if age <= 10 then
    price `div` 2
    else
    price

add_sale :: Integer -> Integer -> Integer -> {- Day -> -} DatabaseContextFunctionMonad ()
add_sale utils args databaseContext = do
  let tuples = [TupleExpr (M.fromList [("ticketId", i 3),
                                       ("visitorAge", i 9),
                                       ("basePrice", FunctionAtomExpr "apply_discount" [i 9, i 20] ()),
                                       ("visitDate", NakedAtomExpr (DayAtom (fromGregorian 2025 10 03)))])]
      i = NakedAtomExpr . IntegerAtom
  executeDatabaseContextExpr (Insert "ticket_sales" (MakeRelationFromExprs Nothing (TupleExprs () tuples)))
                                                                                                                          

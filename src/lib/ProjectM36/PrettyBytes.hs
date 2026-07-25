module ProjectM36.PrettyBytes where
import Data.Int

units :: [String]
units = ["B","KB","MB","GB","TB","PB","EB","ZB","YB"]

prettyBytes' :: [String] -> Int64 -> String
prettyBytes' remainingUnits n =
  case remainingUnits of
    [] -> show n
    (nextUnit:moreUnits) ->
      if n < 1000 then
        show n <> nextUnit
      else
        prettyBytes' moreUnits (n `div` 1000)

prettyBytes :: Int64 -> String
prettyBytes = prettyBytes' units

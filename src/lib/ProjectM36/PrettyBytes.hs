module ProjectM36.PrettyBytes where
import Data.Int

units :: [String]
units = ["B","KB","MB","GB","TB","PB","EB","ZB","YB"]

prettyBytes' :: [String] -> Int64 -> String
prettyBytes' remainingUnits n | null remainingUnits = show n
                              | n < 1000 = show n <> head remainingUnits
                              | otherwise = prettyBytes' (tail remainingUnits) (n `div` 1000)

prettyBytes :: Int64 -> String
prettyBytes = prettyBytes' units

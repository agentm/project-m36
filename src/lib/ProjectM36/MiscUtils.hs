module ProjectM36.MiscUtils where

--returns duplicates of a pre-sorted list
dupes :: Eq a => [a] -> [a]    
dupes [] = []
dupes (_:[]) = []
dupes (x:y:[]) = if x == y then [x] else []
dupes (x:y:xs) = dupes(x:[y]) ++ dupes(xs)

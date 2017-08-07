module ProjectM36.MiscUtils where

--returns duplicates of a pre-sorted list
dupes :: Eq a => [a] -> [a]    
dupes [] = []
dupes [_] = []
dupes [x,y] = [x | x == y]
dupes (x:y:xs) = dupes(x:[y]) ++ dupes(y : xs)


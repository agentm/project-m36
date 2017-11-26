module ProjectM36.MiscUtils where

--returns duplicates of a pre-sorted list
dupes :: Eq a => [a] -> [a]    
dupes [] = []
dupes [_] = []
dupes [x,y] = [x | x == y]
dupes (x:y:xs) = dupes(x:[y]) ++ dupes(y : xs)

--Data.Vector.indexed but for lists
indexed :: [a] -> [(Int, a)]
indexed = go 0
  where
    go _ [] = []
    go i (v:ys) = (i,v):go (i+1) ys

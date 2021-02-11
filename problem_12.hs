factorNums :: Int -> Int
factorNums n = length [x | x <- [1..n], n `mod` x == 0]

triangle :: Int -> Int
triangle n = sum [x | x <- [1..n]]

solve :: Int
solve = head [triangle x | x <- [1..1000000], factorNums (triangle x) == 500]

data Direction = North | East | South | West
                deriving Show




primefact :: Int -> [Int]
primefact x = [a | a <- [1..x], mod x a == 0, prime a]

divides :: Int -> Int -> Bool
divides x y = y `mod` x == 0

prime :: Int -> Bool
prime n = n > 1 &&  and [not(divides x n) | x <- [2..(n-1)]]

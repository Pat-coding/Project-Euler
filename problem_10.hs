import System.IO

divides :: Int -> Int -> Bool
divides x y = y `mod` x == 0

prime :: Int -> Bool
prime n = n > 1 && and [not(divides x n) | x <- [2..(n-1)]]

allprimes :: Int
allprimes = sum [x | x <- [1..2000000], prime x]

factors :: Integer -> [Integer]
factors n = [x | x <- [1..n], mod n x == 0]

--primes :: Int -> Bool
--primes x = x > 1 && and factors == [1, x]

--primesBetween :: Int -> Int -> [Int]
--primesBetween x y = [z | z <- [x..y], primes z]

counts :: Int -> [String] -> Int
counts n xs = sum $ filter (<n) $ map length xs

countz :: Int -> [String] -> Int
countz _ [] = 0
countz n (x:xs)
    | length x < n = length x + countz n xs
    | otherwise = countz n xs

--([char], [Bool, Bool])
middle :: (a, b, c) -> b
middle (x,y,z) = y
palindrome :: (Eq a) => [a] -> Bool
palindrome xs = xs == reverse xs
iszero :: (Eq a, Num a) => a -> Bool
iszero n = n == 0

getNumber :: String -> [(Int, String)] -> [Int]
getNumber _ [] = []
getNumber s (x:xs) = if snd x == s then fst x:getNumber s xs else getNumber s xs 

remove :: [(Int, String)] -> [String] -> [(Int, String)]
remove [] _ = []
remove (x:db) xs
    |  elem (snd x) xs = remove db xs
    | otherwise = x:remove db xs

main :: IO()
main = do {
            putStr "Please enter a number: ";
            x <- getLine;
            if read x > 100
                then putStrLn ("The number is large enough: " ++ x)
                else 
                    (do { putStrLn "Try again with a larger number";
                    main});
}

printStars :: Int -> IO()
printStars 0 = return ()
printStars n = do {
                    putStrLn (printStarz n);
                    printStars (n - 1);
                }

printStarz :: Int -> String
printStarz 0 = ""
printStarz n = '*':printStarz (n - 1)
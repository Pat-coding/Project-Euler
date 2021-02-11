fib :: [Int]
fib = 0:1:[(fib !! x) + (fib !! (x-1)) | x <- [1..]]
 
fibby :: Int 
fibby = sum $ filter even $ takeWhile (<4000000) fib

 
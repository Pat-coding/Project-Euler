list :: [Int]
list = [x * y| x <- [900..999], y <- [900..999]]

pal :: [Int] -> [Int]
pal [] = []
pal xs = if show (last xs) == reverse (show (last xs)) then [last xs] else pal (init xs)
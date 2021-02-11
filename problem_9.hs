triplets :: [(Float, Float, Float)]
triplets = [(a, b, c) | a <- [1..1000], b <- [1..a], c <- [1..sqrt (a^2 + b^2)], a + b + c == 1000, a^2 + b^2 == c^2]


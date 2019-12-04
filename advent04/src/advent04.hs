
main :: IO ()
main = do 
        print part1
        print part2 

lowerLimit = 134792
upperLimit = 675810


part1 = length $ filter inRange $ filter adjacentSame candidates
part2 = length $ filter isolatedAdjacentSame $ filter inRange $ filter adjacentSame candidates

inRange digits = n >= lowerLimit && n <= upperLimit
    where n = numify digits

numify :: (Int, Int, Int, Int, Int, Int) -> Int
numify (d1, d2, d3, d4, d5, d6) = 
    d1 * 10^5 + d2 * 10^4 + d3 * 10^3 + d4 * 10^2 + d5 * 10 + d6

adjacentSame (d1, d2, d3, d4, d5, d6) = 
    d1 == d2 || d2 == d3 || d3 == d4 || d4 == d5 || d5 == d6

isolatedAdjacentSame (d1, d2, d3, d4, d5, d6) = 
                   (d1 == d2 && d2 /= d3)
    || (d1 /= d2 && d2 == d3 && d3 /= d4)
    || (d2 /= d3 && d3 == d4 && d4 /= d5)
    || (d3 /= d4 && d4 == d5 && d5 /= d6)
    || (d4 /= d5 && d5 == d6)


candidates = [ (d1, d2, d3, d4, d5, d6)
             | d1 <- [1..6]
             , d2 <- [d1..9]
             , d3 <- [d2..9]
             , d4 <- [d3..9]
             , d5 <- [d4..9]
             , d6 <- [d5..9]
             ]
         
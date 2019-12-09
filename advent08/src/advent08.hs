import Data.List
import Data.List.Split
import Data.Char
import Data.Ord
import Data.Function

main :: IO ()
main = do 
        text <- readFile "data/advent08.txt"
        let digits = successfulParse text
        let layers = chunksOf (imageWidth * imageHeight) digits
        print $ part1 layers
        putStrLn $ part2 layers


imageWidth = 25
imageHeight = 6

part1 layers = (count 1 target) * (count 2 target)
    where target = minimumBy (comparing (count 0)) layers

part2 layers = unlines rows
    where pixelLayers = transpose layers
          pixels = map firstVisible pixelLayers
          dPixels = map showPixel pixels
          pixelRows = chunksOf imageWidth dPixels
          rows = map concat pixelRows


firstVisible = head . dropWhile (== 2)

showPixel 0 = " "
showPixel 1 = "\x2588"


count n = length . filter (== n)

-- Count the number of times a predicate is true
-- (Taken from GHC API utility functions)

-- count :: (a -> Bool) -> [a] -> Int
-- count p = go 0
--   where go !n [] = n
--         go !n (x:xs) | p x       = go (n+1) xs
--                      | otherwise = go n xs


successfulParse :: String -> [Int]
successfulParse input = map digitToInt input
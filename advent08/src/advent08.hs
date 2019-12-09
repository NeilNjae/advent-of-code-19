import Data.List
import Data.List.Split
import Data.Char
import Data.Ord

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
          image = concatMap showPixel pixels
          rows = chunksOf imageWidth image

firstVisible = head . dropWhile (== 2)

showPixel 0 = " "
showPixel 1 = "\x2588"

count n = length . filter (== n)

successfulParse :: String -> [Int]
successfulParse input = map digitToInt input
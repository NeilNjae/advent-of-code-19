import Debug.Trace

import Data.Char (digitToInt, intToDigit)

-- import qualified Data.Map.Strict as M
-- import Data.Map.Strict ((!))
import Data.List
-- import qualified Data.Set as S



main :: IO ()
main = do 
        text <- readFile "data/advent16.txt"
        let digits = parseDigits text
        -- print mem
        print $ part1 digits
        print $ part2 digits
        -- print $ part2 mem

part1 :: [Int] -> String
part1 digits = map intToDigit $ take 8 $ (fft digits)!!100

part2 digits = map intToDigit signal
    where offset = read @Int $ map intToDigit $ take 7 digits 
          fullDigits = concat $ replicate 10000 digits
          suffix = drop offset fullDigits
          signal = take 8 $ (fastFft suffix)!!100

part2naive :: [Int] -> String
part2naive digits = map intToDigit signal
    where fullDigits = concat $ replicate 10000 digits
          result = (fft fullDigits)!!100
          offset = read @Int $ map intToDigit $ take 7 digits 
          signal = take 8 $ drop offset result


basePattern :: [Int]
basePattern = [0, 1, 0, -1]

patternOf :: Int -> [Int]
patternOf index = drop 1 $ cycle $ concatMap (replicate index) basePattern

elementAt :: [Int] -> Int -> Int
elementAt digits index = (abs $ sum $ zipWith (*) digits $ patternOf index) `mod` 10

fftStep :: [Int] -> [Int]
fftStep digits = map (elementAt digits) [1..(length digits)]

fft :: [Int] -> [[Int]]
fft = iterate fftStep 

fastFft :: [Int] -> [[Int]]
fastFft = iterate fastFftStep 

fastFftStep :: [Int] -> [Int]
-- fastFftStep digits = map (\ds -> (sum ds) `mod` 10) $ tails digits
fastFftStep digits = scanr (\d s -> (d + s) `mod` 10) 0 digits


parseDigits :: String -> [Int]
parseDigits = map digitToInt




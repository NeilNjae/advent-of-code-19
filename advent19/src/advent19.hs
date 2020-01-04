import Debug.Trace

import Intcode

import qualified Data.Text.IO as TIO

import qualified Data.Map.Strict as M
-- import Data.Map.Strict ((!))
import Data.List

type Bounds = (Integer, Integer) -- upper, lower

type Beam = M.Map Integer Bounds


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent19.txt"
        let mem = parseMachineMemory text
        let machine = makeMachine mem
        print $ part1 machine
        print $ part2 machine


maxY = 50 :: Integer
xRange = [0..49] :: [Integer]
boxSize = 100 :: Integer


part1 machine = sum $ map cellsInRange $ M.elems beamPresence
    where beamPresence = foldl' (traceBeam machine) M.empty xRange -- [0..49] @[Integer]


part2 machine = score $ head $ dropWhile (not . containsBox) corners
    where uppers = scanl' (traceUpper machine) 0 xs
          lowers = scanl' (traceLower machine) (0, 0) xs
          corners = zip (drop ((fromIntegral boxSize) - 1) uppers) lowers
          xs = [0..] :: [Integer]

cellsInRange :: Bounds -> Integer
cellsInRange (u, l) = l' - u'
    where u' = min u maxY
          l' = min l maxY

containsBox (yt, (_xb, yb)) = yt + boxSize - 1 <= yb

score (yt, (xb, _yb)) = xb * 10000 + yt


traceBeam :: Machine -> Beam -> Integer -> Beam
-- traceBeam _machine beam x | trace ((show x) ++ " " ++ (show beam)) False = undefined
traceBeam machine beam x = M.insert x (u', l') beam
    where (prevU, _prevL) = M.findWithDefault (0, 0) (x - 1) beam
          (bic, _foundU) = beamInColumn machine x
          u = head $ dropWhile (\y -> not $ tractorBeamAt machine x y) [prevU..]
          l = head $ dropWhile (\y -> tractorBeamAt machine x y) [u..]
          (u', l') = if prevU == 0 && bic == False
                       then (0, 0)
                       else (u, l)

traceUpper :: Machine -> Integer -> Integer -> Integer
traceUpper machine prev x = u'
    where (bic, _foundU) = beamInColumn machine x
          u = head $ dropWhile (\y -> not $ tractorBeamAt machine x y) [prev..]
          u' = if prev == 0 && bic == False
               then 0
               else u

traceLower :: Machine -> (Integer, Integer) -> Integer -> (Integer, Integer)
traceLower machine (_, prev) x = (x, l')
    where (bic, foundU) = beamInColumn machine x
          startL = if prev == 0 then foundU else prev
          l = head $ dropWhile (\y -> tractorBeamAt machine x y) [startL..]
          l' = if prev == 0 && bic == False
               then 0
               else l - 1

tractorBeamAt :: Machine -> Integer -> Integer -> Bool
tractorBeamAt machine x y = (head output) == 1
    where (_, _, output) = runMachine [x, y] machine


beamInColumn :: Machine -> Integer -> (Bool, Integer)
beamInColumn machine x
    | null fromTop = (False, 0)
    | otherwise = (True, head fromTop)
    where fromTop = dropWhile (\y -> not $ tractorBeamAt machine x y) [0..maxY]


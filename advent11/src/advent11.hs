import Debug.Trace

import Intcode

import qualified Data.Text.IO as TIO

import qualified Data.Map.Strict as M
-- import Data.Map.Strict ((!))
-- import Data.List
-- import Data.Function (on)

type Position = (Int, Int) -- x, y
data Colour = Black | White deriving (Show, Eq, Ord)
data Direction = North | East | South | West deriving (Show, Eq, Ord, Enum, Bounded)

data Ant = Ant 
    { _machine :: Machine
    , _executionState :: ExecutionState
    , _currentInput :: [Integer]
    , _machineOutput :: [Integer]
    , _currentPosition :: Position
    , _currentDirection :: Direction
    } deriving (Show, Eq)

type Hull = M.Map Position Colour


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent11.txt"
        let mem = parseMachineMemory text
        -- print mem
        print $ part1 mem
        putStrLn $ part2 mem


part1 mem = M.size hull
    where ant = encapsulate mem []
          hull = runAnt ant M.empty

part2 mem = showHull hull
    where ant = encapsulate mem []
          hull = runAnt ant (M.singleton (0, 0) White)

encapsulate :: [Integer] -> [Integer] -> Ant
encapsulate mem input = Ant 
    { _machine = makeMachine mem
    , _executionState = Runnable
    , _machineOutput = []
    , _currentInput = input
    , _currentPosition = (0, 0)
    , _currentDirection = North
    }


runAnt :: Ant -> Hull -> Hull
-- runAnt ant hull | trace (show ant ++ " -> " ++ (show (runAntStep ant)) ++ " -- " ++ show hull) False = undefined
runAnt ant hull = hull''
    where ant' = runAntStep ant
          output = _machineOutput ant'
          hull' = if (null output) then hull else paint hull ant' (output!!0)
          ant'' = if (null output) then ant' else move ant' (output!!1)
          ant''' = camera ant'' hull
          hull'' =  if (_executionState ant' == Terminated)
                    then hull'
                    else runAnt ant''' hull'


paint :: Hull -> Ant -> Integer -> Hull
paint hull ant 0 = M.insert (_currentPosition ant) Black hull
paint hull ant 1 = M.insert (_currentPosition ant) White hull

move :: Ant -> Integer -> Ant
move ant angle = ant { _currentDirection = direction', _currentPosition = position' }
    where direction' = turn (_currentDirection ant) angle
          delta = directionDelta direction'
          position' = sumPos delta $ _currentPosition ant

camera :: Ant -> Hull -> Ant
camera ant hull = ant { _currentInput = input' }
    where colour = M.findWithDefault Black (_currentPosition ant) hull
          input = _currentInput ant
          input' = input ++ [colourNum colour]

colourNum :: Colour -> Integer
colourNum Black = 0
colourNum White = 1

turn :: Direction -> Integer -> Direction
turn direction 0 = predW direction
turn direction 1 = succW direction

directionDelta :: Direction -> Position
directionDelta North = ( 0 ,  1)
directionDelta East  = ( 1 ,  0)
directionDelta South = ( 0 , -1)
directionDelta West  = (-1 ,  0)

sumPos :: Position -> Position -> Position
sumPos (a, b) (c, d) = (a + c, b + d)


runAntStep :: Ant -> Ant
runAntStep a = a { _machine = machine'
                 , _executionState = halted
                 , _machineOutput = output
                 }
    where   machine = _machine a
            input = _currentInput a
            (halted, machine', output) = runMachine input machine


showHull :: Hull -> String
showHull hull = unlines rows
    where   minX = minimum $ map fst $ M.keys hull
            minY = minimum $ map snd $ M.keys hull
            maxX = maximum $ map fst $ M.keys hull
            maxY = maximum $ map snd $ M.keys hull
            rows = [showHullRow hull minX maxX y | y <- reverse [minY..maxY]]

showHullRow :: Hull -> Int -> Int -> Int -> String
showHullRow hull minX maxX y = [showHullCell hull x y | x <- [minX..maxX]] 

showHullCell :: Hull -> Int -> Int -> Char
showHullCell hull x y
    | colour == White = '\x2588'
    | colour == Black = ' '
    where colour = M.findWithDefault Black (x, y) hull


-- | a `succ` that wraps 
succW :: (Bounded a, Enum a, Eq a) => a -> a 
succW dir | dir == maxBound = minBound
          | otherwise = succ dir

-- | a `pred` that wraps
predW :: (Bounded a, Enum a, Eq a) => a -> a
predW dir | dir == minBound = maxBound
          | otherwise = pred dir


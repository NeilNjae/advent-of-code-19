import Debug.Trace

import Intcode

import qualified Data.Text.IO as TIO

import qualified Data.Map.Strict as M
-- import Data.Map.Strict ((!))
import Data.List
import Data.List.Split
-- import Data.List
-- import Data.Function (on)

type Position = (Integer, Integer) -- x, y
data Cell  = Empty | Wall | Block | Paddle | Ball deriving (Show, Eq, Ord)
-- data Direction = North | East | South | West deriving (Show, Eq, Ord, Enum, Bounded)

type Field = M.Map Position Cell

data Game = Game 
    { _machine :: Machine
    , _executionState :: ExecutionState
    , _currentInput :: [Integer]
    , _machineOutput :: [Integer]
    , _currentScore :: Integer
    , _paddleX :: Integer
    , _ballX :: Integer
    } deriving (Eq)

instance Show Game where
  show g = "Game {<m>, _executionState = " ++ show (_executionState g) ++
           ", _currentInput = " ++ show (_currentInput g) ++
           ", _machineOutput = " ++ show (_machineOutput g) ++
           ", _currentScore = " ++ show (_currentScore g) ++
           ", _paddleX = " ++ show (_paddleX g) ++ 
           ", _ballX = " ++ show (_ballX g) ++ 
           " }"


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent13.txt"
        let mem = parseMachineMemory text
        -- print mem
        print $ part1 mem
        print $ part2 mem


part1 mem = M.size $ M.filter (== Block) screen
    where (_halted, _machine, output) = runProgram [] mem
          (screen, _score) = buildScreen output

part2 mem = _currentScore game
    where mem' = [2] ++ (tail mem)
          game0 = buildGame mem'
          game = runGame game0


buildScreen :: [Integer] -> (Field, Integer)
buildScreen output = foldl' addCell (M.empty, 0) $ chunksOf 3 output

addCell :: (Field, Integer) -> [Integer] -> (Field, Integer)
addCell (screen, _s) [- 1 , 0, s] = (screen, s)
addCell (screen, score) [x, y, c] = (M.insert (x, y) (cellOf c) screen, score)

cellOf :: Integer -> Cell
cellOf 0 = Empty
cellOf 1 = Wall
cellOf 2 = Block
cellOf 3 = Paddle
cellOf 4 = Ball


buildGame mem = Game 
    { _machine = makeMachine mem
    , _executionState = Runnable
    , _currentInput = []
    , _machineOutput = []
    , _currentScore = 0
    , _paddleX = 0
    , _ballX = 0
    }

runGame :: Game -> Game
-- runGame game | trace (show (_currentScore game) ++ " " ++ show (_executionState game)) False = undefined
runGame game0 = game
    where game1 = runGameStep game0
          game = if (_executionState game1 == Terminated)
                 then game1
                 else runGame game1

runGameStep :: Game -> Game
-- runGameStep game | trace (show (_currentScore game) ++ " " ++ show (_executionState game)) False = undefined
runGameStep game0 = game
    where game1 = runGameMachine game0
          output = _machineOutput game1
          (screen, score) = buildScreen output
          cs = _currentScore game0
          score' = if score > cs then score else cs
          game2 = game1 { _currentScore = score' }
          game = joystick game2 screen


runGameMachine :: Game -> Game
runGameMachine g = g { _machine = machine'
                     , _executionState = halted
                     , _machineOutput = output
                     }
    where   machine = _machine g
            input = _currentInput g
            (halted, machine', output) = runMachine input machine

joystick :: Game -> Field -> Game
joystick game screen = game {_currentInput = ci ++ [direction], 
                              _paddleX = px, _ballX = bx,
                              _executionState = termination}
  where knownBall = M.filter (== Ball) screen
        bx = if M.null knownBall
             then _ballX game
             else fst $ fst $ M.findMin knownBall
        knownPaddle = M.filter (== Paddle) screen
        px = if M.null knownPaddle
             then _paddleX game
             else fst $ fst $ M.findMin knownPaddle
        termination = if _executionState game == Blocked
                      then Runnable
                      else _executionState game
        ci = _currentInput game
        direction = if bx > px 
                    then 1
                    else if bx < px
                         then -1
                         else 0


ghcisetup text = game0
    where mem = parseMachineMemory text
          mem' = [2] ++ (tail mem)
          game0 = buildGame mem'


showScreen :: Field -> String
showScreen screen = unlines rows
    where   minX = minimum $ map fst $ M.keys screen
            minY = minimum $ map snd $ M.keys screen
            maxX = maximum $ map fst $ M.keys screen
            maxY = maximum $ map snd $ M.keys screen
            rows = [showScreenRow screen minX maxX y | y <- [minY..maxY]]

showScreenRow :: Field -> Integer -> Integer -> Integer -> String
showScreenRow screen minX maxX y = [showScreenCell screen x y | x <- [minX..maxX]] 

showScreenCell :: Field -> Integer -> Integer -> Char
showScreenCell screen x y = 
    case (M.findWithDefault Empty (x, y) screen) of 
        Empty -> ' '
        Wall -> '#'
        Block -> '*'
        Paddle -> '='
        Ball -> '+'

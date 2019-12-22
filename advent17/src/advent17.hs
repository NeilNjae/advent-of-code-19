import Debug.Trace

import Intcode

import qualified Data.Text.IO as TIO

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Data.List
import qualified Data.Set as S
-- import Data.Char
import Data.List

type Position = (Integer, Integer) -- x, y
type Boundary = [Position]
data Direction = North | East | South | West deriving (Show, Eq, Ord)
data ReturnValue = Static | Moved | Goal deriving (Show, Eq, Ord)

data Droid = Droid
    { _machine :: Machine
    , _executionState :: ExecutionState
    , _currentInput :: [Integer]
    , _machineOutput :: [Integer]
    } deriving (Eq)

instance Show Droid where
  show d = "Droid {<m>, _executionState = " ++ show (_executionState d) ++
           ", _currentInput = " ++ show (_currentInput d) ++
           ", _machineOutput = " ++ show (_machineOutput d) ++
           " }"

type Scaffold = S.Set Position

data ScaffoldBuilder = ScaffoldBuilder { _scaffold :: Scaffold
                     , _r :: Integer
                     , _c :: Integer
                     , _droidPos :: Position
                     , _droidDirection :: Direction
                     } deriving (Show, Eq)


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent17.txt"
        let mem = parseMachineMemory text
        -- print mem
        let sb = buildScaffold mem
        print $ part1 sb
        -- print $ part2 mem


part1 sb = S.foldl (+) 0 $ S.map alignmentParam intersections
    where scaffold = _scaffold sb
          intersections = S.filter (isIntersection scaffold) scaffold


buildScaffold mem = foldl' addGridChar emptyScaffoldBuilder output
    where (_, _, output) = runProgram [] mem
          emptyScaffoldBuilder = ScaffoldBuilder {_scaffold = S.empty, _r = 0, _c = 0, 
                    _droidPos = (0, 0), _droidDirection = North }


addGridChar sb 10  = sb { _r = _r sb + 1, _c = 0 }
addGridChar sb 46  = sb { _c = _c sb + 1 }
addGridChar sb 35  = sb { _scaffold = S.insert (_r sb, _c sb) $ _scaffold sb, 
                            _c = _c sb + 1 }
addGridChar sb 94  = sb { _scaffold = S.insert (_r sb, _c sb) $ _scaffold sb, 
                            _c = _c sb + 1,
                            _droidPos = (_r sb, _c sb), _droidDirection = North }
addGridChar sb 118 = sb { _scaffold = S.insert (_r sb, _c sb) $ _scaffold sb, 
                            _c = _c sb + 1,
                            _droidPos = (_r sb, _c sb), _droidDirection = South }
addGridChar sb 60  = sb { _scaffold = S.insert (_r sb, _c sb) $ _scaffold sb, 
                            _c = _c sb + 1,
                            _droidPos = (_r sb, _c sb), _droidDirection = West }
addGridChar sb 61  = sb { _scaffold = S.insert (_r sb, _c sb) $ _scaffold sb, 
                            _c = _c sb + 1,
                            _droidPos = (_r sb, _c sb), _droidDirection = East }


isIntersection :: Scaffold -> Position -> Bool
isIntersection scaffold (r, c) = neighbours `S.isSubsetOf` scaffold
    where  neighbours = [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

alignmentParam :: Position -> Integer
alignmentParam (r, c) = r * c


-- runDroid :: Droid -> Direction -> (Droid, ReturnValue)
-- runDroid droid direction = (droid', found)
--     where   ci = _currentInput droid
--             droid' = runDroidMachine (droid {_currentInput = ci ++ [commandOf direction]})
--             found = returnValue $ last $ _machineOutput droid'


-- runDroidMachine :: Droid -> Droid
-- runDroidMachine d = d { _machine = machine'
--                       , _executionState = halted
--                       , _machineOutput = output
--                       }
--     where   machine = _machine d
--             input = _currentInput d
--             (halted, machine', output) = runMachine input machine


showHull :: Hull -> String
showHull screen = unlines rows
    where   minX = minimum $ map fst $ M.keys screen
            minY = minimum $ map snd $ M.keys screen
            maxX = maximum $ map fst $ M.keys screen
            maxY = maximum $ map snd $ M.keys screen
            rows = [showHullRow screen minX maxX y | y <- [minY..maxY]]

showHullRow :: Hull -> Integer -> Integer -> Integer -> String
showHullRow screen minX maxX y = [showHullCell screen x y | x <- [minX..maxX]] 

showHullCell :: Hull -> Integer -> Integer -> Char
showHullCell screen x y = 
    case (M.findWithDefault Unknown (x, y) screen) of 
        Empty _ _ True -> 'O'
        Empty _ _ _ -> '.'
        Wall -> '\x2588'
        Unknown -> ' '

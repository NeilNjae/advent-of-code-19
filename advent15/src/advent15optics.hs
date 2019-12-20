import Debug.Trace

import Intcode

import qualified Data.Text.IO as TIO

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Data.List
import qualified Data.Set as S
import Control.Lens
import Data.Map.Lens
import Data.Maybe

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
makeLenses ''Droid

instance Show Droid where
  show d = "Droid {<m>, _executionState = " ++ show (_executionState d) ++
           ", _currentInput = " ++ show (_currentInput d) ++
           ", _machineOutput = " ++ show (_machineOutput d) ++
           " }"

data Cell = Vacant { _droid :: Droid 
                  , _fromStart :: Integer
                  , _isGoal :: Bool
                  } 
                  | Wall 
                  | Unknown
                  deriving (Show, Eq)
makeLenses ''Cell
makePrisms ''Cell

type Hull = M.Map Position Cell


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent15.txt"
        let mem = parseMachineMemory text
        -- print mem
        print $ part1 mem
        print $ part2 mem

part1 mem = _fromStart $ snd $ M.findMin $ M.filter (containsGoal) hull
    where hull = fst $ head $ searchHull $ initialHullBoundary mem


part2 mem = fillTime hull S.empty [(start, 0)] 0
    where hull = completeHull $ initialHullBoundary mem
          start = fst $ M.findMin $ M.filter (containsGoal) hull


step :: Position -> Direction -> Position
step (x, y) North = (x, y + 1)
step (x, y) East  = (x + 1, y)
step (x, y) South = (x, y - 1)
step (x, y) West  = (x - 1, y)

commandOf :: Direction -> Integer
commandOf North = 1
commandOf South = 2
commandOf West  = 3
commandOf East  = 4

returnValue 0 = Static
returnValue 1 = Moved
returnValue 2 = Goal


buildDroid :: [Integer] -> Droid
buildDroid mem = Droid
    { _machine = makeMachine mem
    , _executionState = Runnable
    , _currentInput = []
    , _machineOutput = []
    }

initialHullBoundary :: [Integer] -> (Hull, Boundary)
initialHullBoundary mem = (hull, [(0, 0)])
    where robot = buildDroid mem
          hull = M.singleton (0, 0) (Vacant {_droid = robot, _fromStart = 0, _isGoal = False})


searchHull :: (Hull, Boundary) -> [(Hull, Boundary)]
searchHull hullBoundary = dropWhile goalNotFound $ iterate searchHullStep hullBoundary


completeHull :: (Hull, Boundary) -> Hull
completeHull hullBoundary = fst $ head $ dropWhile incomplete $ iterate searchHullStep hullBoundary


searchHullStep :: (Hull, Boundary) -> (Hull, Boundary)
-- searchHullStep (hull, _) | trace (showHull hull) False = undefined
searchHullStep (hull, []) = (hull, [])
searchHullStep (hull, (here:boundary)) = foldl' (searchHullDirection here) (hull, boundary) directions
    where directions = [North, East, South, West] :: [Direction]

searchHullDirection :: Position -> (Hull, Boundary) -> Direction -> (Hull, Boundary)
searchHullDirection here (hull, boundary) direction
    | there `M.member` hull = (hull, boundary)
    | found == Static = (M.insert there Wall hull, boundary)
    | otherwise = (M.insert there newCell hull, boundary ++ [there])
    where there = step here direction
          -- robot = _droid $ hull!here
          -- robot = fromJust $ preview (at here . _Just . isGoal) hull
          -- robot = fromJust $ hull ^? at here . _Just . droid
          robot = fromJust $ hull ^? ix here . droid
          -- distance = _fromStart $ hull!here
          distance = fromJust $ hull ^? ix here . fromStart
          (robot', found) = runDroid robot direction
          -- newCell = Vacant { _droid = robot'
          --                 , _fromStart = distance + 1
          --                 , _isGoal = (found == Goal)
          --                 }
          newCell = _Vacant # (robot', distance + 1, found == Goal)

fillTime :: Hull -> (S.Set Position) -> [(Position, Integer)] -> Integer -> Integer
fillTime _ _ [] t = t
fillTime hull closed ((here, t):boundary) maxt
    | hull!here == Wall = fillTime hull closed boundary maxt
    | S.member here closed = fillTime hull closed boundary maxt
    | otherwise = fillTime hull closed' (boundary ++ neighbours) (max maxt t)
    where closed' = S.insert here closed
          neighbours = map (\d -> (step here d, t + 1)) directions
          directions = [North, East, South, West] :: [Direction]

goalNotFound :: (Hull, Boundary) -> Bool
goalNotFound (hull, _boundary) = M.null $ M.filter containsGoal hull

containsGoal :: Cell -> Bool
-- containsGoal Wall = False
-- containsGoal c = _isGoal c
containsGoal c = fromMaybe False $ c ^? isGoal


incomplete (_, []) = False
incomplete (_, (_:_)) = True


runDroid :: Droid -> Direction -> (Droid, ReturnValue)
runDroid robot direction = (robot', found)
    where   ci = _currentInput robot
            robot' = runDroidMachine (robot {_currentInput = ci ++ [commandOf direction]})
            found = returnValue $ last $ _machineOutput robot'


runDroidMachine :: Droid -> Droid
-- runDroidMachine d = d { _machine = machine'
--                       , _executionState = halted
--                       , _machineOutput = output
--                       }
    -- where   machine = _machine d
    --         input = _currentInput d
    --         (halted, machine', output) = runMachine input machine
runDroidMachine d = d & machine .~ machine' 
                      & executionState .~ halted 
                      & machineOutput .~ output
    where (halted, machine', output) = runMachine (d ^. currentInput) (d ^. machine)


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
        Vacant _ _ True -> 'O'
        Vacant _ _ _ -> '.'
        Wall -> '\x2588'
        Unknown -> ' '

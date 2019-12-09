import Debug.Trace

import Intcode

import qualified Data.Text.IO as TIO

import qualified Data.IntMap.Strict as M
import Data.IntMap.Strict ((!))
import Data.List
import Data.Function (on)


data EncapsulatedMacine = EncapsulatedMacine 
    { _machine :: Machine
    , _executionState :: ExecutionState
    , _initialInput :: [Integer]
    , _currentInput :: [Integer]
    , _machineOutput :: [Integer]
    } deriving (Show, Eq)

type Pipeline = M.IntMap EncapsulatedMacine


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent07.txt"
        let mem = parseMachineMemory text
        print $ part1 mem
        print $ part2 mem


part1 mem = maximum outputs
    where inputs = permutations [0..4]
          outputs = map (chainMachines mem) inputs

chainMachines mem settings = foldl' (chainMachine mem) 0 settings

chainMachine mem prevOutput setting = last output
    where (_, _, output) = runProgram [setting, prevOutput] mem


part2 mem = maximum outputs
    where   inputs = permutations [5..9]
            pipelines = map (buildPipeline mem) inputs
            outputs = map runPipeline pipelines

buildPipeline :: [Integer] -> [Integer] -> Pipeline
buildPipeline mem input = M.insert 0 machine0' pipeline
    where pipeline = M.fromList $ zip [0..] $ map (encapsulate mem) input
          machine0 = pipeline!0
          machine0' = machine0 { _initialInput = (_initialInput machine0) ++ [0]}


encapsulate :: [Integer] -> Integer -> EncapsulatedMacine
encapsulate mem input = EncapsulatedMacine 
    { _machine = makeMachine mem
    , _executionState = Runnable
    , _initialInput = [input]
    , _machineOutput = []
    , _currentInput = [input]
    }


runPipeline :: Pipeline -> Integer
-- runPipeline pipeline | trace (pipelineTrace pipeline) False = undefined
runPipeline pipeline 
    | finished pipeline = last $ _machineOutput $ snd $ M.findMax pipeline
    | otherwise = runPipeline pipeline''
    where   (indexToRun, machineToRun) = M.findMin $ runnableMachines pipeline
            feedsIntoIndex = (indexToRun + 1) `mod` (M.size pipeline)
            feedsIntoMachine = pipeline!feedsIntoIndex
            fimi = _initialInput feedsIntoMachine
            machine' = runEncapsulatedMachine machineToRun
            fullOutput = _machineOutput machine'
            feedsIntoState = case (_executionState feedsIntoMachine) of
                                  Blocked -> Runnable
                                  Terminated -> Terminated
                                  Runnable -> Runnable
            feedsIntoMachine' = feedsIntoMachine {_executionState = feedsIntoState, _currentInput = fimi ++ fullOutput}
            pipeline' = M.insert indexToRun machine' pipeline
            pipeline'' = M.insert feedsIntoIndex feedsIntoMachine' pipeline'



pipelineTrace :: Pipeline -> String
pipelineTrace pipeline = show $ M.toList $ M.map emTrace pipeline

emTrace e = intercalate " ; " terms
    where terms = [ show $ _executionState e
                  , "in"
                  , show $ _currentInput e 
                  , "out"
                  , show $ _machineOutput e
                  ]

finished :: Pipeline -> Bool
finished = M.null . runnableMachines

runnableMachines :: Pipeline -> Pipeline
runnableMachines = M.filter (\e -> _executionState e == Runnable)

runEncapsulatedMachine :: EncapsulatedMacine -> EncapsulatedMacine
runEncapsulatedMachine e = e { _machine = machine'
                             , _executionState = halted
                             , _machineOutput = (_machineOutput e) ++ output
                             }
    where   machine = _machine e
            input = _currentInput e
            (halted, machine', output) = runMachine input machine

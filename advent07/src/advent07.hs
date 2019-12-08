import Debug.Trace

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

import Control.Monad (unless)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS.Strict


import qualified Data.IntMap.Strict as M
import Data.IntMap.Strict ((!))
import Data.List
import Data.Function (on)

type Memory = M.IntMap Int

data Machine = Machine { _memory :: Memory
                       , _ip :: Int
                       , _inputIndex :: Int
                       } 
               deriving (Show, Eq)

type ProgrammedMachine = RWS [Int] [Int] Machine

data EncapsulatedMacine = EncapsulatedMacine 
    { _machine :: Machine
    , _executionState :: ExecutionState
    , _initialInput :: [Int]
    , _currentInput :: [Int]
    , _machineOutput :: [Int]
    } deriving (Show, Eq)

data ParameterMode = Position | Immediate deriving (Ord, Eq, Show)

data ExecutionState = Runnable | Blocked | Terminated  deriving (Ord, Eq, Show)

type Pipeline = M.IntMap EncapsulatedMacine


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent07.txt"
        let mem = successfulParse text
        print $ part1 mem
        print $ part2 mem


part1 mem = maximum outputs
    where inputs = permutations [0..4]
          outputs = map (chainMachines mem) inputs

chainMachines mem settings = foldl' (chainMachine mem) 0 settings

chainMachine mem prevOutput setting = findMachineOutput [setting, prevOutput] mem


part2 mem = maximum outputs
    where   inputs = permutations [5..9]
            pipelines = map (buildPipeline mem) inputs
            outputs = map runPipeline pipelines

buildPipeline :: [Int] -> [Int] -> Pipeline
buildPipeline mem input = M.insert 0 machine0' pipeline
    where pipeline = M.fromList $ zip [0..] $ map (encapsulate mem) input
          machine0 = pipeline!0
          machine0' = machine0 { _initialInput = (_initialInput machine0) ++ [0]}


encapsulate :: [Int] -> Int -> EncapsulatedMacine
encapsulate mem input = EncapsulatedMacine 
    { _machine = makeMachine mem
    , _executionState = Runnable
    , _initialInput = [input]
    , _machineOutput = []
    , _currentInput = [input]
    }


runPipeline :: Pipeline -> Int
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
            (halted, machine', output) = runRWS runAll input machine


findMachineOutput :: [Int] -> [Int] -> Int
findMachineOutput inputs program = last output
    where (_haltedBecause, _machine, output) = runRWS runAll inputs (makeMachine program)


makeMachine :: [Int] -> Machine
makeMachine memory = Machine    {_ip = 0, _inputIndex = 0
                                , _memory = M.fromList $ zip [0..] memory
                                }


runAll :: ProgrammedMachine ExecutionState
runAll = do mem <- gets _memory
            ip <- gets _ip
            input <- ask
            iIndex <- gets _inputIndex
            let acutalInputLength = length input
            let requiredInputLength = iIndex + 1
            if (mem!ip == 99)
            then return Terminated
            else    if (mem!ip == 3 && requiredInputLength > acutalInputLength)
                    then return Blocked
                    else do runStep
                            runAll

runStep :: ProgrammedMachine ()
runStep = 
    do mem <- gets _memory
       ip <- gets _ip
       let opcode = (mem!ip) `mod` 100
       let modes = parameterModes ((mem!ip) `div` 100)
       fetchInput opcode
       putOutput opcode modes
       mem' <- gets _memory
       let (mem'', ip') = perform opcode ip modes mem'
       modify (\m -> m {_ip = ip', _memory = mem''})

fetchInput :: Int -> ProgrammedMachine ()
-- fetchInput opcode | trace ("Input with opcode " ++ show opcode) False = undefined
fetchInput 3 =
    do mem <- gets _memory
       ip <- gets _ip
       inputIndex <- gets _inputIndex
       inputs <- ask 
       let mem' = iInsert (ip + 1) (inputs!!inputIndex) mem
       modify (\m -> m {_inputIndex = inputIndex + 1, _memory = mem'})
fetchInput _ = return ()

putOutput :: Int -> [ParameterMode] -> ProgrammedMachine ()
-- putOutput opcode _modes | trace ("Output with opcode " ++ show opcode) False = undefined
putOutput 4 modes =
    do mem <- gets _memory
       ip <- gets _ip
       let v = getMemoryValue (ip + 1) (modes!!0) mem
       tell [v]
putOutput _ _ = return ()       


perform :: Int -> Int -> [ParameterMode] -> Memory -> (Memory, Int)
-- perform instr ip modes mem | trace ("Perform ip " ++ show ip ++ " opcode " ++ show instr ++ " modes " ++ (show (take 3 modes)) ++ " args " ++ (intercalate ", " (map show [(mem!(ip+1)), (mem!(ip+2)), (mem!(ip+3))]))) False = undefined
perform 1 ip modes mem = (iInsert (ip + 3) (a + b) mem, ip + 4)
    where a = getMemoryValue (ip + 1) (modes!!0) mem
          b = getMemoryValue (ip + 2) (modes!!1) mem
perform 2 ip modes mem = (iInsert (ip + 3) (a * b) mem, ip + 4)
    where a = getMemoryValue (ip + 1) (modes!!0) mem
          b = getMemoryValue (ip + 2) (modes!!1) mem
perform 3 ip _ mem = (mem, ip + 2)
perform 4 ip _ mem = (mem, ip + 2)
perform 5 ip modes mem = (mem, ip')
    where a = getMemoryValue (ip + 1) (modes!!0) mem
          b = getMemoryValue (ip + 2) (modes!!1) mem
          ip' = if a /= 0 then b else ip + 3
perform 6 ip modes mem = (mem, ip')
    where a = getMemoryValue (ip + 1) (modes!!0) mem
          b = getMemoryValue (ip + 2) (modes!!1) mem
          ip' = if a == 0 then b else ip + 3
perform 7 ip modes mem = (iInsert (ip + 3) res mem, ip + 4)
    where a = getMemoryValue (ip + 1) (modes!!0) mem
          b = getMemoryValue (ip + 2) (modes!!1) mem
          res = if a < b then 1 else 0
perform 8 ip modes mem = (iInsert (ip + 3) res mem, ip + 4)
    where a = getMemoryValue (ip + 1) (modes!!0) mem
          b = getMemoryValue (ip + 2) (modes!!1) mem
          res = if a == b then 1 else 0
perform _ ip _ mem = (mem, ip)


getMemoryValue loc Position mem = mem!>loc
getMemoryValue loc Immediate mem = mem!loc


parameterModes :: Int -> [ParameterMode]
parameterModes modeCode = unfoldr generateMode modeCode

generateMode :: Int -> Maybe (ParameterMode, Int)
generateMode modeCode = Just (mode, modeCode `div` 10)
    where mode = case (modeCode `mod` 10) of 
                    0 -> Position
                    1 -> Immediate


-- Some IntMap utility functions, for syntactic sugar

-- prefix version of (!)
lkup k m = m!k

-- indirect lookup
(!>) m k = m!(m!k)

-- indirect insert
iInsert k v m = M.insert (m!k) v m



-- Parse the input file
type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty
-- sc = L.space (skipSome (char ' ')) CA.empty CA.empty

lexeme  = L.lexeme sc
integer = lexeme L.decimal
signedInteger = L.signed sc integer
symb = L.symbol sc
comma = symb ","

memoryP = signedInteger `sepBy` comma

successfulParse :: Text -> [Int]
successfulParse input = 
        case parse memoryP "input" input of
                Left  _err -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right memory -> memory
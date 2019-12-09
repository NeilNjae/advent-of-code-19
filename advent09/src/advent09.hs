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


import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Data.List
import Data.Function (on)

type Memory = M.Map Integer Integer

data Machine = Machine { _memory :: Memory
                       , _ip :: Integer
                       , _inputIndex :: Int
                       , _rb :: Integer
                       } 
               deriving (Show, Eq)

type ProgrammedMachine = RWS [Integer] [Integer] Machine

data ExecutionState = Runnable | Blocked | Terminated  deriving (Ord, Eq, Show)

data ParameterMode = Position | Immediate | Relative deriving (Ord, Eq, Show)


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent09.txt"
        let mem = successfulParse text
        print $ part1 mem
        print $ part2 mem


part1 mem = findMachineOutput [1] mem

part2 mem = findMachineOutput [2] mem


findMachineOutput :: [Integer] -> [Integer] -> [Integer]
findMachineOutput inputs program = output
    where (_haltedBecause, _machine, output) = runRWS runAll inputs (makeMachine program)


makeMachine :: [Integer] -> Machine
makeMachine memory = Machine    {_ip = 0, _inputIndex = 0, _rb = 0
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
       rb <- gets _rb
       let opcode = (mem!ip) `mod` 100
       let modes = parameterModes ((mem!ip) `div` 100)
       fetchInput opcode modes
       putOutput opcode modes
       mem' <- gets _memory
       let (mem'', ip', rb') = perform opcode ip modes rb mem'
       modify (\m -> m {_ip = ip', _memory = mem'', _rb = rb'})

fetchInput :: Integer -> [ParameterMode] -> ProgrammedMachine ()
-- fetchInput opcode _modes | trace ("Input with opcode " ++ show opcode) False = undefined
fetchInput 3 modes =
    do mem <- gets _memory
       ip <- gets _ip
       rb <- gets _rb
       inputIndex <- gets _inputIndex
       inputs <- ask 
       let mem' = iInsert (ip + 1) (modes!!0) rb (inputs!!inputIndex) mem
       modify (\m -> m {_inputIndex = inputIndex + 1, _memory = mem'})
fetchInput _ _ = return ()

putOutput :: Integer -> [ParameterMode] -> ProgrammedMachine ()
-- putOutput opcode _modes | trace ("Output with opcode " ++ show opcode) False = undefined
putOutput 4 modes =
    do mem <- gets _memory
       ip <- gets _ip
       rb <- gets _rb
       let v = getMemoryValue (ip + 1) (modes!!0) rb mem
       tell [v]
putOutput _ _ = return ()       


perform :: Integer -> Integer -> [ParameterMode] -> Integer -> Memory -> (Memory, Integer, Integer)
-- perform instr ip modes rb mem | trace ("Perform ip " ++ show ip ++ " opcode " ++ show instr ++ " modes " ++ (show (take 3 modes)) ++ " rb " ++ (show rb) ++ " args " ++ (intercalate ", " (map show [(mem!(ip+1)), (mem!(ip+2)), (mem!(ip+3))]))) False = undefined
perform 1 ip modes rb mem = (iInsert (ip + 3) (modes!!2) rb (a + b) mem, ip + 4, rb)
    where a = getMemoryValue (ip + 1) (modes!!0) rb mem
          b = getMemoryValue (ip + 2) (modes!!1) rb mem
perform 2 ip modes rb mem = (iInsert (ip + 3) (modes!!2) rb (a * b) mem, ip + 4, rb)
    where a = getMemoryValue (ip + 1) (modes!!0) rb mem
          b = getMemoryValue (ip + 2) (modes!!1) rb mem
perform 3 ip _ rb mem = (mem, ip + 2, rb)
perform 4 ip _ rb mem = (mem, ip + 2, rb)
perform 5 ip modes rb mem = (mem, ip', rb)
    where a = getMemoryValue (ip + 1) (modes!!0) rb mem
          b = getMemoryValue (ip + 2) (modes!!1) rb mem
          ip' = if a /= 0 then b else ip + 3
perform 6 ip modes rb mem = (mem, ip', rb)
    where a = getMemoryValue (ip + 1) (modes!!0) rb mem
          b = getMemoryValue (ip + 2) (modes!!1) rb mem
          ip' = if a == 0 then b else ip + 3
perform 7 ip modes rb mem = (iInsert (ip + 3) (modes!!2) rb res mem, ip + 4, rb)
    where a = getMemoryValue (ip + 1) (modes!!0) rb mem
          b = getMemoryValue (ip + 2) (modes!!1) rb mem
          res = if a < b then 1 else 0
perform 8 ip modes rb mem = (iInsert (ip + 3) (modes!!2) rb res mem, ip + 4, rb)
    where a = getMemoryValue (ip + 1) (modes!!0) rb mem
          b = getMemoryValue (ip + 2) (modes!!1) rb mem
          res = if a == b then 1 else 0
perform 9 ip modes rb mem = (mem, ip + 2, rb + a)
    where a = getMemoryValue (ip + 1) (modes!!0) rb mem
perform _ ip _ rb mem = (mem, ip, rb)


getMemoryValue loc Position rb mem = getMemoryValue loc' Immediate rb mem
    where loc' = M.findWithDefault 0 loc mem
getMemoryValue loc Immediate _ mem = M.findWithDefault 0 loc mem
getMemoryValue loc Relative rb mem = getMemoryValue loc' Immediate 0 mem
    where loc' = rb + M.findWithDefault 0 loc mem

-- indirect insert
iInsert :: Integer -> ParameterMode -> Integer -> Integer -> Memory -> Memory
iInsert loc Position _rb value mem = M.insert iloc value mem
    where iloc = M.findWithDefault 0 loc mem
iInsert loc Relative rb value mem = M.insert iloc value mem
    where iloc = rb + M.findWithDefault 0 loc mem

parameterModes :: Integer -> [ParameterMode]
parameterModes modeCode = unfoldr generateMode modeCode

generateMode :: Integer -> Maybe (ParameterMode, Integer)
generateMode modeCode = Just (mode, modeCode `div` 10)
    where mode = case (modeCode `mod` 10) of 
                    0 -> Position
                    1 -> Immediate
                    2 -> Relative

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

successfulParse :: Text -> [Integer]
successfulParse input = 
        case parse memoryP "input" input of
                Left  _err -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right memory -> memory
                
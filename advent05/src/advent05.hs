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

import qualified Data.IntMap.Strict as M
import Data.IntMap.Strict ((!))
import Data.List

type Memory = M.IntMap Int

data Machine = Machine { _memory :: Memory
                       , _ip :: Int
                       , _inputIndex :: Int
                       } 
               deriving (Show, Eq)

type ProgrammedMachine = WriterT [Int] (ReaderT ([Int]) (State Machine)) ()

data ParameterMode = Position | Immediate deriving (Ord, Eq, Show)


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent05.txt"
        let mem = successfulParse text
        print $ findMachineOutput [1] mem
        print $ findMachineOutput [5] mem


findMachineOutput inputs program = last output
    where   finalStack = 
                runState (
                    runReaderT (
                        runWriterT runAll
                               ) 
                        inputs
                         ) 
                         (makeMachine program)
            ((_retval, output), _machine) = finalStack

makeMachine :: [Int] -> Machine
makeMachine memory = Machine    {_ip = 0, _inputIndex = 0
                                , _memory = M.fromList $ zip [0..] memory
                                }


runAll :: ProgrammedMachine
runAll = do mem <- gets _memory
            ip <- gets _ip
            unless (mem!ip == 99)
                do runStep
                   runAll

runStep :: ProgrammedMachine
runStep = 
    do mem <- gets _memory
       ip <- gets _ip
       let opcode = (mem!ip) `mod` 100
       let modes = parameterModes  ((mem!ip) `div` 100)
       fetchInput opcode
       putOutput opcode modes
       mem' <- gets _memory
       let (mem'', ip') = perform opcode ip modes mem'
       modify (\m -> m {_ip = ip', _memory = mem''})


-- fetchInput opcode | trace ("Input with opcode " ++ show opcode) False = undefined
fetchInput 3 =
    do mem <- gets _memory
       ip <- gets _ip
       inputIndex <- gets _inputIndex
       inputs <- ask 
       let mem' = iInsert (ip + 1) (inputs!!inputIndex) mem
       modify (\m -> m {_inputIndex = inputIndex + 1, _memory = mem'})
fetchInput _ = return ()


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
-- Some code taken from [AoC 2017 day 5](https://adventofcode.com/2017/day/5), 
--    and some from [AoC 2018 day 21](https://adventofcode.com/2018/day/21)

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

import Control.Monad (unless)
import Control.Monad.State.Strict

import qualified Data.IntMap.Strict as M
import Data.IntMap.Strict ((!))

type Memory = M.IntMap Int

data Machine = Machine { _memory :: Memory
                       , _ip :: Int
                       } 
               deriving (Show, Eq)

type ProgrammedMachine = State Machine ()


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent02.txt"
        let mem = successfulParse text
        let machine = makeMachine mem
        print $ part1 machine
        print $ part2 machine


-- part1 machine = (_memory $ execState runAll machine1202)!0
--     where machine1202 = machine { _memory = M.insert 1 12 $ M.insert 2 2 $ _memory machine }


part1 = nounVerbResult 12 2

part2Target = 19690720

part2 machine = noun * 100 + verb
    where (noun, verb) = head $ [(n, v) | n <- [0..99], v <- [0..99],
                                          nounVerbResult n v machine == part2Target ]


makeMachine :: [Int] -> Machine
makeMachine memory = Machine {_ip = 0, _memory = M.fromList $ zip [0..] memory}

nounVerbResult :: Int -> Int -> Machine -> Int
nounVerbResult noun verb machine = machineOutput nvMachine
    where nvMachine0 = machineNounVerb machine noun verb
          nvMachine = execState runAll nvMachine0

machineNounVerb :: Machine -> Int -> Int -> Machine
machineNounVerb machine noun verb = machine { _memory = M.insert 1 noun $ M.insert 2 verb $ _memory machine }

machineOutput :: Machine -> Int
machineOutput machine = (_memory machine)!0


runAll :: ProgrammedMachine
runAll = do m0 <- get
            unless (lkup (_ip m0) (_memory m0) == 99)
                do runStep
                   runAll

runStep :: ProgrammedMachine
runStep = 
    do m0 <- get
       let mem = _memory m0
       let ip = _ip m0
       let (mem', ip') = perform (mem!ip) ip mem
       put m0 {_ip = ip', _memory = mem'}

perform :: Int -> Int -> Memory -> (Memory, Int)
perform 1 ip mem = (iInsert (ip + 3) (a + b) mem, ip + 4)
    where a = mem!>(ip + 1)
          b = mem!>(ip + 2)
perform 2 ip mem = (iInsert (ip + 3) (a * b) mem, ip + 4)
    where a = mem!>(ip + 1)
          b = mem!>(ip + 2)


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
-- signedInteger = L.signed sc integer
symb = L.symbol sc
comma = symb ","

memoryP = integer `sepBy` comma

successfulParse :: Text -> [Int]
successfulParse input = 
        case parse memoryP "input" input of
                Left  _err -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right memory -> memory
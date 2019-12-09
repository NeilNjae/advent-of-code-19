
import qualified Data.Text.IO as TIO

import Intcode

import qualified Data.Map as M
import Data.Map ((!))

main :: IO ()
main = do 
        text <- TIO.readFile "data/advent02.txt"
        let mem = parseMachineMemory text
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


nounVerbResult :: Integer -> Integer -> Machine -> Integer
nounVerbResult noun verb machine = machineOutput machine'
    where   (_, machine', _) = runMachine [] nvMachine
            nvMachine = machineNounVerb machine noun verb

machineNounVerb :: Machine -> Integer -> Integer -> Machine
machineNounVerb machine noun verb = machine { _memory = M.insert 1 noun $ M.insert 2 verb $ _memory machine }

machineOutput :: Machine -> Integer
machineOutput machine = (_memory machine)!0


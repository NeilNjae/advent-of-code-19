
import qualified Data.Text.IO as TIO

import Intcode


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent05.txt"
        let mem = parseMachineMemory text
        print $ findMachineOutput [1] mem
        print $ findMachineOutput [5] mem


findMachineOutput inputs program = last output
    where (_, _, output) = runProgram inputs program

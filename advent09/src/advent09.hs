import Intcode

-- import Data.Text (Text)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do 
        text <- TIO.readFile "data/advent09.txt"
        let mem = parseMachineMemory text
        print $ part1 mem
        print $ part2 mem

part1 mem = head output
    where (_, _, output) = runProgram [1] mem

part2 mem = head output
    where (_, _, output) = runProgram [2] mem


-- import Debug.Trace

import Intcode

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Char
import Data.List

main :: IO ()
main = do 
        text <- TIO.readFile "data/advent25.txt"
        let mem = parseMachineMemory text
        print $ length mem
        (machine, instructions, items) <- runPreamble mem
        -- runGameM machine instructions
        putStrLn $ passSecurity machine instructions items

runPreamble :: [Integer] -> IO (Machine, [String], [String])
runPreamble mem =
      do        
        instr <- TIO.readFile "data/advent25-instructions.txt"
        let instructions = lines $ T.unpack instr
        -- runGame mem $ lines $ T.unpack instructions
        let (machine, _output) = runCommand mem instructions
        let (_s, _machine1, output1) = runMachine (encodeCommands (instructions ++ ["inv"])) machine
        putStrLn $ decodeOutput output1
        let items = extractItems $ decodeOutput output1
        -- print items
        return (machine, instructions, items)


encodeCommands :: [String] -> [Integer]
-- encodeCommands cmds = map (fromIntegral . ord) $ concat $ map (++ "\n") cmds
encodeCommands = map (fromIntegral . ord) . concat . map (++ "\n")

decodeOutput :: [Integer] -> String
decodeOutput = map (chr . fromIntegral)

extractItems :: String -> [String]
extractItems text = items
    where candidates = lines text
          items = map (drop 2) $ filter (isPrefixOf "- ") candidates


powerList :: [a] -> [[a]]
powerList [] = [[]]
powerList (x:xs) = powerList xs ++ map (x:) (powerList xs) 

passSecurity :: Machine -> [String] -> [String] -> String
passSecurity machine instructions items = 
    "You keep: " ++ (intercalate ", " keeps) ++ "\n\n" ++ (attemptSecurity machine instructions validDropset)
    where
        dropsets = powerList items
        validDropset = head $ filter (passesSecurity machine instructions) dropsets
        keeps = items \\ validDropset

passesSecurity :: Machine -> [String] -> [String] -> Bool
passesSecurity machine instructions drops = not ("Alert" `isInfixOf` output)
    where output = attemptSecurity machine instructions drops

attemptSecurity :: Machine -> [String] -> [String] -> String
attemptSecurity machine instructions drops = decodeOutput output
    where dropCommands = map ("drop " ++ ) drops
          (_, _, output) = runMachine (encodeCommands (instructions ++ dropCommands ++ ["north"])) machine

runCommand :: [Integer] -> [String] -> (Machine, String)
runCommand mem inputs = ( machine, decodeOutput output )
    where (_state,  machine, output) = runProgram inputCodes mem
          inputCodes = encodeCommands inputs

runGame :: [Integer] -> [String] -> IO ()
runGame mem inputs = 
    do let (_, outputs) = runCommand mem inputs
       putStrLn outputs
       nextIn <- getLine
       runGame mem (inputs ++ [nextIn])

runGameM :: Machine -> [String] -> IO ()
runGameM machine inputs = 
    do nextIn <- getLine
       let (_s, machine1, output1) = runMachine (encodeCommands (inputs ++ [nextIn])) machine
       putStrLn $ decodeOutput output1
       runGameM machine1 (inputs ++ [nextIn])

-- import Debug.Trace

import Intcode

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Char
import Data.List
import Control.Monad.Reader

type CachedMachine a = Reader (Machine, [String]) a


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
passSecurity machine instructions items = "You keep: " ++ (intercalate ", " keeps) ++ "\n\n" ++ successResponse
    where
        env = (machine, instructions)
        dropsets = powerList items
        validDropset = head $ filter (\ds -> runReader (passesSecurity ds) env) dropsets
        successResponse = (runReader (attemptSecurity validDropset) env)
        keeps = items \\ validDropset

passesSecurity :: [String] -> CachedMachine Bool
passesSecurity drops = 
    do  output <- attemptSecurity drops
        return $ not ("Alert" `isInfixOf` output)

attemptSecurity :: [String] -> CachedMachine String
attemptSecurity drops = 
    do  let dropCommands = map ("drop " ++ ) drops
        output <- runCachedMachine dropCommands
        return output 

runCachedMachine :: [String] -> CachedMachine String
runCachedMachine dropCommands =
    do (machine, instructions) <- ask
       let (_, _, output) = runMachine (encodeCommands (instructions ++ dropCommands ++ ["north"])) machine
       return $ decodeOutput output



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

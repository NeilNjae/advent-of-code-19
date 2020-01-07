import Debug.Trace

import Intcode

import qualified Data.Text.IO as TIO

import Data.List
import Data.Char


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent21.txt"
        let mem = parseMachineMemory text
        let machine = makeMachine mem
        putStrLn $ part1 machine
        putStrLn $ part2 machine

part1Code :: [Integer]
part1Code = map (fromIntegral . ord) $ concatMap (++ "\n") 
    [ "OR  A T"
    , "AND B T"
    , "AND C T"
    , "NOT T J"
    , "AND D J"
    , "WALK"
    ]

part2Code :: [Integer]
part2Code = map (fromIntegral . ord) $ concatMap (++ "\n") 
    [ "OR  A T"
    , "AND B T"
    , "AND C T"
    , "NOT T J"
    -- e OR H -> T
    , "OR  E T"
    , "OR  H T"
    , "AND T J"
    , "AND D J"
    , "RUN"
    ]


part1 machine = drawOutput output
    where (_, _, output) = runMachine part1Code machine

part2 machine = drawOutput output
    where (_, _, output) = runMachine part2Code machine

drawOutput cs = t ++ n
    where t = map (chr . fromIntegral) $ filter (<= 128) cs
          n = show (filter ( > 128) cs)


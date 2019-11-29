{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

import Data.IntSet (IntSet)
import qualified Data.IntSet as S

main :: IO ()
main = do 
        text <- TIO.readFile "data/advent01.txt"
        let changes = successfulParse text
        print $ part1 changes
        print $ part2 changes


part1 :: [Int] -> Int
part1 = sum 

part2 :: [Int] -> Int
part2 = snd . head . dropWhile unRepeated . scanl merge (S.empty, 0) . cycle 

merge :: (IntSet, Int) -> Int -> (IntSet, Int)
merge (frequencies, frequency) change = (S.insert frequency frequencies, frequency + change)

unRepeated :: (IntSet, Int) -> Bool
unRepeated (frequencies, frequency) = frequency `S.notMember` frequencies

-- Parse the input file
type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty
-- sc = L.space (skipSome (char ' ')) CA.empty CA.empty


lexeme  = L.lexeme sc
integer = lexeme L.decimal
signedInteger = L.signed sc integer

changesP = many signedInteger

successfulParse :: Text -> [Int]
successfulParse input = 
        case parse changesP "input" input of
                Left  _err -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right changes -> changes     
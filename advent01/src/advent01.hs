import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent01.txt"
        let changes = successfulParse text
        print $ part1 changes
        print $ part2 changes


part1 :: [Int] -> Int
part1 = sum . map fuelRequired

part2 :: [Int] -> Int
part2 = sum . map fuelForFuel


fuelRequired :: Int -> Int
fuelRequired m = (m `div` 3) - 2

fuelForFuel :: Int -> Int
fuelForFuel = sum . takeWhile (> 0) . drop 1 . iterate fuelRequired


-- Parse the input file
type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty
-- sc = L.space (skipSome (char ' ')) CA.empty CA.empty


lexeme  = L.lexeme sc
integer = lexeme L.decimal
-- signedInteger = L.signed sc integer

moduleP = many integer

successfulParse :: Text -> [Int]
successfulParse input = 
        case parse moduleP "input" input of
                Left  _err -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right changes -> changes     
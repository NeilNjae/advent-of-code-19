import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

import Data.List (foldl')
import qualified Data.Set as S
import Data.Set ((\\))
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))

-- from satellite to primary
type Orbits = M.Map String String

type Transfers = S.Set String


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent06.txt"
        let directOrbits = successfulParse text
        let orbits = buildOrbits directOrbits
        print $ part1 orbits 
        print $ part2 orbits

part1 :: Orbits -> Int
part1 orbits = sum $ map (orbitCount orbits) $ M.keys orbits

part2 :: Orbits -> Int
part2 orbits = youDist + sanDist
    where youTrans = transferSteps orbits S.empty (orbits!"YOU")
          sanTrans = transferSteps orbits S.empty (orbits!"SAN")
          onlyYou = youTrans \\ sanTrans
          onlySan = sanTrans \\ youTrans
          youDist = S.size onlyYou
          sanDist = S.size onlySan  


buildOrbits :: [(String, String)] -> Orbits
buildOrbits = foldl' addOrbit M.empty

addOrbit :: Orbits -> (String, String) -> Orbits
addOrbit orbits (primary, satellite) = M.insert satellite primary orbits

orbitCount :: Orbits -> String -> Int
orbitCount orbits here
    | here `M.member` orbits = 1 + (orbitCount orbits (orbits!here))
    | otherwise = 0

transferSteps :: Orbits -> Transfers -> String -> Transfers
transferSteps orbits transfers here
    | here `M.member` orbits = transferSteps orbits transfers' there
    | otherwise = transfers'
    where there = orbits!here
          transfers' = S.insert here transfers


-- Parse the input file
type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty
-- sc = L.space (skipSome (char ' ')) CA.empty CA.empty

-- lexeme  = L.lexeme sc
-- integer = lexeme L.decimal
-- signedInteger = L.signed sc integer
symb = L.symbol sc
orbSep = symb ")"
identifierP = some alphaNumChar <* sc

orbitsP = many orbitP
orbitP = (,) <$> identifierP <* orbSep <*> identifierP

successfulParse :: Text -> [(String, String)]
successfulParse input = 
        case parse orbitsP "input" input of
                Left  _err -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right orbits -> orbits
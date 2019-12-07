import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

import Data.List (foldl')
-- import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!), (\\))

-- from satellite to primary
type Orbits = M.Map String String

-- transfer steps to each primary
type TransferDistances = M.Map String Int


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent06.txt"
        let directOrbits = successfulParse text
        let orbits = buildOrbits directOrbits
        print $ part1 orbits directOrbits
        print $ part2 orbits

part1 :: Orbits -> [(String, String)] -> Int
part1 orbits directOrbits = sum $ map (orbitCount orbits) satellites
    where satellites = map snd directOrbits

part2 :: Orbits -> Int
part2 orbits = youDist + sanDist
    where youTrans = transferDistance orbits M.empty (orbits!"YOU") 0
          sanTrans = transferDistance orbits M.empty (orbits!"SAN") 0
          onlyYou = youTrans \\ sanTrans
          onlySan = sanTrans \\ youTrans
          -- youDist = 1 + (maximum $ M.elems onlyYou)
          -- sanDist = 1 + (maximum $ M.elems onlySan)
          youDist = M.size onlyYou
          sanDist = M.size onlySan


buildOrbits :: [(String, String)] -> Orbits
buildOrbits = foldl' addOrbit M.empty

addOrbit :: Orbits -> (String, String) -> Orbits
addOrbit orbits (primary, satellite) = M.insert satellite primary orbits

orbitCount :: Orbits -> String -> Int
orbitCount orbits here
    | here `M.member` orbits = 1 + (orbitCount orbits (orbits!here))
    | otherwise = 0

transferDistance :: Orbits -> TransferDistances -> String -> Int -> TransferDistances
transferDistance orbits transfers here dist
    | here `M.member` orbits = transferDistance orbits transfers' there (dist + 1)
    | otherwise = transfers'
    where there = orbits!here
          transfers' = M.insert here dist transfers


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
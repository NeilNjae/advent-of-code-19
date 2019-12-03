import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

import Data.List (foldl')
import qualified Data.Map as M
import Data.Map ((!))

import Linear (V2(..), (^+^), (^-^), (*^), (*^))

data Direction = East | South | West | North deriving (Show, Eq)

type Location = V2 Int -- x, y

type Visited = M.Map Location Int

data Path = Path { _visited :: Visited
                 , _tip :: Location
                 , _currentLength :: Int
                 } 
               deriving (Show, Eq)

data Segment = Segment { _direction :: Direction
                       , _steps :: Int
                       } deriving (Show, Eq)


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent03.txt"
        let segmentss = successfulParse text
        let paths = travelAllPaths segmentss
        -- print $ travelPath $ head segmentss
        print $ part1 paths
        print $ part2 paths

part1 :: [Path] -> Int
part1 paths = closest $ crossovers paths

part2 :: [Path] -> Int
part2 paths = shortestPaths $ crossovers paths


closest :: Visited -> Int
closest points = snd $ M.findMin $ M.mapWithKey (\k _ -> manhattan k) points

shortestPaths :: Visited -> Int
shortestPaths crossings = minimum $ M.elems  crossings


crossovers :: [Path] -> Visited
crossovers travelledPaths = 
      foldl' (M.intersectionWith (+))
             (_visited $ head travelledPaths)
             (map _visited $ drop 1 travelledPaths)

travelAllPaths :: [[Segment]] -> [Path]
travelAllPaths = map travelPath

travelPath :: [Segment] -> Path
travelPath segments = foldl' travelSegment path0 segments
    where   path0 = Path { _visited = M.empty, _tip = V2 0 0, _currentLength = 0 }

travelSegment :: Path -> Segment -> Path
travelSegment path segment = path { _tip = tip', _visited = visited', _currentLength = len'}
    where   delta = facing $ _direction segment
            distance = _steps segment
            start = _tip path
            len = _currentLength path
            len' = len + distance
            visited = _visited path
            visited' = foldl' insertStep visited $ take distance $ drop 1 $ zip [len, (len + 1) ..] $ iterate (^+^ delta) start
            tip' = start ^+^ distance *^ delta
            insertStep visits (dist, loc) = if loc `M.member` visits
                                            then visits
                                            else M.insert loc dist visits

facing :: Direction -> Location
facing East = V2 1 0
facing South = V2 0 (-1)
facing West = V2 (-1) 0
facing North = V2 0 1


manhattan (V2 x y) = (abs x) + (abs y)

-- Parse the input file
type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty
-- sc = L.space (skipSome (char ' ')) CA.empty CA.empty

lexeme  = L.lexeme sc
integer = lexeme L.decimal
-- signedInteger = L.signed sc integer
symb = L.symbol sc
comma = symb ","

wiresP = some pathP
pathP = segmentP `sepBy1` comma

segmentP = segmentify <$> directionP <*> integer
    where segmentify direction steps = 
              Segment { _direction = direction, _steps = steps }


directionP = eP <|> sP <|> wP <|> nP
eP = (symb "R" *> pure East)
sP = (symb "D" *> pure South)
wP = (symb "L" *> pure West)
nP = (symb "U" *> pure North)


successfulParse :: Text -> [[Segment]]
successfulParse input = 
        case parse wiresP "input" input of
                Left  _err -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right wires -> wires
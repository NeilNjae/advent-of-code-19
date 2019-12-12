import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

import Linear (V3(..), (^+^), (^-^))

import qualified Data.Set as S

-- import Data.List (foldl')
-- import Data.Set ((\\))
-- import qualified Data.Map.Strict as M
-- import Data.Map.Strict ((!))


type Vec = V3 Integer
data Planet = Planet { _pos :: Vec, _vel :: Vec} deriving (Show, Eq, Ord)
type Planets = S.Set Planet


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent12a.txt"
        let planetsT = successfulParse text
        let planets = enplanet planetsT
        print planets
        print $ part1 planets


part1 planets = take 12 $ simulate planets


enplanet  = S.fromList . map (\p -> Planet {_pos = p, _vel = (V3 0 0 0)} )

_x (V3 x _ _) = x
_y (V3 _ y _) = y
_z (V3 _ _ z) = z


gravity (V3 x y z) = V3 (signum x) (signum y) (signum z)


simulate = iterate simulationStep 

simulationStep planets = planets''
    where   planets' = applyGravity planets
            planets'' = applyVelocity planets'


applyGravity planets = S.map (applyGravityHere planets) planets

applyGravityHere planets here = S.foldl' updateGravity here planets

updateGravity here there = here { _vel = vel'}
    where   vel = _vel here
            vel' = vel ^+^ gravity ((_pos there) ^-^ (_pos here))


applyVelocity = S.map applyVelocityHere

applyVelocityHere here = here {_pos = (_pos here) ^+^ (_vel here)}



-- Parse the input file
type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty
-- sc = L.space (skipSome (char ' ')) CA.empty CA.empty

lexeme  = L.lexeme sc
integer = lexeme L.decimal
signedInteger = L.signed sc integer
symb = L.symbol sc
equalP = symb "="
commaP = symb ","
identifierP = some alphaNumChar <* sc
openBracketP = symb "<"
closeBracketP = symb ">"

planetsP = many planetP

planetP = (between openBracketP closeBracketP) coordsP

coordsP = envector <$> (coordP `sepBy` commaP)
    where envector [x, y, z] = V3 x y z
coordP = identifierP *> equalP *> signedInteger


successfulParse :: Text -> [Vec]
successfulParse input = 
        case parse planetsP "input" input of
                Left  _err -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right planets -> planets
import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

import Linear (V3(..), V1(..), (^+^), (^-^))

import qualified Data.Set as S
import qualified Data.Vector as V

-- import Data.List (foldl')
-- import Data.Set ((\\))
-- import qualified Data.Map.Strict as M
-- import Data.Map.Strict ((!))


-- data Vec3 = Vec3 (V3 Integer) deriving (Show, Eq, Ord)
-- data Vec1 = Vec1 (V1 Integer) deriving (Show, Eq, Ord)
class (Ord a) => NVec a where
    (^+^^) :: a -> a -> a
    (^-^^) :: a -> a -> a
    nvZero :: a
    nvSignum :: a -> a
    nvAbsSum :: a -> Integer
instance NVec (V1 Integer) where
    x ^+^^ y  = x ^+^ y
    x ^-^^ y  = x ^-^ y
    nvZero = V1 0
    nvSignum (V1 x) = V1 (signum x)
    nvAbsSum (V1 x) = abs x
instance NVec (V3 Integer) where
    x ^+^^ y  = x ^+^ y
    x ^-^^ y  = x ^-^ y
    nvZero = V3 0 0 0
    nvSignum (V3 x y z) = V3 (signum x) (signum y) (signum z)
    nvAbsSum (V3 x y z) = (abs x) + (abs y) + (abs z)


-- data Vec = Vec1 (V1 Integer) | Vec3 (V3 Integer) 
--     deriving (Show, Eq, Ord)
-- data Planet1 = Planet { _pos :: (V1 Integer), _vel :: (V1 Integer)} deriving (Show, Eq, Ord)
data Planet a = Planet { _pos :: a , _vel :: a} deriving (Show, Eq, Ord)
-- type Planets1 = S.Set Planet1
type Planets a = S.Set (Planet a)


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent12.txt"
        let planetsT = successfulParse text
        let planets = enplanet planetsT
        print planets
        print $ part1 planets
        print $ part2 planets

part1 :: Planets (V3 Integer) -> Integer
part1 planets = systemEnergy $ head $ drop 1000 $ simulate planets

-- part2 :: Planets (V3 Integer) -> [Planet (V1 Integer)]
part2 planets = take 10 $ simulate $ head planetDimensions -- take 10 $ simulate (head planetDimensions)
    where planetDimensions = unzipPlanets planets


enplanet :: (NVec a) => [a] -> S.Set (Planet a)
enplanet = S.fromList . map (\p -> Planet {_pos = p, _vel = nvZero} )
-- enplanet (Vec3 p)  = S.fromList . map (\p -> Planet {_pos = (Vec3 p), _vel = Vec3 (V3 0 0 0)} )

-- _x (V3 x _ _) = x
-- _y (V3 _ y _) = y
-- _z (V3 _ _ z) = z

unzipPlanets :: S.Set (Planet (V3 Integer)) -> [S.Set (Planet (V1 Integer))]
unzipPlanets planets = dimensionSlice $ S.map unzipPlanet planets

unzipPlanet :: Planet (V3 Integer) -> [Planet (V1 Integer)]
unzipPlanet planet = map mkPlanet posVecs
    where posVecs = unzipVec $ _pos planet
          mkPlanet p = Planet {_pos = p, _vel = nvZero}

unzipVec :: V3 Integer -> [V1 Integer]
unzipVec (V3 x y z) = [V1 x, V1 y, V1 z]

dimensionSlice :: (NVec a) => S.Set [Planet a] -> [S.Set (Planet a)]
dimensionSlice slicedPlanets = [sliceDim d | d <- [0..2]]
    where sliceDim d = S.map (!!d) slicedPlanets



simulate :: (NVec a) => Planets a -> [Planets a]
simulate = iterate simulationStep 

countSimulate 

simulationStep :: (NVec a) => Planets a -> Planets a
simulationStep planets = planets''
    where   planets' = applyGravity planets
            planets'' = applyVelocity planets'


gravity :: (NVec a) => a -> a 
gravity v = nvSignum v

applyGravity :: (NVec a) => Planets a -> Planets a
applyGravity planets = S.map (applyGravityHere planets) planets

applyGravityHere :: (NVec a) => Planets a -> Planet a -> Planet a
applyGravityHere planets here = S.foldl' updateGravity here planets

updateGravity :: (NVec a) => Planet a -> Planet a -> Planet a
updateGravity here there = here { _vel = vel'}
    where   vel = _vel here
            vel' = vel ^+^^ gravity ((_pos there) ^-^^ (_pos here))

applyVelocity :: (NVec a) => Planets a -> Planets a
applyVelocity = S.map applyVelocityHere

applyVelocityHere :: (NVec a) => Planet a  -> Planet a
applyVelocityHere here = here {_pos = (_pos here) ^+^^ (_vel here)}




-- absSum (Vec1 (V1 x)) = (abs x)
-- absSum (Vec3 (V3 x y z)) = (abs x) + (abs y) + (abs z)

potentalEnergy planet = nvAbsSum $ _pos planet
kineticEnergy planet = nvAbsSum $ _vel planet
totalEnergy planet = (potentalEnergy planet) * (kineticEnergy planet)

systemEnergy = (S.foldl' (+) 0) . (S.map totalEnergy)



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


-- successfulParse :: Text -> [Vec]
successfulParse :: Text -> [V3 Integer]
successfulParse input = 
        case parse planetsP "input" input of
                Left  _err -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right planets -> planets
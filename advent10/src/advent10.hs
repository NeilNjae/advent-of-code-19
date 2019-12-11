import Data.Ratio
import qualified Data.Set as S
import qualified Data.Map.Strict as M
-- import Data.Map.Strict ((!))
import Linear (V2(..), (^+^), (^-^), (*^), (*^))
import Linear.Metric (norm)

import Data.List
import Data.Ord


type Bounds = (Int, Int)
type Position = V2 Int
type Delta = V2 (Ratio Int)

type Asteroids = S.Set Position

type TargetInfo = (Float, Float)
type Targets = M.Map TargetInfo Position

main :: IO ()
main = do 
        text <- readFile "data/advent10.txt"
        let (asteroids, bounds) = successfulParse text
        -- print asteroids
        let (monitor, visCount) = bestVisible bounds asteroids
        print visCount -- part 1
        let targets = makeTargets monitor (S.delete monitor asteroids)
        -- print targets
        print $ part2 targets


part2 targets = 100 * x + y
    where V2 x y = (targetSequence targets)!!199


bestVisible :: Bounds -> Asteroids -> (Position, Int)
bestVisible bounds asteroids = maximumBy (comparing snd) $ S.toList $ S.map (visibleCount bounds asteroids) asteroids

visibleCount :: Bounds -> Asteroids -> Position -> (Position, Int)
visibleCount bounds asteroids origin = (origin, S.size $ visible bounds origin asteroids)

visible :: Bounds -> Position -> Asteroids -> Asteroids
visible bounds origin asteroids = S.delete origin $ S.difference asteroids screened
    where screened = allScreenings bounds origin asteroids

allScreenings :: Bounds -> Position -> Asteroids -> Asteroids
allScreenings bounds origin asteroids = S.foldl' (screenings bounds origin) S.empty asteroids


screenings :: Bounds -> Position -> Asteroids -> Position -> Asteroids
screenings bounds origin@(V2 ox oy) screened0 target@(V2 tx ty) 
    | origin == target = screened0
    | otherwise        = S.union screened0 screened
    where maxComponent = max (abs (tx - ox)) (abs (ty - oy))
          delta = V2 ((tx - ox) % maxComponent) ((ty - oy) % maxComponent)
          startR = V2 (tx % 1) (ty % 1)
          rawScreens = takeWhile (inBounds bounds) [startR ^+^ n *^ delta | n <- [1..]]
          screens = filter isIntegral rawScreens
          screenInteger = map integerVec screens
          fullScreened = S.fromList screenInteger
          screened = S.delete target fullScreened

inBounds :: Bounds -> Delta -> Bool
inBounds (maxX, maxY) (V2 x y) = (x >= 0) && (x <= (maxX % 1)) && (y >= 0) && (y <= (maxY % 1))

integerVec :: Delta -> Position
integerVec (V2 x y) = V2 (numerator x) (numerator y)

isIntegral :: Delta -> Bool
isIntegral (V2 x y) = (denominator x == 1) && (denominator y == 1)


makeTargets :: Position -> Asteroids -> Targets
makeTargets origin asteroids = S.foldl' addTarget M.empty asteroids
    where addTarget m t = M.insert (targetInfo origin t) t m

targetInfo :: Position -> Position -> TargetInfo
targetInfo origin target = (angle, range)
    where V2 dx dy = target - origin
          angle = atan2 (fromIntegral dy) (fromIntegral dx)
          -- recipRange = 1 / (norm (V2 (fromIntegral dy) (fromIntegral dx)))
          range = norm (V2 (fromIntegral dy) (fromIntegral dx))

possibleTargets :: Float -> Targets -> Targets
possibleTargets angle targets = M.filterWithKey (\(a, _) _ -> a > angle) targets

firstTarget :: Targets -> (TargetInfo, Position)
firstTarget targets = M.findMin targets

targetSequence targets = targetNext ((- pi / 2) - 0.001) targets

targetNext :: Float -> Targets -> [Position]
targetNext angle targets 
    | M.null targets = []
    | M.null possibles = targetNext (- pi) targets
    | otherwise = (target:(targetNext angle' targets'))
    where possibles = possibleTargets angle targets
          ((targetAngle, targetRange), target) = firstTarget possibles
          targets' = M.delete (targetAngle, targetRange) targets
          angle' = targetAngle


successfulParse :: String -> (Asteroids, Bounds)
successfulParse input = ( S.fromList [(V2 x y) | x <- [0..maxX], y <- [0..maxY]
                                               , isAsteroid x y
                                               ]
                        , (maxX, maxY)
                        )
    where grid = lines input
          maxX = (length $ head grid) - 1
          maxY = (length grid) - 1
          isAsteroid x y = (grid!!y)!!x == '#'


showPattern (maxX, maxY) asteroids = unlines rows
    where rows = [[cell x y | x <- [0..maxX]] | y <- [0..maxY] ]
          cell x y = if S.member (V2 x y) asteroids then '#' else '.'

          
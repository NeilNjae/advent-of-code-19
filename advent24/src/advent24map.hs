-- import Debug.Trace


import Data.Finite (Finite, modulo, getFinite)
import GHC.TypeNats (KnownNat)


-- import Data.Functor.Compose (Compose(..))
-- import Data.Matrix (Matrix, matrix, safeGet, (!), prettyMatrix, mapPos, fromList, toList)
-- import qualified Data.Matrix as X
import Data.Bool (bool)
import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Functor.Identity (Identity(..))
import Control.Comonad.Representable.Store (Store(..), StoreT(..), store, experiment, runStore)
import Control.Comonad (Comonad(..))

import Data.Maybe
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)


data Coord = Ongrid Int Int | Offgrid deriving (Show, Eq, Ord)
data Grid a = Grid a (M.Map Coord a) deriving (Show, Eq, Ord)
type StoredGrid = Store Grid Bool 
type Rule = StoredGrid -> Bool

type GridCache = S.Set (Grid Bool)


instance Functor Grid where
  fmap f (Grid a m) = Grid (f a) (fmap f m)

instance Distributive Grid where
  distribute = distributeRep

instance Representable Grid where
  type Rep Grid = Coord
  index (Grid a m) Offgrid = a
  index (Grid a m) here = M.findWithDefault a here m
  tabulate f = 
    Grid (f Offgrid)
      (M.union (M.singleton c (f c))
                (M.unions (fmap (mapOfGrid . tabulate) 
                               (fmap (f . ) (fmap addCoords neighbourCoords)))
                               -- (fmap (f . addCoords . ) neighbourCoords))
                )
      )
    where
      c = (Ongrid 1 1)
      mapOfGrid (Grid _ m) = m

gridSize :: Int
gridSize = 5

-- validCoord :: Coord -> Bool
-- validCoord (r, c) = r >= 1 && r <= gridSize && c >= 1 && c <= gridSize

boundCoord :: Coord -> Coord
boundCoord Offgrid = Offgrid
boundCoord (Ongrid r c)
  | r >= 1 && r <= gridSize && c >= 1 && c <= gridSize = Ongrid r c
  | otherwise = Offgrid

neighbourCoords :: [Coord]
-- neighbourCoords = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]
neighbourCoords = [(Ongrid -1 0), (Ongrid 1 0), (Ongrid 0 -1), (Ongrid 0 1)]

addCoords :: Coord -> Coord -> Coord
addCoords Offgrid _ = Offgrid
addCoords _ Offgrid = Offgrid
addCoords (Ongrid x y) (Ongrid x' y') = boundCoord $ Ongrid (x + x') (y + y')

basicRule :: Rule
basicRule g = (alive && numNeighboursAlive == 1) || ((not alive) && (numNeighboursAlive == 1 || numNeighboursAlive == 2))
  where
    alive = extract g
    neighbours = experiment ((map boundCoord) . (at neighbourCoords)) g
    numNeighboursAlive = length (filter id neighbours)

step :: Rule -> StoredGrid -> StoredGrid
step = extend

render :: StoredGrid -> String
-- render (StoreT (Identity g) _) = foldMap ((++ "\n") . foldMap (bool "." "#")) g
-- render grid = X.prettyMatrix $ X.mapPos (\_ c -> bool "." "#" c) g
--     where g = unGrid grid
render grid = show $ unGrid grid


mkGrid :: [Coord] -> StoredGrid
mkGrid xs = store (`elem` xs) (Ongrid 1 1)

unGrid :: StoredGrid -> Grid Bool
-- unGrid (StoreT (Identity g) _) = g
unGrid grid = Grid False $ M.fromList gridList
    where (sgf, _sgl) = runStore grid
          gridList = [((Ongrid r c), sgf (Ongrid r c)) | c <- [1..gridSize], r <- [1..gridSize]]


at :: [Coord] -> Coord -> [Coord]
coords `at` origin = map (addCoords origin) coords

-- glider, blinker, beacon :: [Coord]
-- glider = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
-- blinker = [(0, 0), (1, 0), (2, 0)]
-- beacon = [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]


tickTime :: Int
tickTime = 200000

start :: IO StoredGrid
start = do coords <- readGrid
           return $ mkGrid coords
  --    glider `at` (1, 1)
  -- ++ beacon `at` (15, 5)

main :: IO ()
main = 
    do sG <- start
       -- print $ part1 sG
       let grids = map unGrid $ iterate (step basicRule) sG
       forM_ (take 5 $ iterate (step basicRule) sG) $ \grid -> do
            putStr "\ESC[2J" -- Clear terminal screen
            putStrLn (render grid)
            threadDelay tickTime


readGrid = 
    do  gs <- readFile "data/advent24.txt"
        let grid = lines gs
        let isBug r c = (grid!!r)!!c == '#'
        let ng = gridSize - 1
        return [Ongrid (r + 1) (c + 1) | r <- [0..ng], c <- [0..ng], isBug r c]


-- part1 :: Grid -> [Grid]
part1 :: StoredGrid -> Integer
-- part1 startingGrid = map fst $ takeWhile (uncurry . S.notMember) (zip grids gridCache)
-- part1 startingGrid = map fst $ takeWhile (\(g, c) -> S.notMember g c) (zip grids gridCache)
-- part1 startingGrid = fst $ head $ dropWhile (\(g, c) -> S.notMember g c) (zip grids gridCache)
part1 startingGrid = bioDiversity firstRepeat
    where 
          -- grids = map unGrid $ iterate (step basicRule) startingGrid
          -- gridCache = scanl' (flip . S.insert) S.empty grids
          grids = fGrids startingGrid
          gridCache = fGridCache grids
          firstRepeat = fst $ head $ dropWhile (uncurry S.notMember) (zip grids gridCache)

fGrids :: StoredGrid -> [Grid Bool]
fGrids stG = map unGrid $ iterate (step basicRule) stG

fGridCache :: [Grid Bool] -> [S.Set (Grid Bool)]
fGridCache gs = scanl' (flip S.insert) S.empty gs
-- fGridCache gs = scanl' (\s g -> S.insert g s) S.empty gs


bioDiversity :: (Grid Bool) -> Integer
bioDiversity (Grid _ g) = sum $ map snd $ filter (id . fst) $ zip bugs $ iterate ( * 2) 1
    where bugs = [ M.findWithDefault False (Ongrid r c) g 
                 | c <- [1..gridSize]
                 , r <- [1..gridSize] 
                 ]

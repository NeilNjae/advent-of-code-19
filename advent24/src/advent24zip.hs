{-# language DeriveFunctor #-}
{-# language TypeFamilies #-}
{-# language InstanceSigs #-}


-- import Debug.Trace


import Data.Finite (Finite, modulo, getFinite)
import GHC.TypeNats (KnownNat)


-- import Data.Functor.Compose (Compose(..))
-- import Data.Matrix (Matrix, matrix, safeGet, (!), prettyMatrix, mapPos, fromList, toList)
import qualified Data.Matrix as X
import Data.Bool (bool)
import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Functor.Identity (Identity(..))
import Control.Comonad.Representable.Store (Store(..), StoreT(..), store, experiment, runStore)
import Control.Comonad (Comonad(..))

import Data.Maybe
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)

import Control.Comonad
import Control.Comonad.Cofree
import Data.Distributive
import Data.Functor.Rep
import qualified Data.Sequence as S
import qualified Data.List.NonEmpty as NE


instance Ord Grid where
    m1 `compare` m2 = (X.toList m1) `compare` (X.toList m2)


type Coord = (Int, Int)
type Grid = X.Matrix Bool
type StoredGrid = Store X.Matrix Bool
type Rule = StoredGrid -> Bool

type GridCache = S.Set Grid

-- mGet :: Coord -> Matrix a -> a
-- mGet (r, c) mtx = fromMaybe False $ safeGet r c mtx
-- mGet rc mtx = mtx ! rc


validCoord :: Coord -> Bool
validCoord (r, c) = r >= 1 && r <= gridSize && c >= 1 && c <= gridSize


instance Distributive X.Matrix where
  distribute = distributeRep

instance Representable X.Matrix where
  type Rep X.Matrix = Coord
  index m c = (X.!) m c -- mGet c m
  tabulate = X.matrix gridSize gridSize

gridSize :: Int
gridSize = 5


neighbourCoords :: [Coord]
-- neighbourCoords = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]
neighbourCoords = [(-1, 0), (1, 0), (0, -1), (0, 1)]

addCoords :: Coord -> Coord -> Coord
addCoords (x, y) (x', y') = (x + x', y + y')

basicRule :: Rule
basicRule g = (alive && numNeighboursAlive == 1) || ((not alive) && (numNeighboursAlive == 1 || numNeighboursAlive == 2))
  where
    alive = extract g
    neighbours = experiment ((filter validCoord) . (at neighbourCoords)) g
    numNeighboursAlive = length (filter id neighbours)

step :: Rule -> StoredGrid -> StoredGrid
step = extend

render :: StoredGrid -> String
-- render (StoreT (Identity g) _) = foldMap ((++ "\n") . foldMap (bool "." "#")) g
render grid = X.prettyMatrix $ X.mapPos (\_ c -> bool "." "#" c) g
    where g = unGrid grid


mkGrid :: [Coord] -> StoredGrid
mkGrid xs = store (`elem` xs) (1, 1)

unGrid :: StoredGrid -> Grid
-- unGrid (StoreT (Identity g) _) = g
unGrid grid = X.fromList gridSize gridSize gridList
    where (sgf, _sgl) = runStore grid
          gridList = [sgf (r, c) | r <- [1..gridSize], c <- [1..gridSize]]


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
       print $ part1 sG
       -- let grids = map unGrid $ iterate (step basicRule) sG
       -- forM_ (take 5 $ iterate (step basicRule) sG) $ \grid -> do
       --      -- putStr "\ESC[2J" -- Clear terminal screen
       --      putStrLn (render grid)
       --      -- threadDelay tickTime


readGrid = 
    do  gs <- readFile "data/advent24.txt"
        let grid = lines gs
        let isBug r c = (grid!!r)!!c == '#'
        let ng = gridSize - 1
        return [(r + 1, c + 1) | r <- [0..ng], c <- [0..ng], isBug r c]


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

fGrids :: StoredGrid -> [Grid]
fGrids stG = map unGrid $ iterate (step basicRule) stG

fGridCache :: [Grid] -> [S.Set Grid]
fGridCache gs = scanl' (flip S.insert) S.empty gs
-- fGridCache gs = scanl' (\s g -> S.insert g s) S.empty gs


bioDiversity :: Grid -> Integer
bioDiversity g = sum $ map snd $ filter (id . fst) $ zip bugs $ iterate ( * 2) 1
    where bugs = X.toList g

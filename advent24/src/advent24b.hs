import Debug.Trace

import qualified Data.Set as S

data Cell = Cell { level :: Int
                 , row :: Int
                 , column :: Int
                 } deriving (Show, Eq, Ord)
type Grid = S.Set Cell


gridSize = 5

main :: IO ()
main = 
    do grid0 <- readGrid
       print grid0
       let finalGrid = head $ drop 200 $ iterate update grid0
       print $ S.size finalGrid


readGrid = 
    do  gs <- readFile "data/advent24.txt"
        let grid = lines gs
        let isBug r c = (grid!!(r - 1))!!(c - 1) == '#'
        let level = 0
        return $ S.fromList [Cell {..} | row <- [1..gridSize], column <- [1..gridSize], isBug row column]

neighbourSpaces :: Cell -> Grid
neighbourSpaces cell = 
     (  (neighbourSpacesLeft cell)
     <> (neighbourSpacesRight cell)
     <> (neighbourSpacesAbove cell)
     <> (neighbourSpacesBelow cell)
     )

neighbourSpacesLeft :: Cell -> Grid
neighbourSpacesLeft (Cell {..}) 
  | column == 4 && row == 3 = S.fromList  [ Cell { level = (level + 1), row = r, column = 5} | r <- [1..gridSize] ]
  | column == 1             = S.singleton ( Cell { level = (level - 1), row = 3, column = 2})
  | otherwise               = S.singleton ( Cell { level, row, column = (column - 1)})

neighbourSpacesRight :: Cell -> Grid
neighbourSpacesRight (Cell {..}) 
  | column == 2 && row == 3 = S.fromList  [ Cell { level = (level + 1), row = r, column = 1} | r <- [1..gridSize] ]
  | column == 5             = S.singleton ( Cell { level = (level - 1), row = 3, column = 4})
  | otherwise               = S.singleton ( Cell { level, row, column = (column + 1)})

neighbourSpacesAbove :: Cell -> Grid
neighbourSpacesAbove (Cell {..}) 
  | row == 4 && column == 3 = S.fromList  [ Cell { level = (level + 1), row = 5, column = c} | c <- [1..gridSize] ]
  | row == 1                = S.singleton ( Cell { level = (level - 1), row = 2, column = 3})
  | otherwise               = S.singleton ( Cell { level, row = (row - 1), column})

neighbourSpacesBelow :: Cell -> Grid
neighbourSpacesBelow (Cell {..}) 
  | row == 2 && column == 3 = S.fromList  [ Cell { level = (level + 1), row = 1, column = c} | c <- [1..gridSize] ]
  | row == 5                = S.singleton ( Cell { level = (level - 1), row = 4, column = 3})
  | otherwise               = S.singleton ( Cell { level, row = (row + 1), column})


countOccupiedNeighbours :: Cell -> Grid -> Int
countOccupiedNeighbours cell grid = S.size $ S.intersection grid $ neighbourSpaces cell

bugSurvives :: Grid -> Cell -> Bool
bugSurvives grid cell = alive && oneNeighbour
  where alive = cell `S.member` grid
        oneNeighbour = (countOccupiedNeighbours cell grid) == 1

bugBorn :: Grid -> Cell -> Bool
bugBorn grid cell = dead && (nNbrs == 1 || nNbrs == 2)
  where dead = cell `S.notMember` grid
        nNbrs = countOccupiedNeighbours cell grid

update :: Grid -> Grid
update grid = S.union (S.filter (bugSurvives grid) bugs) (S.filter (bugBorn grid) empties)
  where bugs = grid
        empties = (S.foldr mergeEmpties S.empty grid) `S.difference` bugs
        mergeEmpties cell acc = S.union acc $ neighbourSpaces cell


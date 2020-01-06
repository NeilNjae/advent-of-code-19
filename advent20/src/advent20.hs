import Debug.Trace

-- import qualified Data.Text.IO as TIO

-- import qualified Data.Map.Strict as M
-- import Data.Map.Strict ((!))
import qualified Data.PQueue.Prio.Min as P
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Data.Sequence ((<|)) -- , (|>), (><))
import Data.Foldable (foldl', sum) -- (toList, foldr', foldl', all)
import Data.Char
import Control.Monad.Reader
import Control.Lens hiding ((<|), (|>))
import Data.Maybe (fromMaybe)



type Position = (Int, Int) -- r, c


data Location = Inner | Outer deriving (Eq, Ord, Show)
data Portal = Portal { _label :: String
                     , _position :: Position
                     , _location :: Location
                     } deriving (Eq, Ord, Show)
makeLenses ''Portal

type Portals = S.Set Portal


type ExpandedMaze = S.Set Position
data MazeComplex = MazeComplex 
    { _mazeE :: ExpandedMaze
    , _portalsE :: Portals
    , _portalLocs :: S.Set Position
    } deriving (Eq, Ord, Show)
makeLenses ''MazeComplex

type EdgeConnects = (Portal, Portal)
data EdgeType = Walk | Teleport deriving (Eq, Ord, Show)

data Edge = Edge { _connects :: EdgeConnects
                 , _edgeType :: EdgeType
                 , _distance :: Int
                 } deriving (Eq, Ord, Show)
makeLenses ''Edge   

type Edges = S.Set Edge

-- type Maze = S.Set Edge
data Maze = Maze { _maze :: Edges
                 , _costPerLevel :: Int
                 , _costToFinish :: Int
                 } deriving (Eq, Ord, Show)
-- makeLenses ''Maze                

type MazeContext = Reader Maze

class (Eq s, Ord s, Show s) => SearchState s where
    successors :: s -> MazeContext (Q.Seq (s, Edge))
    estimateCost :: s -> MazeContext Int
    emptySearchState :: Portal -> s
    isGoal :: s -> MazeContext Bool

data LevelledSearchState = LevelledSearchState
    { _portalS :: Portal
    , _levelS :: Int
    } deriving (Eq, Ord, Show)
makeLenses ''LevelledSearchState    


type ExploredStates s = S.Set s

data Agendum s = 
    Agendum { _current :: s
            , _trail :: Q.Seq Edge
            , _cost :: Int
            } deriving (Show, Eq)
makeLenses ''Agendum                       

type Agenda s = P.MinPQueue Int (Agendum s)


main :: IO ()
main = do 
        maze <- setup
        -- print maze
        putStrLn $ showContracted maze
        print $ part1 maze
        print $ part2 maze


setup = do
        text <- readFile "data/advent20.txt"
        let mc = buildComplex text
        -- print mc
        -- print maze
        return $ contractMaze mc
        

part1 :: Maze -> Int
-- part1 :: Maze -> Maybe (Agendum Portal)
part1 maze = maybe 0 _cost result
    where result = runReader searchMaze maze :: Maybe (Agendum Portal)

part2 :: Maze -> Int
-- part2 :: Maze -> Maybe (Agendum LevelledSearchState)
part2 maze = maybe 0 _cost result
    where result = runReader searchMaze maze :: Maybe (Agendum LevelledSearchState)


buildComplex :: String -> MazeComplex
buildComplex text = mc & portalLocs .~ pLocs & portalsE .~ portals'
    where mc = foldl' (buildMazeRow rows) mc0 [0..l]
          mc0 = MazeComplex {_mazeE = S.empty, _portalsE = S.empty, _portalLocs = S.empty}
          rows = lines text
          l = length rows - 1
          minR = 2
          maxR = l - 2
          minC = 2
          maxC = length (rows!!2) - 3
          pLocs = S.map _position (mc ^. portalsE)
          portals = mc ^. portalsE
          portals' = S.map (classifiyPortal minR maxR minC maxC) portals

classifiyPortal :: Int -> Int -> Int -> Int -> Portal -> Portal
classifiyPortal minR maxR minC maxC portal = portal & location .~ loc
    where (r, c) = portal ^. position
          loc = if (r == minR) || (r == maxR) || (c == minC) || (c == maxC)
                then Outer
                else Inner

buildMazeRow :: [String] -> MazeComplex -> Int -> MazeComplex
buildMazeRow rows mc r = foldl' (buildMazeCell rows r) mc [0..l]
    where l = length (rows!!r) - 1


buildMazeCell :: [String] -> Int -> MazeComplex -> Int -> MazeComplex
buildMazeCell rows r mc c
    | char == '.' = mc'
    | isUpper char = mc & portalsE .~ portals'
    | otherwise = mc
    where char = (rows!!r)!!c
          mc' = mc & mazeE %~ (S.insert here)
          here = (r, c)
          portals' = makePortal (mc ^. portalsE) rows char r c


makePortal portals rows hc r c 
    | isUpper rc = if pr == '.'
                   then S.insert (Portal { _label = [hc, rc], _position = (r, c + 2), _location = Outer } ) portals
                   else S.insert (Portal { _label = [hc, rc], _position = (r, c - 1), _location = Outer } ) portals
    | isUpper dc = if pd == '.'
                   then S.insert (Portal { _label = [hc, dc], _position = (r + 2, c), _location = Outer } ) portals
                   else S.insert (Portal { _label = [hc, dc], _position = (r - 1, c), _location = Outer } ) portals
    | otherwise = portals
    where -- lc = charAt rows r (c - 1)
          rc = charAt rows r (c + 1)
          -- uc = charAt rows (r - 1) c
          dc = charAt rows (r + 1) c
          -- pu = charAt rows (r - 1) c
          pd = charAt rows (r + 2) c
          -- pl = charAt rows r (c - 1)
          pr = charAt rows r (c + 2)


charAt :: [String] -> Int -> Int -> Char
charAt rows r c = atDef ' ' (atDef "" rows r) c

atDef :: a -> [a] -> Int -> a
atDef x xs i = fromMaybe x $ atMaybe xs i
-- atDef x = (fromMaybe x) . atMaybe

atMaybe :: [a] -> Int -> Maybe a
atMaybe xs i
    | i < 0 = Nothing
    | i >= (length xs) = Nothing
    | otherwise = Just (xs!!i)


contractMaze :: MazeComplex -> Maze
contractMaze expanded = Maze 
    { _maze = S.union mazeW mazeP
    , _costPerLevel = cpl
    , _costToFinish = ctf
    }
    where starts = expanded ^. portalsE
          mazeP = S.foldr (contractFrom expanded) S.empty starts
          mazeW = S.foldr (addWarp starts) S.empty starts
          cpl = minimum $ map (^. distance) $ S.toList $ S.filter (\e -> e ^. edgeType == Walk) mazeP
          ctf = minimum $ map (^. distance) $ S.toList $ S.filter (edgeTouches term) mazeP
          term = S.findMin $ S.filter (\p -> p ^. label == "ZZ") starts

contractFrom :: MazeComplex -> Portal -> Edges -> Edges
contractFrom expanded start maze0 = S.union maze0 reachables
    where startPos = start ^. position
          reachables = reachableFrom [(startPos, 0)] S.empty expanded' start
          expanded' = expanded & portalsE %~ (S.delete start)
                               & portalLocs %~ (S.delete startPos)
                               -- & mazeE %~ (S.delete startPos)

reachableFrom :: [(Position, Int)] -> (S.Set Position) -> MazeComplex -> Portal -> Edges
reachableFrom [] _closed _expanded _start = S.empty
reachableFrom ((here, dist):boundary) closed expanded start
    | here `S.member` closed = reachableFrom boundary closed expanded start
    | here `S.member` ps = S.insert edge $ reachableFrom boundary closed' expanded start
    | otherwise = reachableFrom boundary' closed' expanded start
    where nbrs0 = S.intersection (expanded ^. mazeE) $ possibleNeighbours here
          nbrs = S.difference nbrs0 closed
          closed' = S.insert here closed
          ps = expanded ^. portalLocs
          other = S.findMin $ S.filter (\p -> p ^. position == here) $ expanded ^. portalsE
          edge = Edge { _connects = mkConnection start other, _edgeType = Walk, _distance = dist }
          neighbours = S.map (\n -> (n, dist + 1)) nbrs
          boundary' = boundary ++ (S.toAscList neighbours)


addWarp :: Portals -> Portal -> Edges -> Edges
addWarp portals portal warps
    | S.null others = warps
    | otherwise = S.insert warp warps
    where others = S.filter (portalsConnect lab pos) portals
          lab = portal ^. label
          pos = portal ^. position
          other = S.findMin others
          warp = Edge {_connects = mkConnection portal other, _edgeType = Teleport, _distance = 1}

portalsConnect :: String -> Position -> Portal -> Bool
portalsConnect lab pos portal = (pLabel == lab) && (pPos /= pos)
    where pLabel = portal ^. label
          pPos = portal ^. position


mkConnection :: Portal -> Portal -> EdgeConnects
mkConnection a b = if a < b then (a, b) else (b, a)


edgeTouches :: Portal -> Edge -> Bool
edgeTouches p e
    | p == a = True
    | p == b = True
    | otherwise = False
    where (a, b) = e ^. connects

edgeOther :: Portal -> Edge -> Portal
edgeOther x e 
    | x == a = b
    | otherwise = a
    where (a, b) = e ^. connects

possibleNeighbours :: Position -> S.Set Position
possibleNeighbours (r, c) = S.fromList [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]     


mazePortals edges = S.foldr' mps S.empty edges
    where mps e ps = let (p1, p2) = e ^. connects
                     in S.insert p1 $ S.insert p2 ps


searchMaze ::  SearchState s => MazeContext (Maybe (Agendum s))
searchMaze = 
    do agenda <- initAgenda
       aStar agenda S.empty

initAgenda ::  SearchState s => MazeContext (Agenda s)
initAgenda = 
    do edges <- asks _maze
       let portals = mazePortals edges
       let portal = S.findMin $ S.filter (\p -> p ^. label == "AA") portals
       let ss = emptySearchState portal
       c <- estimateCost ss
       return $ P.singleton c Agendum { _current = ss, _trail = Q.empty, _cost = c}


aStar ::  SearchState s => Agenda s -> ExploredStates s -> MazeContext (Maybe (Agendum s))
aStar agenda closed 
    -- | trace ("Peeping " ++ (show $ fst $ P.findMin agenda) ++ ": " ++ (show reached) ++ " <- " ++ (show $ toList $ Q.take 1 $ _trail $ currentAgendum) ++ " :: " ++ (show newAgenda)) False = undefined
    -- | trace ("Peeping " ++ (show $ _current $ snd $ P.findMin agenda) ) False = undefined
    | P.null agenda = return Nothing
    | otherwise = 
        do  let (_, currentAgendum) = P.findMin agenda
            let reached = _current currentAgendum
            nexts <- candidates currentAgendum closed
            let newAgenda = foldl' (\q a -> P.insert (_cost a) a q) (P.deleteMin agenda) nexts
            reachedGoal <- isGoal reached
            if reachedGoal
            then return (Just currentAgendum)
            else if reached `S.member` closed
                 then aStar (P.deleteMin agenda) closed
                 else aStar newAgenda (S.insert reached closed)


candidates ::  SearchState s => Agendum s -> ExploredStates s -> MazeContext (Q.Seq (Agendum s))
candidates agendum closed = 
    do  let candidate = agendum ^. current
        let previous = agendum ^. trail
        -- let prevCost = agendum ^. cost
        succs <- successors candidate
        let nonloops = Q.filter (\s -> not $ (fst s) `S.member` closed) succs
        mapM (makeAgendum previous) nonloops

makeAgendum ::  SearchState s => (Q.Seq Edge) -> (s, Edge) -> MazeContext (Agendum s)
makeAgendum previous (newP, newE) = 
    do predicted <- estimateCost newP
       let newTrail = newE <| previous
       let incurred = sum $ fmap (^. distance) newTrail
       return Agendum { _current = newP
                      , _trail = newTrail
                      , _cost = incurred + predicted
                      }


instance SearchState Portal where

    emptySearchState portal = portal

    -- successors :: Portal -> MazeContext (Q.Seq (Portal, Edge))
    successors portal = 
        do maze <- asks _maze
           let edges = S.filter (edgeTouches portal) maze
           let locations = S.map (\e -> (edgeOther portal e, e)) edges
           let succs = S.foldr' (<|) Q.empty locations
           return succs

    -- estimateCost :: Portal -> MazeContext Int
    estimateCost _portal = return 0

    -- isGoal :: Portal -> MazeContext Bool
    isGoal portal = return $ portal ^. label == "ZZ"

instance SearchState LevelledSearchState where
    emptySearchState portal = LevelledSearchState {_portalS = portal, _levelS = 0}

    -- successors :: LevelledSearchState -> MazeContext (Q.Seq (LevelledSearchState, Edge))
    successors ss = 
        do maze <- asks _maze
           let lvl = ss ^. levelS
           let portal = ss ^. portalS
           let edges = S.filter (edgeTouches portal) maze
           let lvlEdges = S.filter (edgeAtLevel portal lvl) edges 
           let locations = S.map (\e -> (newLSS portal lvl e, e)) lvlEdges
           let locations' = S.filter (\(s, _) -> (s ^. levelS) >= 0) locations
           let succs = S.foldr' (<|) Q.empty locations'
           return succs

    -- estimateCost :: Portal -> MazeContext Int
    estimateCost ss = -- return 0
        do let lvl = ss ^. levelS
           cpl <- asks _costPerLevel
           ctf <- asks _costToFinish
           let cplT = if ss ^. portalS . location == Outer
                      then cpl * (lvl - 1) + 1
                      else cpl * lvl
           if isTerminal (ss ^. portalS)
           then return 0 
           else return (cplT + ctf)

    -- isGoal :: LevelledSearchState -> MazeContext Bool
    isGoal ss = return $ ss ^. portalS . label == "ZZ"

edgeAtLevel portal lvl edge 
    -- | (lvl == 0) && (isTerminal other) && (et == Walk) = True
    | (lvl /= 0) && (isTerminal other) && (et == Walk) = False
    | (lvl == 0) && (not $ isTerminal other) && (et == Walk) && ((other ^. location) == Outer) = False
    | otherwise = True
    where other = edgeOther portal edge
          et = edge ^. edgeType

isTerminal p = (p ^. label == "AA") || (p ^. label == "ZZ")

newLSS :: Portal -> Int -> Edge -> LevelledSearchState
newLSS portal lvl edge
    | et == Teleport && pl == Outer = LevelledSearchState { _portalS = otherPortal, _levelS = lvl - 1 }
    | et == Teleport && pl == Inner = LevelledSearchState { _portalS = otherPortal, _levelS = lvl + 1 }
    | otherwise = LevelledSearchState { _portalS = otherPortal, _levelS = lvl } -- et == Walk
    where pl = portal ^. location
          et = edge ^. edgeType
          otherPortal = edgeOther portal edge


showContracted :: Maze -> String
showContracted m = "graph Maze {\n" ++ bulk ++ "\n}"
    where   bulk = S.foldr (\e s -> (showEdge e) ++ s) "" (_maze m)

showEdge :: Edge -> String
showEdge e = (showPortal h) ++ " -- " ++ (showPortal t) ++ " [ label = \"" ++ edgeLabel ++ "\" style = \"" ++ style ++ "\"];\n"
    where -- edgeLabel e = (show (e ^. edgeType)) ++ ", " ++ (show (e ^. distance))
          (edgeLabel, style) = 
                if (e ^. edgeType) == Walk
                then (show (e ^. distance), "solid")
                else ("", "dashed")
          (h, t) = e ^. connects
          -- showPortal p = p ^. label ++ (show (fst (p ^. position))) ++ "c" ++ (show (snd (p ^. position))) ++ (take 1 $ show (p ^. location))
          showPortal p = p ^. label ++ "_" ++ (take 1 $ show (p ^. location))

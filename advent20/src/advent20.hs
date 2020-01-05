import Debug.Trace

-- import qualified Data.Text.IO as TIO

-- import qualified Data.Map.Strict as M
-- import Data.Map.Strict ((!))
import qualified Data.PQueue.Prio.Min as P
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Data.Sequence ((<|)) -- , (|>), (><))
import Data.Foldable (foldl', any, sum) -- (toList, foldr', foldl', all)
import Data.Char
import Control.Monad.Reader
import Control.Lens hiding ((<|), (|>))
import Data.Maybe (fromMaybe)



type Position = (Int, Int) -- r, c

data Portal = Portal { _label :: String
                     , _position :: Position
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

type Maze = S.Set Edge

type MazeContext = Reader Maze

type ExploredStates = S.Set Portal

data Agendum = Agendum { _current :: Portal
                       , _trail :: Q.Seq Edge
                       , _cost :: Int} deriving (Show, Eq)
makeLenses ''Agendum                       

type Agenda = P.MinPQueue Int (Agendum)


main :: IO ()
main = do 
        -- text <- readFile "data/advent20a.txt"
        -- let mc = buildComplex text
        -- print mc
        -- let maze = contractMaze mc
        -- print maze
        maze <- setup
        putStrLn $ showContracted maze
        print $ part1 maze
        -- print $ S.size $ edgeC $ _caveE ccE
        -- print $ S.size $ _cave $ contractCave ccE [startPosition]
        -- putStrLn $ showContracted $ contractCave ccE [startPosition]
        -- let (re, ce) = startPosition
        -- let startPositions = [(re - 1, ce - 1), (re - 1, ce + 1), (re + 1 , ce - 1), (re + 1, ce + 1)]
        -- let cavern0 = ccE ^. caveE
        -- let cavern = cavern0 `S.difference` [(re, ce), (re + 1, ce), (re - 1, ce), (re, ce + 1), (re, ce - 1)]
        -- let caveComplex = ccE & caveE .~ cavern
        -- let cc = contractCave caveComplex startPositions
        -- putStrLn $ showContracted cc 
        -- print $ part1 ccE startPosition
        -- print $ part2 ccE startPosition

-- edgeC ec = S.foldl' ecAdd S.empty ec
--     where ecAdd es n = S.union (eds n) es
--           eds n = S.map (\m -> S.fromList [n, m]) $ nbrs n
--           nbrs n = S.intersection ec $ possibleNeighbours n



setup = do
        text <- readFile "data/advent20.txt"
        let mc = buildComplex text
        -- print mc
        return $  contractMaze mc
        -- print maze
        

-- part1 :: Maze -> Int
part1 maze = result -- maybe 0 _cost result
    where result = runReader searchMaze maze



buildComplex :: String -> MazeComplex
buildComplex text = mc & portalLocs .~ pLocs
    where mc = foldl' (buildMazeRow rows) mc0 [0..l]
          mc0 = MazeComplex {_mazeE = S.empty, _portalsE = S.empty, _portalLocs = S.empty}
          rows = lines text
          l = length rows - 1
          pLocs = S.map _position (mc ^. portalsE)

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
                   then S.insert (Portal [hc, rc] (r, c + 2)) portals
                   else S.insert (Portal [hc, rc] (r, c - 1)) portals
    | isUpper dc = if pd == '.'
                   then S.insert (Portal [hc, dc] (r + 2, c)) portals
                   else S.insert (Portal [hc, dc] (r - 1, c)) portals
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
contractMaze expanded = S.union mazeW mazeP
    where starts = expanded ^. portalsE
          mazeW = S.foldr (contractFrom expanded) S.empty starts
          mazeP = S.foldr (addWarp starts) S.empty starts


contractFrom :: MazeComplex -> Portal -> Maze -> Maze
contractFrom expanded start maze0 = S.union maze0 reachables
    where startPos = start ^. position
          reachables = reachableFrom [(startPos, 0)] S.empty expanded' start
          expanded' = expanded & portalsE %~ (S.delete start)
                               & portalLocs %~ (S.delete startPos)
                               -- & mazeE %~ (S.delete startPos)

reachableFrom :: [(Position, Int)] -> (S.Set Position) -> MazeComplex -> Portal -> Maze
reachableFrom [] _closed _expanded _start = S.empty
reachableFrom ((here, distance):boundary) closed expanded start
    | here `S.member` closed = reachableFrom boundary closed expanded start
    | here `S.member` ps = S.insert edge $ reachableFrom boundary closed' expanded start
    | otherwise = reachableFrom boundary' closed' expanded start
    where nbrs0 = S.intersection (expanded ^. mazeE) $ possibleNeighbours here
          nbrs = S.difference nbrs0 closed
          closed' = S.insert here closed
          ps = expanded ^. portalLocs
          other = S.findMin $ S.filter (\p -> p ^. position == here) $ expanded ^. portalsE
          edge = Edge { _connects = mkConnection start other, _edgeType = Walk, _distance = distance }
          neighbours = S.map (\n -> (n, distance + 1)) nbrs
          boundary' = boundary ++ (S.toAscList neighbours)


addWarp :: Portals -> Portal -> Maze -> Maze
addWarp portals portal warps
    | S.null others = warps
    | otherwise = S.insert warp warps
    where others = S.filter (portalsConnect lab pos) portals
          lab = portal ^. label
          pos = portal ^. position
          other = S.findMin others
          warp = Edge {_connects = mkConnection portal other, _edgeType = Teleport, _distance = 1}


portalsConnect :: String -> Position -> Portal -> Bool
portalsConnect lab pos portal = (pLabel == lab) && (not (pPos == pos))
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

-- anyEdgeTouch :: S.Set Portal -> Edge -> Bool
-- -- anyEdgeTouch xs e = S.foldl' (\t x -> t || (edgeTouches e x)) False xs
-- anyEdgeTouch xs e = any (edgeTouches e) xs

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



searchMaze ::  MazeContext (Maybe (Agendum))
searchMaze = 
    do agenda <- initAgenda
       aStar agenda S.empty

initAgenda ::  MazeContext (Agenda)
initAgenda = 
    do edges <- ask
       let portals = mazePortals edges
       let portal = S.findMin $ S.filter (\p -> p ^. label == "AA") portals
       cost <- estimateCost portal
       return $ P.singleton cost Agendum { _current = portal, _trail = Q.empty, _cost = cost}


aStar ::  Agenda -> ExploredStates -> MazeContext (Maybe (Agendum))
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


isGoal :: Portal -> MazeContext Bool
isGoal portal = return $ portal ^. label == "ZZ"

candidates ::  Agendum -> ExploredStates -> MazeContext (Q.Seq (Agendum))
candidates agendum closed = 
    do  let candidate = agendum ^. current
        let previous = agendum ^. trail
        succs <- successors candidate
        let nonloops = Q.filter (\s -> not $ (fst s) `S.member` closed) succs
        mapM (makeAgendum previous) nonloops

makeAgendum ::  (Q.Seq Edge) -> (Portal, Edge) -> MazeContext (Agendum)
makeAgendum previous (newP, newE) = 
    do predicted <- estimateCost newP
       let incurred = (newE ^. distance) + (sum $ fmap (^. distance) previous)
       return Agendum { _current = newP
                      , _trail = newE <| previous
                      , _cost = incurred + predicted
                      }

successors :: Portal -> MazeContext (Q.Seq (Portal, Edge))
successors portal = 
    do maze <- ask
       let edges = S.filter (edgeTouches portal) maze
       let locations = S.map (\e -> (edgeOther portal e, e)) edges
       let succs = S.foldr' (<|) Q.empty locations
       return succs

estimateCost :: Portal -> MazeContext Int
estimateCost portal = return 0
    -- do let heres = explorer ^. position
    --    ks <- asks _keys
    --    cavern <- asks _cave
    --    let kH = explorer ^. keysHeld
    --    let unfound = ks `S.difference` kH
    --    let unfoundEdges0 = S.filter (\e -> edgeTouch heres e) cavern
    --    let unfoundEdges = S.filter (\e -> not $ anyEdgeTouch kH e) unfoundEdges0
    --    let furthest = S.findMax $ S.insert 0 $ S.map _distance unfoundEdges
    --    return $ max 0 $ furthest + (S.size unfound) - 1






showContracted :: Maze -> String
showContracted maze = "graph Maze {\n" ++ bulk ++ "\n}"
    where   bulk = S.foldr (\e s -> (showEdge e) ++ s) "" maze

showEdge :: Edge -> String
showEdge e = (showPortal h) ++ " -- " ++ (showPortal t) ++ " [ label = \"" ++ (edgeLabel e) ++ "\"];\n"
    where edgeLabel e = (show (e ^. edgeType)) ++ ", " ++ (show (e ^. distance))
          (h, t) = e ^. connects
          showPortal p = p ^. label ++ (show (fst (p ^. position))) ++ "c" ++ (show (snd (p ^. position)))

import Debug.Trace

-- import qualified Data.Text.IO as TIO

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import qualified Data.PQueue.Prio.Min as P
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Data.Sequence ((<|)) -- , (|>), (><))
import Data.Foldable (foldl') -- (toList, foldr', foldl', all)
import Data.Char
import Control.Monad.Reader
import Control.Lens hiding ((<|), (|>))


type Position = (Integer, Integer) -- r, c

type Keys = S.Set Char
type PointOfInterest = M.Map Position Char

data Explorer = Explorer { _position :: S.Set Char
                           , _keysHeld :: Keys
                           , _travelled :: Int
                           } deriving (Show)
makeLenses ''Explorer

instance Eq Explorer where
    e1 == e2 = (_position e1 == _position e2) && (_keysHeld e1 == _keysHeld e2)

instance Ord Explorer where
    e1 `compare` e2 =
        if _position e1 == _position e2
        then (_keysHeld e1) `compare` (_keysHeld e2)
        else (_position e1) `compare` (_position e2)

type ExploredStates = S.Set Explorer

type ExpandedCave = S.Set Position
data ExpandedCaveComplex = ExpandedCaveComplex { _caveE :: ExpandedCave
                               , _keysE :: PointOfInterest
                               , _doors :: PointOfInterest
                               } deriving (Eq, Ord, Show)
makeLenses ''ExpandedCaveComplex

type Connection = (Char, Char)
data CaveEdge = CaveEdge { _connections :: Connection
                         , _keysRequired :: S.Set Char
                         , _distance :: Int
                         } deriving (Eq, Ord, Show)
makeLenses ''CaveEdge   

type Cave = S.Set CaveEdge

data CaveComplex = CaveComplex { _cave :: Cave
                               , _keys :: S.Set Char
                               } deriving (Eq, Ord, Show)
makeLenses ''CaveComplex

type CaveContext = Reader CaveComplex

data Agendum = Agendum { _current :: Explorer
                       , _trail :: Q.Seq Explorer
                       , _cost :: Int} deriving (Show, Eq)
type Agenda = P.MinPQueue Int (Agendum)


main :: IO ()
main = do 
        text <- readFile "data/advent18.txt"
        let (ccE, startPosition) = buildCaveComplex text
        -- print ccE
        -- print $ contractCave ccE [startPosition]
        print $ part1 ccE startPosition
        print $ part2 ccE startPosition


part1 :: ExpandedCaveComplex -> Position -> Int
part1 cavern startPosition = maybe 0 _cost result
    where cc = contractCave cavern [startPosition]
          explorer = emptyExplorer ['0']
          result = runReader (searchCave explorer) cc

part2 ::  ExpandedCaveComplex -> Position -> Int
part2 caveComplex0 (re, ce) = maybe 0 _cost result
    where 
        startPositions = [(re - 1, ce - 1), (re - 1, ce + 1), (re + 1 , ce - 1), (re + 1, ce + 1)]
        cavern0 = caveComplex0 ^. caveE
        cavern = cavern0 `S.difference` [(re, ce), (re + 1, ce), (re - 1, ce), (re, ce + 1), (re, ce - 1)]
        caveComplex = caveComplex0 {_caveE = cavern}
        cc = contractCave caveComplex startPositions
        explorer = emptyExplorer $ S.fromList "0123"
        result = runReader (searchCave explorer) cc


buildCaveComplex :: String -> (ExpandedCaveComplex, Position)
buildCaveComplex text = (ccE, startPosition)
    where (ccE, startPosition) = foldl' buildCaveRow (cc0, (0, 0)) $ zip [0..] rows
          cc0 = ExpandedCaveComplex {_caveE = S.empty, _keysE = M.empty, _doors = M.empty}
          rows = lines text

buildCaveRow :: (ExpandedCaveComplex, Position) -> (Integer, String) -> (ExpandedCaveComplex, Position)
buildCaveRow (cc, explorers) (r, row) = foldl' (buildCaveCell r) (cc, explorers) $ zip [0..] row


buildCaveCell :: Integer -> (ExpandedCaveComplex, Position) -> (Integer, Char) -> (ExpandedCaveComplex, Position)
buildCaveCell r (cc, startPosition) (c, char) 
    | char == '.' = (cc', startPosition)
    | char == '@' = (cc', here)
    | isLower char = (cc' & keysE %~ (M.insert here char), startPosition) -- (cc' { _keysE = M.insert here char $ _keysE cc'}, startPosition)
    | isUpper char = (cc' & doors %~ (M.insert here char), startPosition) 
    | otherwise = (cc, startPosition)
    where cc' = cc & caveE %~ (S.insert here)
          here = (r, c)


mkConnection :: Char -> Char -> Connection
mkConnection a b = if a < b then (a, b) else (b, a)

edgeTouches :: Char -> CaveEdge -> Bool
edgeTouches x e
    | x == a = True
    | x == b = True
    | otherwise = False
    where (a, b) = e ^. connections

anyEdgeTouch :: Keys -> CaveEdge -> Bool
anyEdgeTouch xs e = S.foldl' (\t x -> t || (edgeTouches x e)) False xs

edgeOther :: Char -> CaveEdge -> Char
edgeOther x e 
    | x == a = b
    | otherwise = a
    where (a, b) = e ^. connections



contractCave :: ExpandedCaveComplex -> [Position] -> CaveComplex
contractCave expanded startPositions = cavern
    where explorers = M.fromList $ zip startPositions $ map intToDigit [0..]
          starts = M.union explorers (expanded ^. keysE)
          cavern0 = CaveComplex {_cave = S.empty, _keys = S.fromList $ M.elems (expanded ^. keysE)}
          cavern = M.foldrWithKey (contractFrom expanded) cavern0 starts

contractFrom :: ExpandedCaveComplex -> Position -> Char -> CaveComplex -> CaveComplex
contractFrom expanded startPos startKey cc = cc { _cave = S.union (_cave cc) reachables }
    where reachables = reachableFrom [(startPos, edge0)] S.empty expanded' startKey
          edge0 = CaveEdge {_connections = ('0', '0'), _keysRequired = S.empty, _distance = 0}
          expanded' = expanded & keysE %~ (M.delete startPos)

reachableFrom :: [(Position, CaveEdge)] -> (S.Set Position) -> ExpandedCaveComplex -> Char -> Cave
reachableFrom [] _closed _expanded _startKey = S.empty
reachableFrom ((here, edge):boundary) closed expanded startKey
    | here `S.member` closed = reachableFrom boundary closed expanded startKey
    | here `M.member` ks = S.insert edgeK $ reachableFrom boundary closed' expanded startKey
    | here `M.member` drs = reachableFrom boundaryD closed' expanded startKey
    | otherwise = reachableFrom boundary' closed' expanded startKey
    where nbrs0 = S.intersection (expanded ^. caveE) $ possibleNeighbours here
          nbrs = S.difference nbrs0 closed
          closed' = S.insert here closed
          ks = expanded ^. keysE
          drs = expanded ^. doors
          edge' = edge & distance %~ (+1)
          edgeK = edge & connections .~ (mkConnection startKey (ks!here))
          edgeD = edge' & keysRequired %~ (S.insert (toLower (drs!here)))
          neighbours = S.map (\n -> (n, edge')) nbrs
          neighboursD = S.map (\n -> (n, edgeD)) nbrs
          boundary' = boundary ++ (S.toAscList neighbours)
          boundaryD = boundary ++ (S.toAscList neighboursD)

possibleNeighbours :: Position -> S.Set Position
possibleNeighbours (r, c) = [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]     


searchCave ::  Explorer -> CaveContext (Maybe (Agendum))
searchCave explorer = 
    do agenda <- initAgenda explorer
       aStar agenda S.empty

initAgenda ::  Explorer -> CaveContext (Agenda)
initAgenda explorer = 
    do cost <- estimateCost explorer
       return $ P.singleton cost Agendum { _current = explorer, _trail = Q.empty, _cost = cost}


aStar ::  Agenda -> ExploredStates -> CaveContext (Maybe (Agendum))
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


isGoal ::  Explorer -> CaveContext Bool
isGoal explorer = 
    do ks <- asks _keys
       return $ ks == (explorer ^. keysHeld)


candidates ::  Agendum -> ExploredStates -> CaveContext (Q.Seq (Agendum))
candidates agendum closed = 
    do  let candidate = _current agendum
        let previous = _trail agendum
        succs <- successors candidate
        let nonloops = Q.filter (\s -> not $ s `S.member` closed) succs
        mapM (makeAgendum candidate previous) nonloops

makeAgendum ::  Explorer -> (Q.Seq Explorer) -> Explorer -> CaveContext (Agendum)
makeAgendum candidate previous new = 
    do predicted <- estimateCost new
       return Agendum { _current = new
                      , _trail = candidate <| previous
                      , _cost = (new ^. travelled) + predicted
                      }

successors :: Explorer -> CaveContext (Q.Seq Explorer)
successors explorer = 
    do let heres = explorer ^. position
       cavern <- asks _cave
       let kH = explorer ^. keysHeld
       let locations0 = S.filter (\e -> anyEdgeTouch heres e) cavern
       let locations1 = S.filter (\e -> S.null ((e ^. keysRequired) `S.difference` kH)) locations0
       let succs = S.foldr' (\e q -> (extendExplorer explorer e) <| q) Q.empty locations1
       return succs

estimateCost :: Explorer -> CaveContext Int
estimateCost explorer = -- return 0
    do let heres = explorer ^. position
       ks <- asks _keys
       cavern <- asks _cave
       let kH = explorer ^. keysHeld
       let unfound = ks `S.difference` kH
       let unfoundEdges0 = S.filter (\e -> anyEdgeTouch heres e) cavern
       let unfoundEdges = S.filter (\e -> not $ anyEdgeTouch kH e) unfoundEdges0
       let furthest = S.findMax $ S.insert 0 $ S.map _distance unfoundEdges
       return $ max 0 $ furthest + (S.size unfound) - 1

emptyExplorer :: S.Set Char -> Explorer
emptyExplorer ps = Explorer { _position = ps, _keysHeld = S.empty, _travelled = 0 }

extendExplorer :: Explorer -> CaveEdge -> Explorer
extendExplorer explorer edge = 
    explorer & position .~ pos'
             & keysHeld .~ kH'
             & travelled .~ d'
    where here = S.findMin $ S.filter (\p -> edgeTouches p edge) (explorer ^. position)
          there = edgeOther here edge
          kH' = S.insert there (explorer ^. keysHeld)
          d' = (explorer ^. travelled) + (edge ^. distance)
          pos' = S.insert there $ S.delete here (explorer ^. position)


showContracted cc = "graph Cave {\n" ++ bulk ++ "\n}"
    where   cavern = cc ^. cave
            bulk = S.foldr (\e s -> (showEdge e) ++ s) "" cavern

showEdge e = (show h) ++ " -- " ++ (show t) ++ " [ label = \"" ++ (edgeLabel e) ++ "\"];\n"
    where edgeLabel e = (S.toList (e ^. keysRequired)) ++ ", " ++ (show (e ^. distance))
          (h, t) = e ^. connections

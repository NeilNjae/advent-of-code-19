import Debug.Trace

-- import qualified Data.Text.IO as TIO

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import qualified Data.PQueue.Prio.Min as P
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Data.Sequence ((<|), (|>), (><))
import Data.Foldable (toList, foldr', foldl', all)
-- import Data.Maybe (fromJust)
-- import Data.List
import Data.Char
import Control.Monad.Reader
import Control.Lens hiding ((<|), (|>))
-- import Data.Map.Lens


type Position = (Integer, Integer) -- r, c

type Keys = S.Set Char
type PointOfInterest = M.Map Position Char

data Explorer = Explorer { _position :: S.Set Char
                           , _keysHeld :: Keys
                           , _travelled :: Int
                           } deriving (Show)
makeLenses ''Explorer

type ExploredStates = S.Set Explorer

type ExpandedCave = S.Set Position
data ExpandedCaveComplex = ExpandedCaveComplex { _caveE :: ExpandedCave
                               , _keysE :: PointOfInterest
                               , _doors :: PointOfInterest
                               } deriving (Eq, Ord, Show)
makeLenses ''ExpandedCaveComplex

data CaveEdge = CaveEdge { _keysRequired :: S.Set Char
                         , _distance :: Int
                         } deriving (Eq, Ord, Show)
makeLenses ''CaveEdge   

type EdgeKey = (Char, Char)
type Cave = M.Map EdgeKey CaveEdge

data CaveComplex = CaveComplex { _cave :: Cave
                               , _keys :: S.Set Char
                               } deriving (Eq, Ord, Show)
makeLenses ''CaveComplex

type CaveContext = Reader CaveComplex

data Agendum = Agendum { _current :: Explorer
                       , _trail :: Q.Seq Explorer
                       , _cost :: Int} deriving (Show, Eq)
type Agenda = P.MinPQueue Int (Agendum)


instance Eq Explorer where
    e1 == e2 = (_position e1 == _position e2) && (_keysHeld e1 == _keysHeld e2)

instance Ord Explorer where
    e1 `compare` e2 =
        if _position e1 == _position e2
        then (_keysHeld e1) `compare` (_keysHeld e2)
        else (_position e1) `compare` (_position e2)

 
    -- positionE :: e -> Position
    -- keysHeldE :: e -> Keys

successors :: Explorer -> CaveContext (Q.Seq Explorer)
successors explorer = -- return Q.empty
    do let heres = explorer ^. position
       cavern <- asks _cave
       let kH = explorer ^. keysHeld
       let locations0 = M.filterWithKey (\k _ds -> anyEdgeTouch heres k) cavern
       let locations1 = M.filter (\e -> S.null ((e ^. keysRequired) `S.difference` kH)) locations0
       let succs = M.foldrWithKey' (\k e q -> (extendExplorer explorer k e) <| q) Q.empty locations1
       return succs

estimateCost :: Explorer -> CaveContext Int
estimateCost explorer = -- return 0
    do let heres = explorer ^. position
       ks <- asks _keys
       cavern <- asks _cave
       let kH = explorer ^. keysHeld
       let unfound = ks `S.difference` kH
       let unfoundEdges0 = M.filterWithKey (\k _ -> anyEdgeTouch heres k) cavern
       let unfoundEdges = M.filterWithKey (\k _ -> not $ anyEdgeTouch kH k) unfoundEdges0
       let furthest = maximum $ (0:) $ map _distance $ M.elems unfoundEdges
       return $ max 0 $ furthest + (S.size unfound) - 1

emptyExplorer :: S.Set Char -> Explorer
emptyExplorer ps = Explorer { _position = ps, _keysHeld = S.empty, _travelled = 0 }

extendExplorer :: Explorer -> EdgeKey -> CaveEdge -> Explorer
extendExplorer explorer edgeKey edge = 
    explorer & position .~ pos'
             & keysHeld .~ kH'
             & travelled .~ d'
    where here = S.findMin $ S.filter (\p -> edgeTouches p edgeKey) (explorer ^. position)
          there = edgeOther here edgeKey
          kH' = S.insert there (explorer ^. keysHeld)
          d' = (explorer ^. travelled) + (edge ^. distance)
          pos' = S.insert there $ S.delete here (explorer ^. position)


main :: IO ()
main = do 
        text <- readFile "data/advent18.txt"
        let (ccE, startPosition) = buildCaveComplex text
        -- print ccE
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
        cavern0 = _caveE caveComplex0
        cavern = cavern0 `S.difference` [(re, ce), (re + 1, ce), (re - 1, ce), (re, ce + 1), (re, ce - 1)]
        caveComplex = caveComplex0 {_caveE = cavern}
        cc = contractCave caveComplex startPositions
        explorer = emptyExplorer $ S.fromList "0123"
        result = runReader (searchCave explorer) cc


-- buildCaveComplex :: Explorer e => String -> (CaveComplex, e)
buildCaveComplex :: String -> (ExpandedCaveComplex, Position)
buildCaveComplex text = (ccE, startPosition)
    where (ccE, startPosition) = foldl' buildCaveRow (cc0, (0, 0)) $ zip [0..] rows
          cc0 = ExpandedCaveComplex {_caveE = S.empty, _keysE = M.empty, _doors = M.empty}
          -- explorer0 = emptyExplorer -- Explorer { _position = (0, 0), _keysHeld = S.empty }
          rows = lines text

buildCaveRow :: (ExpandedCaveComplex, Position) -> (Integer, String) -> (ExpandedCaveComplex, Position)
buildCaveRow (cc, explorers) (r, row) = foldl' (buildCaveCell r) (cc, explorers) $ zip [0..] row


buildCaveCell :: Integer -> (ExpandedCaveComplex, Position) -> (Integer, Char) -> (ExpandedCaveComplex, Position)
buildCaveCell r (cc, startPosition) (c, char) 
    | char == '.' = (cc', startPosition)
    | char == '@' = (cc', here)
    | isLower char = (cc' { _keysE = M.insert here char $ _keysE cc'}, startPosition)
    | isUpper char = (cc' { _doors = M.insert here char $ _doors cc'}, startPosition)
    | otherwise = (cc, startPosition)
    where cc' = cc { _caveE = S.insert here $ _caveE cc }
          here = (r, c)



mkEdgeKey a b = if a < b then (a, b) else (b, a)

edgeTouches x (a, b)
    | x == a = True
    | x == b = True
    | otherwise = False

anyEdgeTouch xs p = S.foldl' (\t x -> t || (edgeTouches x p)) False xs

edgeOther x (a, b)
    | x == a = b
    | otherwise = a



contractCave :: ExpandedCaveComplex -> [Position] -> CaveComplex
contractCave expanded startPositions = cavern
    where explorers = M.fromList $ zip startPositions $ map intToDigit [0..]
          starts = M.union explorers $ _keysE expanded
          cavern0 = CaveComplex {_cave = M.empty, _keys = S.fromList $ M.elems $ _keysE expanded}
          cavern = M.foldrWithKey (contractFrom expanded) cavern0 starts

contractFrom :: ExpandedCaveComplex -> Position -> Char -> CaveComplex -> CaveComplex
contractFrom expanded startPos startKey cc = cc { _cave = M.union (_cave cc) reachables }
    where reachables = reachableFrom [(startPos, edge0)] S.empty expanded' startKey
          edge0 = CaveEdge {_keysRequired = S.empty, _distance = 0}
          expanded' = expanded {_keysE = M.delete startPos $ _keysE expanded}

reachableFrom :: [(Position, CaveEdge)] -> (S.Set Position) -> ExpandedCaveComplex -> Char -> Cave
reachableFrom [] _closed _expanded _startKey = M.empty
reachableFrom ((here, edge):boundary) closed expanded startKey
    | here `S.member` closed = reachableFrom boundary closed expanded startKey
    | here `M.member` ks = M.insert edgeKey edge $ reachableFrom boundary closed' expanded startKey
    | here `M.member` drs = reachableFrom boundaryD closed' expanded startKey
    | otherwise = reachableFrom boundary' closed' expanded startKey
    where nbrs0 = S.intersection (_caveE expanded) $ possibleNeighbours here
          nbrs = S.difference nbrs0 closed
          closed' = S.insert here closed
          ks = _keysE expanded
          drs = _doors expanded
          edgeKey = mkEdgeKey startKey (ks!here)
          edge' = edge { _distance = (_distance edge) + 1}
          edgeD = edge' {_keysRequired = S.insert (toLower (drs!here)) (_keysRequired edge')}
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
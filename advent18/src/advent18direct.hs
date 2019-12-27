import Debug.Trace

-- import qualified Data.Text.IO as TIO

import qualified Data.Map.Strict as M
-- import Data.Map.Strict ((!))
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


class (Eq e, Ord e) => ExplorerC e where 
    successors :: e -> CaveContext (Q.Seq e)
    estimateCost :: e -> CaveContext Int
    -- positionE :: e -> Position
    keysHeldE :: e -> Keys
    emptyExplorer :: e


data Explorer1 = Explorer1 { _position1 :: Position
                           , _keysHeld1 :: Keys
                           } deriving (Eq, Ord, Show)
data Explorer4 = Explorer4 { _position4 :: S.Set Position
                           , _keysHeld4 :: Keys
                           } deriving (Eq, Ord, Show)
type ExploredStates e = S.Set e

type Cave = S.Set Position
data CaveComplex = CaveComplex { _cave :: Cave
                               , _keys :: PointOfInterest
                               , _doors :: PointOfInterest
                               } deriving (Eq, Ord, Show)
type CaveContext = Reader CaveComplex

data Agendum e = Agendum { _current :: e
                       , _trail :: Q.Seq e
                       , _cost :: Int} deriving (Show, Eq)
type Agenda e = P.MinPQueue Int (Agendum e)
-- type Candidates e = S.Set (Int, Agendum e)

instance ExplorerC Explorer1 where
    successors explorer = 
        do  let here = _position1 explorer
            let locations0 = possibleNeighbours here
            cave <- asks _cave
            keys <- asks _keys
            doors <- asks _doors
            let keysHeld = _keysHeld1 explorer
            let locations1 = Q.filter (`S.member` cave) locations0
            let locations2 = Q.filter (hasKeyFor doors keysHeld) locations1
            return $ fmap (\l -> explorer { _position1 = l, _keysHeld1 = pickupKey keys keysHeld l}) locations2

    estimateCost explorer = -- return 0
        do keys <- asks _keys
           let (r, c) = _position1 explorer
           let unfoundKeys = M.keysSet $ M.filter (`S.notMember` (_keysHeld1 explorer)) keys
           let (minR, maxR, minC, maxC) = bounds $ unfoundKeys
           --  = minimum $ map fst $ M.keys unfoundKeys
           -- let minC = minimum $ map snd $ M.keys unfoundKeys
           -- let maxR = maximum $ map fst $ M.keys unfoundKeys
           -- let maxC = maximum $ map snd $ M.keys unfoundKeys
           let spanR = spanV r minR maxR
           let spanC = spanV c minC maxC
           if S.null unfoundKeys
           then return 0
           else return $ fromIntegral (spanR + spanC)
           -- return $ sum $ map (manhattan here) $ M.keys unfoundKeys

    -- positionE = _position1
    keysHeldE = _keysHeld1  
    emptyExplorer = Explorer1 { _position1 = (0, 0), _keysHeld1 = S.empty }

instance ExplorerC Explorer4 where
    successors explorer = 
        do  let rawHeres = _position4 explorer
            let heres = setToSeq $ allSplits rawHeres
            let locations0 = over (traversed . _1) possibleNeighbours heres
            cave <- asks _cave
            keys <- asks _keys
            doors <- asks _doors
            let keysHeld = _keysHeld4 explorer
            let locations1 = over (traversed . _1) (Q.filter (`S.member` cave)) locations0
            let locations2 = over (traversed . _1) (Q.filter (hasKeyFor doors keysHeld)) locations1
            let locations3 = fmap (\(ls, hs) -> fmap (\l -> (l, hs)) ls) locations2
            let locations4 = foldl1 (><) locations3
            return $ fmap (\(l, hs) -> explorer { _position4 = S.insert l hs, _keysHeld4 = pickupKey keys keysHeld l}) locations4

    estimateCost explorer = -- return 0
        do keys <- asks _keys
           let unfoundKeys = M.keysSet $ M.filter (`S.notMember` (_keysHeld4 explorer)) keys
           let (minR, maxR, minC, maxC) = bounds unfoundKeys
           let (minDR, maxDR, minDC, maxDC) = bounds $ _position4 explorer
           let dr = abs (minR - minDR) + abs (maxR - maxDR)
           let dc = abs (minC - minDC) + abs (maxC - maxDC)
           if S.null unfoundKeys
           then return 0
           else return $ fromIntegral (dr + dc)
           -- return $ sum $ map (manhattan here) $ M.keys unfoundKeys

    -- positionE = _position1
    keysHeldE = _keysHeld4
    emptyExplorer = Explorer4 { _position4 = S.fromList $ replicate 4 (0, 0), _keysHeld4 = S.empty }



main :: IO ()
main = do 
        text <- readFile "data/advent18.txt"
        let (cc, explorer) = buildCaveComplex text
        -- print cc
        -- print explorer
        print $ part1 cc explorer
        print $ part2 cc explorer

part1 :: ExplorerC e => CaveComplex -> e -> Int
part1 cave explorer = maybe 0 (( + 1) . _cost ) result
    where result = runReader (searchCave explorer) cave

-- -- part1 :: CaveComplex -> Explorer -> Maybe Agendum
-- part1 cave explorer = keySeq (fromJust result)
--     where result = runReader (searchCave explorer) cave


part2 ::  CaveComplex -> Explorer1 -> Int
part2 caveComplex0 explorer1 = maybe 0 (( + 1) . _cost ) result
    where 
        (re, ce) = _position1 explorer1
        cave0 = _cave caveComplex0
        cave = cave0 `S.difference` [(re, ce), (re + 1, ce), (re - 1, ce), (re, ce + 1), (re, ce - 1)]
        caveComplex = caveComplex0 {_cave = cave}
        explorer = Explorer4 {_position4 = [(re + 1, ce + 1), (re - 1, ce + 1), (re + 1, ce - 1), (re - 1, ce - 1)], _keysHeld4 = S.empty }
        result = runReader (searchCave explorer) caveComplex

keySeq :: ExplorerC e => (Agendum e) -> Q.Seq Keys
keySeq agendum = Q.filter (not . S.null) kdiff
    where keyss = fmap keysHeldE $ _trail agendum
          kdiff = fmap (uncurry S.difference) $ Q.zip ((keysHeldE $ _current agendum) <| keyss) keyss


searchCave :: ExplorerC e => e -> CaveContext (Maybe (Agendum e))
searchCave explorer = 
    do agenda <- initAgenda explorer
       aStar agenda S.empty


buildCaveComplex text = foldl' buildCaveRow (cc0, explorer0) $ zip [0..] rows
    where cc0 = CaveComplex {_cave = S.empty, _keys = M.empty, _doors = M.empty}
          explorer0 = emptyExplorer -- Explorer { _position = (0, 0), _keysHeld = S.empty }
          rows = lines text

buildCaveRow (cc, explorer) (r, row) = foldl' (buildCaveCell r) (cc, explorer) $ zip [0..] row

buildCaveCell r (cc, explorer) (c, char) 
    | char == '.' = (cc', explorer)
    | char == '@' = (cc', explorer { _position1 = here })
    | isLower char  = (cc' { _keys = M.insert here char $ _keys cc'}, explorer)
    | isUpper char  = (cc' { _doors = M.insert here char $ _doors cc'}, explorer)
    | otherwise = (cc, explorer)
    where cc' = cc { _cave = S.insert here $ _cave cc }
          here = (r, c)


initAgenda :: ExplorerC e => e -> CaveContext (Agenda e)
initAgenda explorer = 
    do cost <- estimateCost explorer
       return $ P.singleton cost Agendum { _current = explorer, _trail = Q.empty, _cost = cost}


aStar :: ExplorerC e => Agenda e -> ExploredStates e -> CaveContext (Maybe (Agendum e))
-- aStar [] _ = Agendum {current=buildingTest, trail=[], cost=0}
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


isGoal :: ExplorerC e => e -> CaveContext Bool
isGoal explorer = 
    do keys <- asks (S.fromList . M.elems . _keys)
       return $ keys == keysHeldE explorer


candidates :: ExplorerC e => Agendum e -> ExploredStates e -> CaveContext (Q.Seq (Agendum e))
candidates agendum closed = 
    do  let candidate = _current agendum
        let previous = _trail agendum
        succs <- successors candidate
        let nonloops = Q.filter (\s -> not $ s `S.member` closed) succs
        mapM (makeAgendum candidate previous) nonloops

makeAgendum :: ExplorerC e => e -> (Q.Seq e) -> e -> CaveContext (Agendum e)
makeAgendum candidate previous new = 
    do cost <- estimateCost new
       return Agendum { _current = new
                      , _trail = candidate <| previous
                      , _cost = cost + (Q.length previous)
                      }



hasKeyFor :: PointOfInterest -> Keys -> Position -> Bool
-- hasKeyFor doors keys here | trace ("hkf: " ++ (intercalate " " [show doors, show keys, show here, show (maybe True (`S.member` keys) $ M.lookup here doors)])) False = undefined
hasKeyFor doors keys here = maybe True keyForDoor $ M.lookup here doors
    where keyForDoor d = (toLower d) `S.member` keys
    -- if location `M.member` doors
    -- then (doors!location) `S.elem` keys
    -- else True


pickupKey :: PointOfInterest -> Keys -> Position -> Keys
pickupKey keys held here = maybe held (`S.insert` held) $ M.lookup here keys
    -- if here `M.member` keys
    -- then S.insert (keys!here) held
    -- else held


spanV this minV maxV 
    | this < minV = maxV - this
    | this > maxV = this - minV
    -- | this > minV && this < maxV = (this - minV) + (maxV - this)
    | otherwise = (this - minV) + (maxV - this)

manhattan :: Position -> Position -> Int
manhattan (r1, c1) (r2, c2) = fromIntegral $ abs (r1 - r2) + abs (c1 - c2)

possibleNeighbours :: Position -> Q.Seq Position
possibleNeighbours (r, c) = [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]

bounds :: S.Set Position -> (Integer, Integer, Integer, Integer)
bounds points = (minR, maxR, minC, maxC)
    where  minR = S.findMin $ S.map fst points
           minC = S.findMin $ S.map snd points
           maxR = S.findMax $ S.map fst points
           maxC = S.findMax $ S.map snd points


allSplits :: Ord a => S.Set a -> S.Set (a, S.Set a)
allSplits xs = S.map (\x -> (x, S.delete x xs)) xs

setToSeq :: Ord a => S.Set a -> Q.Seq a
setToSeq = S.foldl (|>) Q.empty

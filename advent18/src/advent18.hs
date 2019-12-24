import Debug.Trace

-- import qualified Data.Text.IO as TIO

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import qualified Data.PQueue.Prio.Min as P
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Data.Sequence ((<|), (|>), (><))
import Data.Foldable (toList, foldr', foldl', all)
import Data.Maybe (fromJust)
import Data.List
import Data.Char
import Control.Monad.Reader


type Position = (Integer, Integer) -- r, c

type Keys = S.Set Char
type PointOfInterest = M.Map Position Char


data Explorer = Explorer { _position :: Position
                         , _keysHeld :: Keys
                         } deriving (Eq, Ord, Show)
type ExploredStates = S.Set Explorer

type Cave = S.Set Position
data CaveComplex = CaveComplex { _cave :: Cave
                               , _keys :: PointOfInterest
                               , _doors :: PointOfInterest
                               } deriving (Eq, Ord, Show)
type CaveContext = Reader CaveComplex

data Agendum = Agendum { _current :: Explorer
                       , _trail :: Q.Seq Explorer
                       , _cost :: Int} deriving (Show, Eq)
type Agenda = P.MinPQueue Int Agendum 
type Candidates = S.Set (Int, Agendum)




main :: IO ()
main = do 
        text <- readFile "data/advent18.txt"
        let (cc, explorer) = buildCaveComplex text
        -- print cc
        -- print explorer
        print $ part1 cc explorer

part1 :: CaveComplex -> Explorer -> Int
part1 cave explorer = maybe 0 (( + 1) . _cost ) result
    where result = runReader (searchCave explorer) cave

-- -- part1 :: CaveComplex -> Explorer -> Maybe Agendum
-- part1 cave explorer = keySeq (fromJust result)
--     where result = runReader (searchCave explorer) cave


keySeq :: Agendum -> Q.Seq Keys
keySeq agendum = Q.filter (not . S.null) kdiff
    where keyss = fmap _keysHeld $ _trail agendum
          kdiff = fmap (uncurry S.difference) $ Q.zip ((_keysHeld $ _current agendum) <| keyss) keyss


searchCave :: Explorer -> CaveContext (Maybe Agendum)
searchCave explorer = 
    do agenda <- initAgenda explorer
       aStar agenda S.empty


buildCaveComplex text = foldl' buildCaveRow (cc0, explorer0) $ zip [0..] rows
    where cc0 = CaveComplex {_cave = S.empty, _keys = M.empty, _doors = M.empty}
          explorer0 = Explorer { _position = (0, 0), _keysHeld = S.empty }
          rows = lines text

buildCaveRow (cc, explorer) (r, row) = foldl' (buildCaveCell r) (cc, explorer) $ zip [0..] row

buildCaveCell r (cc, explorer) (c, char) 
    | char == '.' = (cc', explorer)
    | char == '@' = (cc', explorer { _position = here })
    | isLower char  = (cc' { _keys = M.insert here char $ _keys cc'}, explorer)
    | isUpper char  = (cc' { _doors = M.insert here char $ _doors cc'}, explorer)
    | otherwise = (cc, explorer)
    where cc' = cc { _cave = S.insert here $ _cave cc }
          here = (r, c)




initAgenda :: Explorer -> CaveContext Agenda
initAgenda explorer = 
    do cost <- estimateCost explorer
       return $ P.singleton cost Agendum { _current = explorer, _trail = Q.empty, _cost = cost}


aStar :: Agenda -> ExploredStates -> CaveContext (Maybe Agendum)
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


isGoal :: Explorer -> CaveContext Bool
isGoal explorer = 
    do keys <- asks (S.fromList . M.elems . _keys)
       return $ keys == _keysHeld explorer


candidates :: Agendum -> ExploredStates -> CaveContext (Q.Seq Agendum)
candidates agendum closed = 
    do  let candidate = _current agendum
        let previous = _trail agendum
        succs <- successors candidate
        let nonloops = Q.filter (\s -> not $ s `S.member` closed) succs
        mapM (makeAgendum candidate previous) nonloops

makeAgendum :: Explorer -> (Q.Seq Explorer) -> Explorer -> CaveContext Agendum
makeAgendum candidate previous new = 
    do cost <- estimateCost new
       return Agendum { _current = new
                      , _trail = candidate <| previous
                      , _cost = cost + (Q.length previous)
                      }

successors :: Explorer -> CaveContext (Q.Seq Explorer)
successors explorer = 
    do  let here = _position explorer
        let locations0 = possibleNeighbours here
        cave <- asks _cave
        keys <- asks _keys
        doors <- asks _doors
        let keysHeld = _keysHeld explorer
        let locations1 = Q.filter (`S.member` cave) locations0
        let locations2 = Q.filter (hasKeyFor doors keysHeld) locations1
        return $ fmap (\l -> explorer { _position = l, _keysHeld = pickupKey keys keysHeld l}) locations2


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


estimateCost :: Explorer -> CaveContext Int
estimateCost explorer = -- return 0
    do keys <- asks _keys
       let (r, c) = _position explorer
       let unfoundKeys = M.filter (`S.notMember` (_keysHeld explorer)) keys
       let minR = minimum $ map fst $ M.keys unfoundKeys
       let minC = minimum $ map snd $ M.keys unfoundKeys
       let maxR = maximum $ map fst $ M.keys unfoundKeys
       let maxC = maximum $ map snd $ M.keys unfoundKeys
       let spanR = spanV r minR maxR
       let spanC = spanV c minC maxC
       if M.null unfoundKeys
       then return 0
       else return $ fromIntegral (spanR + spanC)
       -- return $ sum $ map (manhattan here) $ M.keys unfoundKeys

spanV this minV maxV 
    | this < minV = maxV - this
    | this > maxV = this - minV
    -- | this > minV && this < maxV = (this - minV) + (maxV - this)
    | otherwise = (this - minV) + (maxV - this)

manhattan :: Position -> Position -> Int
manhattan (r1, c1) (r2, c2) = fromIntegral $ abs (r1 - r2) + abs (c1 - c2)

possibleNeighbours :: Position -> Q.Seq Position
possibleNeighbours (r, c) = [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]

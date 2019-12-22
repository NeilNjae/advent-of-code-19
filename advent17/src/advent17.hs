import Debug.Trace

import Intcode

import qualified Data.Text.IO as TIO

import qualified Data.Set as S
import Data.Char
import Data.List
import Control.Monad

type Position = (Integer, Integer) -- r, c
data Direction = North | East | South | West deriving (Show, Eq, Ord, Enum, Bounded)
data Step = F | ACW | CW deriving (Show, Eq, Ord)
data Command = FN Int | L | R | A | B | C deriving (Eq)

instance Show Command where
    show (FN n) = show n
    show L = "L"
    show R = "R"
    show A = "A"
    show B = "B"
    show C = "C"
    showList [] s = s
    showList (c:[]) s = (show c) ++ s
    showList (c:cs) s = (show c) ++ "," ++ (showList cs s)

type Routine = ([Command], [Command], [Command], [Command])

type Scaffold = S.Set Position

data ScaffoldBuilder = ScaffoldBuilder 
    { _scaffold :: Scaffold
    , _r :: Integer
    , _c :: Integer
    , _droidPos :: Position
    , _droidDirection :: Direction
    } deriving (Show, Eq)


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent17.txt"
        let mem = parseMachineMemory text
        -- print mem
        let sb = buildScaffold mem
        print $ part1 sb
        let (scaff, num) = part2 sb mem
        putStrLn scaff
        print num


part1 sb = S.foldl (+) 0 $ S.map alignmentParam intersections
    where scaffold = _scaffold sb
          intersections = S.filter (isIntersection scaffold) scaffold


part2 sb mem = (scaff, last output)
    where compressedCmds = findRoutine sb
          inputs = encodeRoutine compressedCmds
          mem' = (2:(tail mem))
          (_, _, output) = runProgram inputs mem'
          scaff = map (chr . fromIntegral) $ init output


buildScaffold :: [Integer] -> ScaffoldBuilder
buildScaffold mem = foldl' addGridChar emptyScaffoldBuilder output
    where (_, _, output) = runProgram [] mem
          emptyScaffoldBuilder = ScaffoldBuilder {_scaffold = S.empty, _r = 0, _c = 0, 
                    _droidPos = (0, 0), _droidDirection = North }

addGridChar :: ScaffoldBuilder -> Integer -> ScaffoldBuilder
addGridChar sb 10  = sb { _r = _r sb + 1, _c = 0 }
addGridChar sb 46  = sb { _c = _c sb + 1 }
addGridChar sb 35  = sb { _scaffold = S.insert (_r sb, _c sb) $ _scaffold sb, 
                            _c = _c sb + 1 }
addGridChar sb 94  = sb { _scaffold = S.insert (_r sb, _c sb) $ _scaffold sb, 
                            _c = _c sb + 1,
                            _droidPos = (_r sb, _c sb), _droidDirection = North }
addGridChar sb 118 = sb { _scaffold = S.insert (_r sb, _c sb) $ _scaffold sb, 
                            _c = _c sb + 1,
                            _droidPos = (_r sb, _c sb), _droidDirection = South }
addGridChar sb 60  = sb { _scaffold = S.insert (_r sb, _c sb) $ _scaffold sb, 
                            _c = _c sb + 1,
                            _droidPos = (_r sb, _c sb), _droidDirection = West }
addGridChar sb 61  = sb { _scaffold = S.insert (_r sb, _c sb) $ _scaffold sb, 
                            _c = _c sb + 1,
                            _droidPos = (_r sb, _c sb), _droidDirection = East }


isIntersection :: Scaffold -> Position -> Bool
isIntersection scaffold (r, c) = neighbours `S.isSubsetOf` scaffold
    where  neighbours = [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

alignmentParam :: Position -> Integer
alignmentParam (r, c) = r * c



findRoutine :: ScaffoldBuilder -> Routine
findRoutine scaff = head $ compressedCmds
    where path = findPath scaff
          cmds = toCommands path
          compressedCmds = compress cmds

encodeRoutine :: Routine -> [Integer]
encodeRoutine (abc, a, b, c) = map (fromIntegral . ord) $ unlines [show abc, show a, show b, show c, "n", ""]


findPath :: ScaffoldBuilder -> [Step]
findPath = unfoldr takeStep

takeStep :: ScaffoldBuilder -> Maybe (Step, ScaffoldBuilder)
takeStep visitedScaffold = step
    where   scaff = _scaffold visitedScaffold
            here = _droidPos visitedScaffold
            dir = _droidDirection visitedScaffold
            fPos   = ahead here dir
            cwPos  = ahead here $ succW dir
            acwPos = ahead here $ predW dir
            step = if canVisit scaff fPos
                   then Just (F, visitedScaffold {_droidPos = fPos})
                   else if canVisit scaff cwPos
                        then Just (CW, visitedScaffold {_droidDirection = succW dir})
                        else if canVisit scaff acwPos
                             then Just (ACW, visitedScaffold {_droidDirection = predW dir})
                             else Nothing

ahead :: Position -> Direction -> Position
ahead (r, c) North = (r - 1, c)
ahead (r, c) South = (r + 1, c)
ahead (r, c) West  = (r, c - 1)
ahead (r, c) East  = (r, c + 1)

canVisit :: Scaffold -> Position -> Bool
canVisit scaff here = (S.member here scaff)

toCommands :: [Step] -> [Command]
toCommands path = map toCommand segments
    where segments = group path

toCommand :: [Step] -> Command
toCommand segment = case (head $ segment) of 
    F -> FN (length segment)
    CW -> R
    ACW -> L

compress :: [Command] -> [Routine]
compress commands = 
    do  a <- tail $ inits commands
        guard $ length (show a) <= 20
        let commandsA = replace a A commands
        let commandsABase = dropWhile (not . isBase) commandsA
        b <- tail $ inits commandsABase        
        guard $ onlyBase b
        guard $ length (show b) <= 20
        let commandsAB = replace b B commandsA
        let commandsABBase = dropWhile (not . isBase) commandsAB
        c <- tail $ inits commandsABBase
        guard $ onlyBase c
        guard $ length (show c) <= 20
        let commandsABC = replace c C commandsAB
        guard $ length (show commandsABC) <= 20
        guard $ onlyNonBase commandsABC
        return (commandsABC, a, b, c)
        

replace :: Eq a => [a] -> a -> [a] -> [a]
-- replace moves label commands | trace (show moves ++ " " ++ show label ++ " " ++ show commands) False = undefined
replace _ _ [] = []
replace moves label commands = 
    if moves `isPrefixOf` commands
    then (label:(replace moves label commands'))
    else (head commands:(replace moves label (tail commands)))
    where commands' = drop (length moves) commands

onlyBase :: [Command] -> Bool
onlyBase moves = all isBase moves

onlyNonBase :: [Command] -> Bool
onlyNonBase moves = all (not . isBase) moves

isBase :: Command -> Bool
isBase (FN _) = True
isBase L = True
isBase R = True
isBase _ = False


-- | a `succ` that wraps 
succW :: (Bounded a, Enum a, Eq a) => a -> a 
succW dir | dir == maxBound = minBound
          | otherwise = succ dir

-- | a `pred` that wraps
predW :: (Bounded a, Enum a, Eq a) => a -> a
predW dir | dir == minBound = maxBound
          | otherwise = pred dir



-- showScaffold :: VisitedScaffold -> String
-- showScaffold scaff = unlines rows
--     where   minR = S.findMin $ S.map fst $ _scaffold scaff
--             minC = S.findMin $ S.map snd $ _scaffold scaff
--             maxR = S.findMax $ S.map fst $ _scaffold scaff
--             maxC = S.findMax $ S.map snd $ _scaffold scaff
--             rows = [showScaffoldRow scaff minC maxC r | r <- [minR..maxR]]

-- showScaffoldRow :: VisitedScaffold -> Integer -> Integer -> Integer -> String
-- showScaffoldRow scaff minC maxC r = [showScaffoldCell scaff r c | c <- [minC..maxC]] 

-- showScaffoldCell :: VisitedScaffold -> Integer -> Integer -> Char
-- showScaffoldCell scaff r c = 
--     if S.member (r, c) (_scaffold scaff)
--     then '#'
--     else ' '


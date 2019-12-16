import Debug.Trace

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

import Data.List
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import qualified Data.Set as S


data Reagent = Reagent { _quantity :: Int, _chemical :: String } deriving (Ord, Eq, Show)
data Reaction = Reaction {_lhs :: S.Set Reagent, _rhs :: Reagent} deriving (Eq, Show)

type Reactions = M.Map String Reaction
type Requirement = M.Map String Int


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent14.txt"
        let reactions = successfulParse text
        print $ part1 reactions
        print $ part2 reactions

oreLimit :: Int
oreLimit = 10^12

-- part1 reactions = required!"ORE"
--     where required0 = M.singleton "FUEL" 1
--           required = produce reactions required
part1 reactions = oreForFuel reactions 1

part2 reactions = searchFuel reactions (upper `div` 2) upper 
    where upper = findUpper reactions (oreLimit `div` base)
          base = oreForFuel reactions 1

oreForFuel :: Reactions -> Int -> Int
oreForFuel reactions n = required!"ORE"
    where required0 = M.singleton "FUEL" n
          required = produce reactions required0 

findUpper :: Reactions -> Int -> Int
-- findUpper _ n | trace ("Upper " ++ show n) False = undefined
findUpper reactions n = if ore > oreLimit
                    then n
                    else findUpper reactions (n * 2)
    where ore = oreForFuel reactions n 

searchFuel :: Reactions -> Int -> Int -> Int
-- searchFuel _ lower upper | trace ("Search " ++ show lower ++ " - " ++ show upper) False = undefined
searchFuel reactions lower upper 
    | upper == lower = upper
    | otherwise = if ore > oreLimit
                  then searchFuel reactions lower (mid - 1)
                  else searchFuel reactions mid upper
    where mid = (upper + lower + 1) `div` 2
          ore = oreForFuel reactions mid


produce :: Reactions -> Requirement -> Requirement
produce reactions required 
    | M.null outstanding = required 
    | otherwise = produce reactions required''
    where outstanding =  M.filter (> 0) $ nonOre required
          (chem, qty) = M.findMin outstanding
          reaction = reactions!chem
          productQty = _quantity $ _rhs reaction
          applications = max 1 (qty `div` productQty)
          qty' = qty - (applications * productQty)
          required' = M.insert chem qty' required
          required'' = S.foldl (addRequrirement applications) required' (_lhs reaction) 


nonOre :: Requirement -> Requirement
nonOre = M.filterWithKey (\c _ -> c /= "ORE")


addRequrirement :: Int -> Requirement -> Reagent -> Requirement
addRequrirement n requirements reagent = M.insert chem qty' requirements
    where chem = _chemical reagent
          qty = M.findWithDefault 0 chem requirements
          qty' = qty + (n * _quantity reagent) 


-- Parse the input file
type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty
-- sc = L.space (skipSome (char ' ')) CA.empty CA.empty

lexeme  = L.lexeme sc
integer = lexeme L.decimal
-- signedInteger = L.signed sc integer
symb = L.symbol sc
arrowP = symb "=>"
commaP = symb ","
identifierP = some alphaNumChar <* sc

reactionsP = mkReactions <$> many reactionP

reactionP = Reaction <$> reagentsP <* arrowP <*> reagentP

reagentP = Reagent <$> integer <*> identifierP
reagentsP = S.fromList <$> reagentP `sepBy` commaP

mkReactions :: [Reaction] -> Reactions
mkReactions = foldl' addReaction M.empty
    where addReaction base reaction = M.insert (_chemical $ _rhs reaction) reaction base

-- successfulParse :: Text -> [Vec]
successfulParse :: Text -> Reactions
successfulParse input = 
        case parse reactionsP "input" input of
                Left  _err -> M.empty -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right reactions -> reactions

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
data Rule = Rule {_lhs :: S.Set Reagent, _rhs :: Reagent} deriving (Eq, Show)

type RuleBase = M.Map String Rule
type Requirement = M.Map String Int


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent14.txt"
        -- let rules = successfulParse text
        -- let ruleBase = mkRuleBase rules
        let ruleBase = successfulParse text
        -- print rules 
        -- print ruleBase
        print $ part1 ruleBase
        print $ part2 ruleBase

oreLimit :: Int
oreLimit = 10^12

mkRuleBase :: [Rule] -> RuleBase
mkRuleBase = foldl' addRule M.empty
    where addRule base rule = M.insert (_chemical $ _rhs rule) rule base


-- part1 rules = required!"ORE"
--     where required0 = M.singleton "FUEL" 1
--           required = produce rules required
part1 rules = oreForFuel rules 1

part2 rules = searchFuel rules (upper `div` 2) upper 
    where upper = findUpper rules (oreLimit `div` base)
          base = oreForFuel rules 1

oreForFuel :: RuleBase -> Int -> Int
oreForFuel rules n = required!"ORE"
    where required0 = M.singleton "FUEL" n
          required = produce rules required0 

findUpper :: RuleBase -> Int -> Int
-- findUpper _ n | trace ("Upper " ++ show n) False = undefined
findUpper rules n = if ore > oreLimit
                    then n
                    else findUpper rules (n * 2)
    where ore = oreForFuel rules n 

searchFuel :: RuleBase -> Int -> Int -> Int
-- searchFuel _ lower upper | trace ("Search " ++ show lower ++ " - " ++ show upper) False = undefined
searchFuel rules lower upper 
    | upper == lower = upper
    | otherwise = if ore > oreLimit
                  then searchFuel rules lower (mid - 1)
                  else searchFuel rules mid upper
    where mid = (upper + lower + 1) `div` 2
          ore = oreForFuel rules mid


produce :: RuleBase -> Requirement -> Requirement
produce rules required 
    | M.null outstanding = required 
    | otherwise = produce rules required''
    where outstanding =  M.filter (> 0) $ nonOre required
          (chem, qty) = M.findMin outstanding
          rule = rules!chem
          productQty = _quantity $ _rhs rule
          applications = max 1 (qty `div` productQty)
          qty' = qty - (applications * productQty)
          required' = M.insert chem qty' required
          required'' = S.foldl (addRequrirement applications) required' (_lhs rule) 


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

rulesP = mkRuleBase <$> many ruleP

ruleP = Rule <$> reagentsP <* arrowP <*> reagentP

reagentP = Reagent <$> integer <*> identifierP
reagentsP = S.fromList <$> reagentP `sepBy` commaP

-- successfulParse :: Text -> [Vec]
successfulParse :: Text -> RuleBase
successfulParse input = 
        case parse rulesP "input" input of
                Left  _err -> M.empty -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right rules -> rules

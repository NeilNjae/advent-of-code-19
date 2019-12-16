import Debug.Trace

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA


import Data.Ratio
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
        let rules = successfulParse text
        let ruleBase = mkRuleBase rules
        -- print rules 
        -- print ruleBase
        print $ part1 ruleBase
        print $ part2 ruleBase

oreLimit = 10^12

mkRuleBase = foldl' addRule M.empty
    where addRule base rule = M.insert (_chemical $ _rhs rule) rule base

-- part1 rules = required!"ORE"
--     where required0 = M.singleton "FUEL" 1
--           required = produce rules required0
part1 rules = oreForFuel rules 1

part2 rules = searchFuel rules 1 upper 
    where upper = findUpper rules (oreLimit `div` base)
          base = oreForFuel rules 1


oreForFuel rules n = required!"ORE"
    where required0 = M.singleton "FUEL" n
          required = produce rules required0 

findUpper _ n | trace ("Upper " ++ show n) False = undefined
findUpper rules n = if ore > oreLimit
                    then n
                    else findUpper rules (n * 2)
    where ore = oreForFuel rules n 

searchFuel _ lower upper | trace ("Search " ++ show lower ++ " - " ++ show upper) False = undefined
searchFuel rules lower upper 
    | upper == lower = upper
    | otherwise = if ore > oreLimit
                  then searchFuel rules lower mid
                  else searchFuel rules mid upper
    where mid = (upper + lower) `div` 2
          ore = oreForFuel rules mid


produce :: RuleBase -> Requirement -> Requirement
produce rules required 
    | M.null outstanding = required 
    | otherwise = produce rules required''
    where outstanding =  M.filter (> 0) $ nonOre required
          (chem, qty) = M.findMin outstanding
          rule = rules!chem
          qty' = qty - (_quantity $ _rhs rule)
          required' = M.insert chem qty' required
          required'' = S.foldl addRequrirement required' (_lhs rule)


nonOre :: Requirement -> Requirement
nonOre = M.filterWithKey (\c _ -> c /= "ORE")


addRequrirement :: Requirement -> Reagent -> Requirement
addRequrirement requirements reagent = M.insert chem qty' requirements
    where chem = _chemical reagent
          qty = M.findWithDefault 0 chem requirements
          qty' = qty + (_quantity reagent) 


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


rulesP = many ruleP

ruleP = Rule <$> reagentsP <* arrowP <*> reagentP

reagentP = Reagent <$> integer <*> identifierP
reagentsP = S.fromList <$> reagentP `sepBy` commaP

-- successfulParse :: Text -> [Vec]
successfulParse :: Text -> [Rule]
successfulParse input = 
        case parse rulesP "input" input of
                Left  _err -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right rules -> rules

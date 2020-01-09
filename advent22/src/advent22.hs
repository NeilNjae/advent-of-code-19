-- import Debug.Trace

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA


import Data.Finite (Finite, modulo, getFinite)
import Data.Group (Group(..), pow)
import GHC.TypeNats (KnownNat)

import Data.Foldable (fold)


data ShuffleOp = Cut Integer
               | Increment Integer
               | Stack 
               deriving (Eq, Ord, Show)

type Shuffle = [ShuffleOp]

data Affine n = Affine { affA :: !(Finite n)
                       , affB :: !(Finite n)
                       } deriving (Eq, Ord, Show)


instance KnownNat n => Semigroup (Affine n) where
    Affine a2 b2 <> Affine a1 b1 = Affine (a2 * a1) (a2 * b1 + b2)

instance KnownNat n => Monoid (Affine n) where
    mempty = Affine 1 0

instance KnownNat n => Group (Affine n) where
    invert (Affine a b) = Affine a' b'
        where
        a' = a ^ (maxBound @(Finite n) - 1)
        b' = negate (a' * b)


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent22.txt"
        let shuffle = successfulParse text 
        print $ part1 shuffle
        print $ part2 shuffle


part1 shuffle = getFinite $ trans @$ 2019
    where trans = mergeOps $ map affOfOp shuffle :: Affine 10007

part2 shuffle = getFinite $ invert bigTrans @$ 2020
    where trans = mergeOps $ map affOfOp shuffle :: Affine 119315717514047
          bigTrans = trans `pow` 101741582076661



affOfOp :: KnownNat n => ShuffleOp -> Affine n
affOfOp (Cut c) = Affine 1 (negate (modulo c))
affOfOp (Increment i) = Affine (modulo i) 0
affOfOp Stack = Affine (modulo -1) (modulo -1)

mergeOps :: KnownNat n => [Affine n] -> Affine n
mergeOps = fold . reverse 

-- given a transformation, where does the item at x end up?
(@$) :: KnownNat n => Affine n -> Finite n -> Finite n
Affine a b @$ x = a * x + b


-- Parse the input file
type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty
-- sc = L.space (skipSome (char ' ')) CA.empty CA.empty

lexeme  = L.lexeme sc
integer = lexeme L.decimal
signedInteger = L.signed sc integer
symb = L.symbol sc
cutSP = symb "cut"
dealIncrementP = symb "deal with increment"
dealIntoP = symb "deal into new stack"

cutP = Cut <$> (cutSP *> signedInteger)
incrementP = Increment <$> (dealIncrementP *> signedInteger)
stackP = Stack <$ dealIntoP

shuffleOpP = cutP <|> incrementP <|> stackP

shuffleP = many shuffleOpP

-- successfulParse :: Text -> [Vec]
successfulParse :: Text -> Shuffle
successfulParse input = 
        case parse shuffleP "input" input of
                Left  _err -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right shuffle -> shuffle

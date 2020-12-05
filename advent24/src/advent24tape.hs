
-- import Debug.Trace


import Data.Bool (bool)
import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Functor.Identity (Identity(..))
import Control.Comonad.Representable.Store (Store(..), StoreT(..), store, experiment, runStore)
import Control.Comonad (Comonad(..))

import Data.Maybe
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)

import Control.Comonad
import Control.Comonad.Cofree
import Data.Distributive
import Data.Functor.Rep
import qualified Data.Sequence as Q
import qualified Data.List.NonEmpty as NE


data TPossible a = TPossible
  { leftward :: a
  , rightward :: a
  , above :: a
  , below :: a
  } deriving (Show, Eq, Functor)

data TChoice = L | R | U | D
  deriving (Show, Eq)

instance Distributive TPossible where
  distribute :: Functor f => f (TPossible a) -> TPossible (f a)
  distribute fga = TPossible (fmap leftward fga) (fmap rightward fga)
                             (fmap above fga) (fmap below fga)

instance Representable TPossible where
  type Rep TPossible = TChoice

  index :: TPossible a -> TChoice -> a
  index here L = leftward here
  index here R = rightward here
  index here U = above here
  index here D = below here

  tabulate :: (TChoice -> a) -> TPossible a
  tabulate describe = TPossible (describe L) (describe R)
                                (describe U) (describe D)

relativePosition :: Q.Seq TChoice -> Int
relativePosition = sum . fmap valOf
  where
    valOf L = (-1)
    valOf R = 1
    valOf U = (-10)
    valOf D = 10

numberLine :: Cofree TPossible Int
numberLine = tabulate relativePosition

project :: NE.NonEmpty a -> Cofree TPossible a
project l = tabulate describe
  where
    describe = (l NE.!!) . foldl go 0
    maxIndex = length l - 1
    minIndex = 0
    go n L = max minIndex (n - 1)
    go n R = min maxIndex (n + 1)
    go n U = max minIndex (n - 1)
    go n D = min maxIndex (n + 1)

elems :: NE.NonEmpty String
elems = "one" NE.:| ["two", "three"]

path :: Q.Seq TChoice
path = Q.fromList [R, R, R, R, L]

moveTo :: Q.Seq TChoice -> Cofree TPossible a -> Cofree TPossible a
moveTo ind = extend (\cfr -> index cfr ind)

main :: IO ()
main = print $ index (project elems) path
-- main = print elems

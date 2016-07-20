module Histogram(Histogram(..), emptyHist, addToHist) where

import qualified Data.Map as Map

import Token

data Histogram = Histogram (Map.Map Token Int) deriving (Show)

emptyHist :: Histogram
emptyHist = Histogram Map.empty

addToHist :: Token -> Histogram -> Histogram
addToHist str (Histogram hmp) =
  let count = Map.findWithDefault 0 str hmp
  in Histogram $ Map.insert str (count + 1) hmp




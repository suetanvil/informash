{-# LANGUAGE DeriveGeneric #-}
module WordRamp (WordRamp(..), emptyWordRamp, newWordRamp,
                 pickFromRamp, allWords ) where

import Data.List
import qualified Data.Map as Map
import Data.Binary
import GHC.Generics (Generic)

import Histogram
import Util
import Token

data WordRamp = WordRamp {
  rampTotal :: Int,
  rampItems :: [(Token, Int)]
  } deriving (Generic, Eq, Show)
instance Binary WordRamp

emptyWordRamp :: WordRamp
emptyWordRamp = WordRamp 0 []

newWordRamp :: Histogram -> WordRamp
newWordRamp (Histogram hmap) =
  let uramp = Map.foldrWithKey (\word count l -> l ++ [(word, count)]) [] hmap
      rcmp (_,c1) (_,c2) = compare c1 c2
      sramp = sortBy rcmp uramp
      total = foldl' (\tot (w,c) -> tot + c) 0 sramp
  in WordRamp total sramp

pickFromRamp :: Double -> WordRamp -> Token
pickFromRamp idx ramp = 
  let iidx = pickInt idx (rampTotal ramp)
      climb i ((w,c):rest)
        | i <= c          = w
        | otherwise       = climb (i - c) rest
  in climb iidx (rampItems ramp)

allWords :: WordRamp -> [Token]
allWords ramp = map fst $ rampItems ramp


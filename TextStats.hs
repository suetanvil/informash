module TextStats(TextStats(..), emptyTextStats, addTokens, newTextStats) where

import Data.List
import qualified Data.Map as Map

import Histogram
import Token

data TextStats = TextStats (Map.Map Token Histogram) deriving (Show)

emptyTextStats :: TextStats
emptyTextStats = TextStats Map.empty

addWordSeq :: Token -> Token -> TextStats -> TextStats
addWordSeq first second (TextStats hmap) =
  let hist = Map.findWithDefault emptyHist first hmap
  in TextStats $ Map.insert first (addToHist second hist) hmap

addTokens :: [Token] -> TextStats -> TextStats
addTokens src stats =
  let pairify []            = []
      pairify [_]           = []
      pairify (s1:s2:rest)  = (s1, s2) : pairify (s2 : rest)

      pairs = pairify $ filter (not . isEmpty) (ParagraphBreak : src)
      addPair ts (first,second) = addWordSeq first second ts
  in foldl' addPair stats pairs


newTextStats :: [Token] -> TextStats
newTextStats src = addTokens src emptyTextStats


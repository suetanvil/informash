{-# LANGUAGE DeriveGeneric #-}
module WordTable (WordTable(..), newWordTable, makeWordTable, pickWord,
                  pickStartingWord, pickNextWord, printableForm) where

import Data.List
import qualified Data.Map as Map
import Data.Binary
import GHC.Generics (Generic)

import Util
import TextStats
import WordRamp
import Token

data WordTable = WordTable (Map.Map Token WordRamp)
                 deriving (Generic, Eq, Show)
instance Binary WordTable

newWordTable :: TextStats -> WordTable
newWordTable (TextStats stats) =
  let addWord word hist ramps   = Map.insert word (newWordRamp hist) ramps
      wtd                       = Map.foldrWithKey addWord Map.empty stats
  in WordTable wtd
  
makeWordTable :: String -> WordTable
makeWordTable src =
  newWordTable $ newTextStats (ParagraphBreak : tokenize src)

pickWord :: Double -> WordTable -> Token
pickWord idx (WordTable rmap) =
  let words = Map.keys rmap
  in words !! pickInt idx (length words)

-- allAfter :: Token -> WordTable -> [Token]
-- allAfter word (WordTable tmap) =
--   allWords $ Map.findWithDefault emptyWordRamp word tmap
    
pickStartingWord :: Double -> Bool -> WordTable -> Token
pickStartingWord _ True _                    = ParagraphBreak
pickStartingWord idx False (WordTable dict)  =
  let words = ParagraphBreak : filter isEnder (Map.keys dict)
  in words !! pickInt idx (length words - 1)

pickNextWord :: Token -> Double -> WordTable -> Token
pickNextWord prev idx (WordTable rmap) =
  if Map.member prev rmap
  then pickFromRamp idx $ Map.findWithDefault undefined prev rmap
  else pickWord idx (WordTable rmap)

printableForm :: WordTable -> String
printableForm (WordTable tblmap) =
  let addItem k ramp rslt   = (showItem k ramp ++ rampVals ramp ++ "\n\n"):rslt
      showItem k ramp       =
        "[" ++ show k ++ "] " ++ show (rampTotal ramp) ++ "\n"
      rampVals ramp         = foldl ltxt "" (rampItems ramp)
      ltxt txt (wrd, cnt)   = "\t" ++ show wrd ++ " = " ++ show cnt ++ "\n" ++ txt
        
  in concat $ Map.foldrWithKey addItem [] tblmap

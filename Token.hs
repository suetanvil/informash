{-# LANGUAGE DeriveGeneric #-}
module Token(Token(..),  tokenize, untokenize, isEmpty, isEnder,
             isPunct, tweenSpace, tokVal, isParagraphBreak) where

import Data.List
import Data.Binary
import GHC.Generics (Generic)

data Token = Word String | EndSentence Char | Punct Char |
             ParagraphBreak | Hyphen | Open Char | Close Char |
             NullTok
           deriving (Generic, Show, Eq, Ord)

instance Binary Token


openers = "([{"
closers = ")]}"
enders = ".!?"
nonEnders = ",;"
whitespace = " \t\n\r"
nonSpaceDelims = openers ++ closers ++ enders ++ nonEnders
para = "\n\n"

makeToken :: String -> Token
makeToken ""            = NullTok
makeToken " "           = NullTok
makeToken "\n\n"        = ParagraphBreak
makeToken "--"          = Hyphen
makeToken "\""          = Punct '"'
makeToken [tc]
  | tc `elem` enders    = EndSentence tc
  | tc `elem` openers   = Open tc
  | tc `elem` closers   = Close tc
  | tc `elem` nonEnders = Punct tc
  | otherwise           = Word [tc]
makeToken tok           = Word tok

toklst :: String -> [Token]
toklst "" = []
toklst " " = []
toklst tok = [makeToken tok]


buildTokens :: String -> String -> [Token]
buildTokens "" ""       = []
buildTokens "" tok      = [makeToken tok]
buildTokens " " tok     = toklst tok
buildTokens [h] tok     = buildTokens [h,' '] tok
buildTokens (h:rest) tok
  | para `isPrefixOf` (h:rest) =
      toklst tok ++ [ParagraphBreak] ++ buildTokens (tail rest) ""
  | "--" `isPrefixOf` (h:rest) =
      toklst tok ++ [Hyphen] ++ buildTokens (tail rest) ""
  | h `elem` whitespace     = toklst tok ++ buildTokens rest ""
  | h `elem` nonSpaceDelims = toklst tok ++ toklst [h] ++ buildTokens rest ""
  | otherwise               = buildTokens rest (tok ++ [h])
  where 
        h2 = head rest


tokenize :: String -> [Token]
tokenize src = buildTokens src ""


tokVal :: Token -> String
tokVal (Word s)             = s
tokVal (EndSentence c)      = [c]
tokVal (Punct c)            = [c]
tokVal (Open c)             = [c]
tokVal (Close c)            = [c]
tokVal ParagraphBreak       = "\n\n"
tokVal Hyphen               = "--"
tokVal NullTok              = ""

tweenSpace :: Token -> Token -> String
tweenSpace (Punct '"') _                = ""
tweenSpace (Punct _) (Punct '"')        = ""
tweenSpace (EndSentence _) (Punct '"')  = ""
tweenSpace _ (Punct _)                  = ""
tweenSpace _ (EndSentence _)            = ""
tweenSpace (Open _) _                   = ""
tweenSpace _ (Close _)                  = ""
tweenSpace (EndSentence _) _            = "  "
tweenSpace NullTok _                    = ""
tweenSpace _ NullTok                    = ""
tweenSpace ParagraphBreak _             = ""
tweenSpace _ ParagraphBreak             = ""
tweenSpace Hyphen _                     = ""
tweenSpace _ Hyphen                     = ""
tweenSpace _ _                          = " "


isEmpty :: Token -> Bool
isEmpty NullTok                         = True
isEmpty (Word "")                       = True
isEmpty _                               = False

isEnder :: Token -> Bool
isEnder (EndSentence _)                 = True
isEnder _                               = False

isPunct :: Token -> Bool
isPunct (Punct _)                       = True
isPunct _                               = False

isParagraphBreak :: Token -> Bool
isParagraphBreak ParagraphBreak         = True
isParagraphBreak _                      = False


untokenize :: [Token] -> String
untokenize [] = ""
untokenize [a] = tokVal a
untokenize (first:rest) =
  tokVal first ++ utkz (length $ tokVal first) (first:rest)
  where
    width = 75

    utkz :: Int -> [Token] -> String
    utkz _ [prev]                   = ""
    utkz _ (ParagraphBreak:rest)    = utkz 0 (NullTok:rest)

    -- Turn ',' into an EndSentence so that the next pattern matches.
    -- (I may have just made Baby Haskell Curry cry.)
    utkz w (Punct ',' : Punct '"' : rest)       =
      utkz w (EndSentence ',' : Punct '"' : rest)

    utkz w (EndSentence p : Punct '"' : rest)   =
      ('"':"  ") ++ utkz (w + 3) (NullTok:rest)

    utkz w (prev:tok:rest)          =
      let splittable tok            = not (isPunct tok || isEnder tok)
          spc = tweenSpace prev tok
          tokStr = tokVal tok
          w2 = w + length spc + length tokStr
          (spcUse, w3)   = if w2 > width && splittable tok
                           then ("\n", length tokStr)
                           else (spc, w2)
      in spcUse ++ tokStr ++ utkz w3 (tok:rest)


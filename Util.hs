module Util (pickInt, takeUpTo, splits) where

import System.Random

-- Given a Double idx and an Int max, return their multiple as an Int.
-- 
-- The point of this is to choose an item from a list using idx as a
-- fraction of the maximum index.

pickInt :: Double -> Int -> Int
pickInt idx maxVal = max 0 $ floor (idx * fromIntegral maxVal)


-- Take all leading items do not satisfy testFn AND the first function
-- that does.  Just like takeWhile except that the predicated is
-- negated and the first item that matches is also included.
takeUpTo :: (a -> Bool) -> [a] -> [a]
takeUpTo _ []               = []
takeUpTo testFn (h:rest)
  | testFn h                = [h]
  | otherwise               = h : takeUpTo testFn rest


splits :: StdGen -> [StdGen]
splits gen = gen1 : splits gen2 where (gen1, gen2) = split gen

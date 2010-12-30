module Helpers where

import qualified Data.Map as M

-- |Useful and generic average function.
average :: (Real a, Fractional b) => [a] -> b
average list = (realToFrac $ sum list) / (realToFrac $ length list)

-- |Calculates weighted average.
weightedAvg :: (Real a, Fractional b) => [(a, a)] -> b
weightedAvg list = (realToFrac listSum) / (realToFrac elements)
  where elementSum (k, a) = k*a
        listSum = sum $ map elementSum list
        elements = sum $ map snd list

-- |Splits the given list to pieces of given lengths.
sublists :: [Int] -> [t] -> [[t]]
sublists _ [] = []
sublists (l:lengths) bits = curBits : sublists lengths leftBits
  where (curBits,leftBits) = splitAt l bits
        
-- |Counts the occurrences of values.
histogram :: (Ord k, Num a) => [k] -> M.Map k a
histogram values = M.fromListWith (+) $ zip values (repeat 1)


module Decoder where

import Data.List (intercalate)
import qualified Data.Map as M
import SoxDatParser (parseDatFromFile, time, left)
import Derivate
import Helpers (average)

-- |Sound bitstream is 0 initially and has 5% threshold.
soundBitstream = quantize (comparator 0.03) False

-- |Produces graphs of data
timeDecodeCSV samples = unlines $ map combine triplet 
  where rawData = map left samples
        triplet = zip3 (map time samples) rawData (soundBitstream rawData)
        combine (a,b,c) = intercalate "," [show $ scale 1000 a, show $ scale 1000 b, show $ scale 100 $ deTruth c]

-- |For extraction of tuple lists to CSV (useful for pasting to OOo).
tuple2csv xs = unlines $ map showTuple xs
  where showTuple (a,b) = intercalate "," [show a,show b]

-- |Counts weighted average.
weightedAvg :: (Real a, Fractional b) => [(a, a)] -> b
weightedAvg list = (realToFrac listSum) / (realToFrac elements)
  where elementSum (k, a) = k*a
        listSum = sum $ map elementSum list
        elements = sum $ map snd list

deTruth True = 1
deTruth False = 0

scale x y = round (x * y)

-- |Counts the occurrences of values.
histogram :: (Ord k, Num a) => [k] -> M.Map k a
histogram values = M.fromListWith (+) $ zip values (repeat 1)

-- |Produces histograms from bit lengths.
bitHistogram :: (Eq t) => [t] -> M.Map Int Int
bitHistogram bits = histogram $ bitLengths bits

-- |Calculates the estimated bit length in transmission. Converts a
-- |bitstream to bit length list. Doesn't assort bit values. Works on
-- |other types than bits, too.
bitLengths :: (Eq t) => [t] -> [Int]
bitLengths (b:bs) = bitLengths' bs b 1

bitLengths' :: (Eq t) => [t] -> t -> Int -> [Int]
bitLengths' [] _ len = [len]
bitLengths' (b:bs) oldB len | b == oldB = bitLengths' bs b (len+1)
                            | otherwise = len : bitLengths' bs b 1

-- |Group bits from a bitstream to groups of given length and starting offset.
groupBits :: Double -> Double -> [Bool] -> [[Bool]]
groupBits length phase bits = sublists series bits
  where series = seriesLengths $ intSeries length phase

-- |Produces list of bit lengths. Lengths are rounded to integers.
intSeries :: Double -> Double -> [Int]
intSeries length phase = map offset [0..]
  where offset x = floor $ length * x + phase

-- |Converts list of ticks to intervals. Location of the first element is zero.
seriesLengths :: (Num a) => [a] -> [a]
seriesLengths list = zipWith (-) list (0:list)

-- |Splits the given list to pieces of given lengths.
sublists :: [Int] -> [t] -> [[t]]
sublists _ [] = []
sublists (l:lengths) bits = curBits : sublists lengths leftBits
  where (curBits,leftBits) = splitAt l bits
        
-- |Decodes one bit and returns the bit and the "fitness ratio" of that bit.
decodeOneBit :: [Bool] -> (Bool, Double)
decodeOneBit stream | 2*trues > total = (True,ratio)
                    | otherwise       = (False,1-ratio)
  where trues = length $ filter (==True) stream
        total = length stream
        ratio = (realToFrac trues)/(realToFrac total)
        
decodeBits :: [[Bool]] -> [(Bool,Double)]
decodeBits bitgroups = map decodeOneBit bitgroups

-- |Removes equal elements from the beginning.
removeHead :: (Eq t) => [(t, a)] -> [(t, a)]
removeHead list = first:dropWhile (equals first) list
  where equals (a,_) (b,_) | a == b    = True 
                           | otherwise = False
        first = head list

-- |Trims given bit list.
trimBits :: (Eq t) => [(t, a)] -> [(t, a)]
trimBits list = reverse $ removeHead $ reverse $ removeHead list

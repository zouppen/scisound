module Decoder where

import SoxDatParser (parseDatFromFile, time, left)
import Derivate
import Helpers (average, sublists)

-- |Sound bitstream is 0 initially and has 5% threshold.
soundBitstream = quantize (comparator 0.03) False

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

-- |Converts boolean to char. Useful in mapping [Bool] to String.
toBinary True = '1'
toBinary False = '0'
        
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

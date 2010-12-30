-- |This module is for interactive use to help analysis of bit streams.

module Analysis where

import Data.List (intercalate)
import qualified Data.Map as M
import Decoder (soundBitstream)
import Helpers (histogram)
import SoxDatParser (time, left)

-- |Produces histograms from bit lengths.
bitHistogram :: (Eq t) => [t] -> M.Map Int Int
bitHistogram bits = histogram $ bitLengths bits

-- |Produces graphs of data
timeDecodeCSV samples = unlines $ map combine triplet 
  where rawData = map left samples
        triplet = zip3 (map time samples) rawData (soundBitstream rawData)
        combine (a,b,c) = intercalate "," [show $ scale 1000 a, show $ scale 1000 b, show $ scale 100 $ deTruth c]

deTruth True = 1
deTruth False = 0

scale x y = round (x * y)

-- |For extraction of tuple lists to CSV (useful for pasting to OOo).
tuple2csv xs = unlines $ map showTuple xs
  where showTuple (a,b) = intercalate "," [show a,show b]

-- |Calculates the estimated bit length in transmission. Converts a
-- |bitstream to bit length list. Doesn't assort bit values. Works on
-- |other types than bits, too.
bitLengths :: (Eq t) => [t] -> [Int]
bitLengths (b:bs) = bitLengths' bs b 1

bitLengths' :: (Eq t) => [t] -> t -> Int -> [Int]
bitLengths' [] _ len = [len]
bitLengths' (b:bs) oldB len | b == oldB = bitLengths' bs b (len+1)
                            | otherwise = len : bitLengths' bs b 1

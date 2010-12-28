module Decoder where

import Data.List (intercalate)
import SoxDatParser (parseDatFromFile, time, left)
import Derivate

-- |Sound bitstream is 0 initially and has 5% threshold.
soundBitstream = quantize (comparator 0.03) False

-- |Produces graphs of data
timeDecodeCSV samples = unlines $ map combine triplet 
  where rawData = map left samples
        triplet = zip3 (map time samples) rawData (soundBitstream rawData)
        combine (a,b,c) = intercalate "," [show $ scale 1000 a, show $ scale 1000 b, show $ scale 100 $ deTruth c]

deTruth True = 1
deTruth False = 0

scale x y = round (x * y)
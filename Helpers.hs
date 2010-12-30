module Helpers where

-- |Useful and generic average function.
average :: (Real a, Fractional b) => [a] -> b
average list = (realToFrac $ sum list) / (realToFrac $ length list)

toBinary True = '1'
toBinary False = '0'
module Derivate where

quantize :: (t -> t -> Ordering) -> Bool -> [t] -> [Bool]
quantize comparator currentBit [x1] = []
quantize comparator currentBit (x1:xs@(x2:rest)) = new:(quantize comparator new xs)  
  where new = newState currentBit (comparator x1 x2)

comparator threshold a b | diff > threshold = GT
                         | diff < -threshold = LT
                         | otherwise = EQ
                           where diff = a - b

-- |Changes state if old differs too from new.
newState False GT = True
newState True LT = False
newState old _ = old

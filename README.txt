> joo <- parseDatFromFile "/Users/joell/poista/codegrove-pumppu/lampo18.dat"
> let bits = soundBitstream $ map left joo
> decodeBits $ groupBits 3.33 2.16 bits

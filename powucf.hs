fun powerucf(0, _) = 1
powerucf(1,b) = b
powerucf(_,0) = 0
powerucf(e,b) =  b * powerucf(e-1,b)

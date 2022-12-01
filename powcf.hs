fun powcf 0 _ = 1
powcf 1 b  = b
powcf _ 0  = 0
powcf e b =  b * powcf  (e-1) b

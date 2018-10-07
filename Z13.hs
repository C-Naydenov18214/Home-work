operM :: (Int -> Bool) -> Int 
operM r = operM1 0 
 where 
	operM1 y = if r y then y else operM1 (y+1) 

del2 x1 x2 = operM (del x1 x2)
 where
   del x1 x2 y = x1 <= x2 * y 
   operM :: (Int -> Bool) -> Int 
   operM r = operM1 0 
    where 
     operM1 y = if r y then y else operM1 (y+1)
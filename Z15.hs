sqrt1 x = operM (sqrt2 x)
 where
  sqrt2 :: Int -> Int -> Bool 
  sqrt2 x y = (y+1) * (y+1) > x 
  operM :: (Int -> Bool) -> Int 
  operM r = operM1 0 
   where 
    operM1 y = if r y then y else operM1 (y+1)

  
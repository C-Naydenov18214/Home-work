pls x 0 = x
pls x y = let 
          inc x | x >= 0 = x + 1
                | otherwise = error "Arg must be positive!"
          dec x | x >  0 = x - 1
                | x == 0 = 0
                | otherwise = error "Arg must be positive!"
          in pls (inc x) (dec y) 

mns x 0 = x
mns x y = let
          inc x | x >= 0 = x + 1
                | otherwise = error "Arg must be positive!"
          dec x | x >  0 = x - 1
                | x == 0 = 0
                | otherwise = error "Arg must be positive!"
          in if  (x >= y) then mns (dec x) (dec y) else 0 

mlt x 1 = x
mlt x y = pls (mlt x (mns y 1)) (x)
          

{-# LANGUAGE FlexibleContexts #-}
f a b c | a == 0 = error "Eto ne kvadratnoe"    
        | b*b - 4 * a * c > 0 = (((-b - sqrt (b*b-4*a*c))/(2*a)), ((-b + sqrt (b*b-4*a*c))/(2*a))) 
        | b*b - 4 * a * c == 0 = b/(2*a)
        | b*b - 4 * a * c < 0 = error "Korney net" 

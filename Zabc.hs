{-koren  a b c = let
               des a b c |a, b, c != 0 = sqrt(b*b-4*a*c)
                         |a == 0 = c/b
                         |b == 0 = (-(sqrt(c/a))) && (sqrt(a/c)) 
                         |c == 0 = b/a && 0  
               in if (des a b c > 0) then ((-b)+sqrt(b*b-4*a*c)/2*a  &&  ((-b)-sqrt(b*b-4*a*c)/2*a      
               else if  (des a b c == 0) then (-b)/(2*a) 
               else if  (des a b c == c/b) then c/b 
               else if  (des a b c == (-(sqrt(c/a))) && (sqrt(a/c)))  then (-(sqrt(c/a))) && (sqrt(a/c))  
               else if  (des a b c == b/a && 0) then b/a && 0 else error "-1" -}
               koren a b c | a == 0 = -c/b
                           | b == 0 = sqrt(-c/a)
                           | c == 0 = -b/a  
                           | otherwise = (-b + sqrt(b*b - 4*c*a))/2*a                                   
                           |    (-b - sqrt(b*b - 4*c*a))/2*a      
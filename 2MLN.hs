sumP a = sum (filter (\x -> prime x == 1 ) [1..a]) 
 where
  prime a = if  (a == 1)  then  0 else del a b  
    where 
     b = 2
     del a b = if (b >= a) then 1 else  if (mod a b == 0) then 0 else del a (b + 1)
            
sumP::Integer->Integer
sumP a = sum (filter (\x -> prime x) [1..a]) 
 where
  prime::Integer->Bool
  prime a = if  (a == 1)  then  False else del a 2  
   where 
    del::Integer->Integer->Bool
    del a b = if (b >= a) then True else  if (mod a b == 0) then False else del a (b + 1)
            
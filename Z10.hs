f a = if  a == 1 then 0 else del a b c 
 where 
  b = 2 
  c = 0 
  del a b c = if ((b == a) && (prime b == 1)) then c+1 else if ((b == a) && (prime b == 0)) then c else  if ((rest a b == 0) && (prime b == 1))  then del a (b+1) (c+1) else del a (b+1) c 
   where 
    rest a b = if (a<0 || b<0) then error "ERROR" else if (a == 0) then 0 else del a b d
     where 
      d = 1 
      del a b d = if a < b then 0 else if (b*d<a && a-(b*d)>=b) then del (a) (b) (d+1) else a-b*d 
    prime (b) = if (b == 1) then 1 else del b g v 
     where 
      g = 2 
      v = 1
      del b g v = if ((b == g) && (v == 1)) then 1 else if ((b == g) && (v > 1)) then 0 else if (g == b)  then  v+1 else if (rest b g) == 0 then del b (g+1) (v+1) else del b (g+1) v 
       where 
        rest b g = if (b<0 || g<0) then error "ERROR" else if (b == 0) then 0 else del b g r
         where 
          r = 1 
          del b g r = if a < b then 0 else if (g*r<b && b-(g*r)>=g) then del (b) (g) (r+1) else b-g*r 

   



  
prime a = if  a == 1 then 0 else del a b c 
 where 
  b = 2 
  c = 1 
  del a b c = if ((b == a) && (c == 1)) then 1 else if ((b == a) && (c > 1)) then 0 else if (b == a)  then  c+1 else if (rest a b) == 0 then del a (b+1) (c+1) else del a (b+1) c 
   where 
    rest a b = if (a<0 || b<0) then error "ERROR" else if (a == 0) then 0 else del a b d
     where 
      d = 1 
      del a b d = if a < b then 0 else if (b*d<a && a-(b*d)>=b) then del (a) (b) (d+1) else a-b*d

chast a b = if ((a < 0) || (b < 0))  then error "ERROR"  else if (b == 0) then 0 else chastt a b c 
 where
   c = 1
   chastt a b c = if (a < b) then 0 else  if (a > b*c && a - b*c >= b) then chastt (a) (b) (c+1) else c
chast a b = if ((a < 0) || (b < 0))  then error "ERROR"  else if (b == 0) then 0 else chas a b   
 where 
 	chas a b   = if (b == 1) then a else if (a == b) then 1 else if (a < b ) then 0 else if ((a > 0) && (a - b >= 0)) then chas (a - b) (b) + 1 else a 
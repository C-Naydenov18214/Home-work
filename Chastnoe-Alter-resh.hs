chast a b = if ((a < 0) || (b < 0))  then error "ERROR"  else if (b == 0) then 0 else chastt a b 
                where
             	
             	chastt a b  = if (a == b) then 1 else if (a < b) then 0 else if (a >= b) then chastt (a - b) (b) +1 else a  
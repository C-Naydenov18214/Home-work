rest a b = if ((a < 0) || (b < 0))  then error "ERROR"  else if (b == 0) then 0 else res a b  
 where 
        res a b  = if (a - b >= b) then res (a - b) b else if (a < b) then a else if (a == b) then 0 else a - b  
f a b = if ((a < 0) || (b < 0))  then error "ERROR"  else if (b == 0) then 0 else pre a b 
        where
         pre a b = if (a == b) then 1 else if (a < b) then 0 else if (a > b && a - b >= b) then (a-b) b else if (a - b == 0) then 1 else 0 
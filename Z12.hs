nok a b = if (a == b) then a else pr a b 
 where
  pr a b = (a * b) / (nod a b)
   where	
    nod a b = if (a == b) then a else if (a > b) then nod (a - b) b else if (a < b) then nod a (b - a) else nod a b  


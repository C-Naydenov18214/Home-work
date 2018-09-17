korni a b c | (a == 0) && (b == 0) = error "ERROR" 
            | (a == 0) && (b /= 0) = (x,x) 
            | (a /= 0) = (x1,x2) 
 where 
        d = b*b-4*a*c 
        sd = if d<0 then error "net korney" else sqrt d 
        x1 = (-b-sd)/(2*a) 
        x2 = (-b+sd)/(2*a) 
        x = -c/b
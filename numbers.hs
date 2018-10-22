toDecimal::Char->String->String
toDecimal base snumber = if ( (fromEnum (base) > 48) && (fromEnum (base)  <= 57)) then ( perevoD2 (toInteger(fromEnum (base) - 48)) (reverse(snumber)) 0 1) 
	                    else if ((fromEnum (base)  >= 97 ) && (fromEnum (base)  <= 122)) then ( perevoD2 (toInteger(fromEnum (base) - 87)) (reverse (snumber)) 0 1) 
	                    else if ((fromEnum (base)  >= 65 ) && (fromEnum (base) <= 90)) then ( perevoD2 (toInteger(fromEnum (base) - 29)) (reverse (snumber)) 0 1 )	
	                    	else error "ERROR"
 where
 	 	perevoD2::Integer->String->Integer->Integer->String
 	 	perevoD2 base ("") s a =  show s
 	 	perevoD2 base (x:xs) s a = if  (((fromEnum x) >= 48) && ((fromEnum x) <= 57) && (base > (toInteger(fromEnum x ) - 48))) then perevoD2 base (xs) (s + (a*(toInteger((fromEnum x) - 48)))) (a*base)
	                               else if (((fromEnum x)  >= 97 ) && ((fromEnum x) <= 122) && (base > (toInteger( fromEnum x ) - 87 ))) then  perevoD2 base (xs) (s + (a*(toInteger((fromEnum x)-87)))) (a*base)
	                               else if (((fromEnum x)  >= 65 ) && ((fromEnum x) <= 90) && (base > (toInteger(fromEnum x) - 29))) then  perevoD2 base (xs) (s + a*(toInteger((fromEnum x) - 29))) (a*base)
	                               else perevoDT (x:xs) 0 
	                               	   where 
	                               		 perevoDT::String->Integer->String
	                               		 perevoDT (x:xs) e = if ((toInteger((fromEnum x ) - 48) >1 )) then error "ERRORI????!" else if (xs == "") then (show (e)) else perevoDT (xs) (e + 1)

	                    	 



fromDecimal::Char->String->String
fromDecimal base snumber = if ((fromEnum (base) > 48) && (fromEnum (base)  <= 57)) then perevoD3 (fromEnum (base) - 48) (read snumber::Int) []   
                           else if ( (fromEnum (base)  >= 97 ) && (fromEnum (base)  <= 122) ) then perevoD3 (fromEnum (base) - 87)  (read snumber::Int) []
                           else if ((fromEnum (base)  >= 65 ) && (fromEnum (base) <= 90)) then perevoD3 (fromEnum (base)-29) (read snumber::Int ) []                       
                           else error "ERRROR"
                            where
                             perevoD3::Int->Int->[Char]->String
                             perevoD3 base 0 cs = cs
                             perevoD3 base chislo cs = if (base == 1) then perevoDT chislo  [] 
                             	                       else          
                             	                       if (mod chislo base <= 9) then perevoD3 base  (div chislo base)  ((toEnum ((mod chislo base) + 48)::Char):cs)
                                                       else
                                                       if ((mod chislo base >= 10) && (mod chislo base <= 35)) then perevoD3 base  (div chislo base)  ((toEnum ((mod chislo base) + 87)::Char):cs)
                                                       else
                                                       if ((mod chislo base >= 36) && (mod chislo base <= 61)) then (perevoD3 (base)  (div chislo base)  ((toEnum ((mod chislo base) + 29)::Char):cs))
                                                       else if (base == 1) then perevoDT chislo  [] else error "ERROR"
                                                          where  
                                                            perevoDT::Int->[Char]->String
                                                            perevoDT chislo  cs = if (chislo == -1) then (cs) else  perevoDT (chislo - 1)   ((toEnum (49)::Char):cs)





convertFromTo::Char->Char->String->String
convertFromTo fr to snumber = fromDecimal to (toDecimal fr snumber)
 where 
  
  fromDecimal::Char->String->String
  fromDecimal base snumber = if ((fromEnum (base) > 48) && (fromEnum (base)  <= 57)) then perevoD3 (fromEnum (base) - 48) (read snumber::Int) []   
                                 else if ( (fromEnum (base)  >= 97 ) && (fromEnum (base)  <= 122) ) then perevoD3 (fromEnum (base) - 87)  (read snumber::Int) []
                                 else if ((fromEnum (base)  >= 65 ) && (fromEnum (base) <= 90)) then perevoD3 (fromEnum (base)-29) (read snumber::Int ) []                       
                                 else error "ERRROR"
                                  where
                                   perevoD3::Int->Int->[Char]->String
                                   perevoD3 base 0 cs = cs
                                   perevoD3 base chislo cs = if (base == 1) then perevoDT chislo  [] 
                             	                             else          
                             	                             if (mod chislo base <= 9) then perevoD3 base  (div chislo base)  ((toEnum ((mod chislo base) + 48)::Char):cs)
                                                             else
                                                             if ((mod chislo base >= 10) && (mod chislo base <= 35)) then perevoD3 base  (div chislo base)  ((toEnum ((mod chislo base) + 87)::Char):cs)
                                                             else
                                                             if ((mod chislo base >= 36) && (mod chislo base <= 61)) then (perevoD3 (base)  (div chislo base)  ((toEnum ((mod chislo base) + 29)::Char):cs))
                                                             else if (base == 1) then perevoDT chislo  [] else error "ERROR"
                                                             where  
                                                              perevoDT::Int->[Char]->String
                                                              perevoDT chislo  cs = if (chislo == -1) then (cs) else  perevoDT (chislo - 1)   ((toEnum (49)::Char):cs)
  toDecimel::Char->String->String
  toDecimel base snumber = if ( (fromEnum (base) > 48) && (fromEnum (base)  <= 57)) then  perevoD2 (toInteger(fromEnum (base) - 48)) (reverse(snumber)) 0 1 
	                        else if ( (fromEnum (base)  >= 97 ) && (fromEnum (base)  <= 122)) then ( perevoD2 (toInteger(fromEnum (base) - 87)) (reverse (snumber)) 0 1) 
	                        else if ((fromEnum (base)  >= 65 ) && (fromEnum (base) <= 90)) then ( perevoD2 (toInteger(fromEnum (base) - 29)) (reverse (snumber)) 0 1 )	
	                        else error "ERROR"
    where
 	 	perevoD2::Integer->String->Integer->Integer->String
 	 	perevoD2 base ("") s a = show (s)
 	 	perevoD2 base (x:xs) s a = if  (((fromEnum x) >= 48) && ((fromEnum x) <= 57) && (base > (toInteger(fromEnum x ) - 48))) then perevoD2 base (xs) (s + (a*(toInteger((fromEnum x) - 48)))) (a*base)
	                               else if (((fromEnum x)  >= 97 ) && ((fromEnum x) <= 122) && (base > (toInteger( fromEnum x ) - 87 ))) then  perevoD2 base (xs) (s + (a*(toInteger((fromEnum x)-87)))) (a*base)
	                               else if (((fromEnum x)  >= 65 ) && ((fromEnum x) <= 90) && (base > (toInteger(fromEnum x) - 29))) then  perevoD2 base (xs) (s + a*(toInteger((fromEnum x) - 29))) (a*base)
	                               else perevoDT (x:xs) 0 
	                               	   where 
	                               		 perevoDT::String->Integer->String
	                               		 perevoDT (x:xs) e = if ((toInteger((fromEnum x ) - 48) >1 )) then error "ERRORI????!" else if (xs == "") then (show (e)) else perevoDT (xs) (e + 1)                                                              



                                                                
                             	                       
                                                    
                                                                
                    	 

 	 	
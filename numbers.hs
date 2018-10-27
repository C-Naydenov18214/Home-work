import Data.Char
toDecimal::Int->String->String
toDecimal base snumber | ((base > 1) && (base <=61)) = perevoD2 base snumber 
                       | (base == 1) = perevoDT snumber   
                       | otherwise = error "NE VERNUE DANNIE"
 where
  perevoD2::Int->String->String 
  perevoD2 base snumber | any (\x -> (transf x) >= base) snumber  = error "ER1"   
                        | otherwise = show (div (foldl (\sum x -> base * (sum+(transf x))) 0 snumber) base)
                         where
                           transf::Char->Int
                           transf c | (ord '0' <= ord c) && (ord c <= ord '9') = ord c - ord '0'
                                    | (ord 'a' <= ord c) && (ord c <= ord 'z') = ord c - 87
                                    | (ord 'A' <= ord c) && (ord c <= ord 'Z') = ord c - 29                
                                    | otherwise = error "ER2"
  perevoDT::String->String
  perevoDT snumber | all (== '1') snumber = show (length snumber - 1)              
                   | otherwise = error "ERROR"


fromDecimal::Int->String->String
fromDecimal base snumber | (base >1) && (base <=61)= perevoD3 base (read snumber)
                         | 1 == base = perevoDT snumber
                         | otherwise = error "ERRORORORORORORORO"
 where
   perevoDT::String->String
   perevoDT snumber = replicate (read snumber + 1) '1'
   perevoD3::Int->Int->String
   perevoD3 base 0  = ""
   perevoD3 base snumber = perevoD3 base (div snumber base) ++ ([transfToChr (mod snumber base)])
  
                    where  
                     transfToChr::Int->Char
                     transfToChr b | '0' <= chr (b +  ord '0') && chr (b + ord '0') <= '9' = chr (b + ord '0')
                                   | 'a' <= chr (b + 87) && chr (b + 87) <= 'z' = chr (b + 87)
                                   | 'A' <= chr (b + 29) && chr (b + 29 ) <= 'Z' = chr (b + 29) 




convertFromTo::Int->Int->String->String
convertFromTo fr to snumber = fromDecimal to (toDecimal fr snumber)
                                                                
                             	                       
                                                    
                                                                
                    	 

 	 	
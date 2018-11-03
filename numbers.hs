import Data.Char
toDecimal::Integer->String->String
toDecimal base snumber | ((base > 1) && (base <=61)) = translate base snumber 
                       | (base == 1) = translateToThuring snumber   
                       | otherwise = error "INVALID DATA (base > 61 or base < 1)"
 where
  translate::Integer->String->String 
  translate base snumber | any (\x -> (transf x) >= toInteger(base)) snumber  = error "INVALID DATA in the number some numeral >= base"   
                         | otherwise = show ( foldl (\sum x -> base * sum+transf x) 0 snumber) 
                          where
                           transf::Char->Integer
                           transf c | (ord '0' <= ord c) && (ord c <= ord '9') = toInteger(ord c - ord '0')
                                    | (ord 'a' <= ord c) && (ord c <= ord 'z') = toInteger(ord c - 87)
                                    | (ord 'A' <= ord c) && (ord c <= ord 'Z') = toInteger(ord c - 29 )               
                                    | otherwise = error "INVALID DATA in the number some numeral >= base"
  translateToThuring::String->String 
  translateToThuring snumber | all (== '1') snumber = show (length snumber - 1)              
                             | otherwise = error "INVALID DATA some numerdl > 1"


fromDecimal::Integer->String->String
fromDecimal base snumber | (base >1) && (base <= 61) = translateFrom base (read snumber)
                         | 1 == base = translateToThuring snumber
                         | otherwise = error "INVALID DATA some numerdl > 1"
 where
   translateToThuring::String->String
   translateToThuring snumber = replicate (read snumber + 1) '1'
   translateFrom::Integer->Integer->String
   translateFrom base 0  = ""
   translateFrom base snumber = translateFrom (base) (div (snumber) (base)) ++ ([transfToChr (mod  (snumber) (base))])
  
                    where  
                     transfToChr::Integer->Char
                     transfToChr b | '0' <= chr (fromInteger (b + 48)) && chr (fromInteger (b + 48)) <= '9' = chr (fromInteger (b + 48))
                                   | 'a' <= chr (fromInteger (b + 87)) &&  chr (fromInteger (b + 87)) <= 'z' = chr (fromInteger (b + 87))
                                   | 'A' <= chr (fromInteger (b + 29)) &&  chr (fromInteger (b + 29 )) <= 'Z' = chr (fromInteger  (b + 29)) 
                                   | otherwise = error "INVALID DATA base > 61 or base < 1"

convertFromTo::Integer->Integer->String->String
convertFromTo fr to snumber = fromDecimal to (toDecimal fr snumber)
                                                                
                             	                       
                                                    
                                                                
                    	 

 	 	
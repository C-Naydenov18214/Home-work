module MyHeap
 where
   import Data.List
   heapsort::Ord a =>[a]->[a]
   heapsort xs = heapRel xs (length xs)
    where 
 	  heapRel::Ord a =>[a]->Int->[a]
 	  heapRel xs len | len == 1 = [head xs]
 	                 | otherwise = sort ((fromInteger (div (toInteger(len)) 2 )) - 1) xs 
 	                  where 
 	                	 sort::Ord a =>Int->[a]->[a]
 	                	 sort len2 xs | len2 > -1 = sort (len2 - 1) (take right (take left ((take len2 xs) ++ [head midl] ++ (drop (len2 + 1) xs)) ++ [midl !! 1] ++ drop (left + 1) ((take len2 xs) ++ [head midl] ++ (drop (len2 + 1) xs))) ++ last1 ++ drop (right + 1) (take left ((take len2 xs) ++ [head midl] ++ (drop (len2 + 1) xs)) ++ [midl !! 1] ++ drop (left + 1) ((take len2 xs) ++ [head midl] ++ (drop (len2 + 1) xs )))) 
 	                	              | otherwise = end xs
 	                	               where
 	                	              	last1 = if (length (midl) == 2) then [] else [last midl]
 	                	              	   
 	                	              	midl = if (length xs) == right then swap (xs !! max) (xs !! left) else swap2 (xs !! max) (xs !! left) (xs !! right) 
 	                	              	    
 	                	              	      where
 	                	              	       swap::Ord a =>a->a->[a]
 	                	              	       swap a b | a >= b = [a] ++ [b]
 	                	              	                | a <= b = [b] ++ [a]
 	                	              	       swap2::Ord a =>a->a->a->[a]
 	                	              	       swap2 a b c | (a >= b) && (a >= c) = [a] ++ [b] ++ [c] 
 	                	              	                   | (b >= a) && (b >= c) = [b] ++ [a] ++ [c]
 	                	              	                   | (c >= a) && (c >= b) = [c] ++ [a] ++ [b] 
 	                	                left = len2 * 2 + 1
 	                	              	right = len2 * 2 + 2
 	                	              	max = len2
 	                	              	end::Ord a =>[a]->[a]
 	                	                end xs = (heapRel  ([last xs] ++ drop 1 (init xs)) (len - 1)) ++ (take 1 xs)  
 	                	                                     

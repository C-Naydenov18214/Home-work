fact n = product [1..n]
product::Num a=>[a]->a

product [] = 1
product (x:xs) = x * product xs

--------------
foldl нач  знач f список
ПРИМЕР
foldl (+) 0 [1..n] сумма до n

foldr (&&) True replicate False

replicate::Int->a->

-----------------------------
СОРТИРОВКА
BubbleSort, QuickSort, HeapSort......

QuickSort
[1,8,7,9,100500,322]
1 -- опорный элемент (первый элемент)
              [x,x<1]   ++[1]++ [x,x>1]
              --------------------------
                      
quick::[a]->[a]
quick [] = []
quick (x:xs) = quick [y | y <- xs, x =< y] ++ [x] ++ quick [y | y<-xs,y>x]
O(N*logN)

--------
BubbleSort
bubble:: Ord a=> [a]->[a]
bubble [] = []
bubble (x:[]) = x:[]
bubble [x:y:xs] = if x>y then y:(bubble $ x:xs) else x: ( bubble $ y:xs)
bubbleSort xs = helper (lenght) xs
            where
            	helper n ys = if n==0 then ys else helper (n-1) (bubble ys)     
	[8,1,0,3]
(x,y)	8:1 [0,3](xs) ----> 1:[8,0,3]---->[1,8,0,3]

________________________________________________
получим аскии код а
import Data.Char
ord 'a'
97
________________________________________________
МОДУЛИ

import Data.Char
________________________________________________

caeserEncode::Int->String->String
caeserEncode offset word = map (\x->chr (ord x + offset )) str

caeserDecode offset str = caeserEncode (- offset) str
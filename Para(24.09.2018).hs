isSorted::Ord a => [a]->Bool ----------------- CОРТИРОВКА
isSorted []= True
isSorted (x:[]) = True
isSorted (x:y:xs) = if (x > y)
	then Fasle else isSorted y:xs

	_________________________
	ГЕНЕРАТОР СПИСКОВ 
	[2*x | x<-[1..10]]
	_________________________
	МИНИМИЗАЦИЯ 
	opMin f predicate = helper 0
	                    where helper arg = if predicate (f arg) == Fasle
	                                       then arg
	                                       else helper arg+1
	                                       
___________________________________________________________________________

ДЛИНА СПИСКА
len::[a]->Integer
len [] = 0
len (x:xs) = 1 + len xs
___________________________________________________________________________
len'::[a]->Integer
len' xs = helper 0 xs where
	      helper 1 [] = 1
	      helper 1 (x:xs) = helper (1+1) xs


	      len'[1,2,3]
	      helper 0[1,2,3]
	      helper 1 [2,3]
	      helper 2 [3]
	      helper 3 [0]
________________________________________________________________
СУММА ДЕЛИТЕЛЕЙ
sumDelimeters n = sum (filter (\x-> n `mod` x == 0) [1..n])
________________________________________________________________

foldl -- подаем функции и нач зна аккамулятора 
foldl (+) 0 [1,2,3]  поочереди складывает в аккумулятор 
=>6
foldr -||- только с конца
________________________________________________________________



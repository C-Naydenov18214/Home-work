---1)
get (x:xs) n = if (n == 0) then  x else  get (xs) (n-1) 

--2)
hea  (x:xs) = x

--3) last
ls (x:xs) = if (xs == []) then  x else ls (xs)

--4) tail
f (x:xs) = xs 

--5)
 
ini [x] = []
ini (x:xs) =  x:(ini(xs)) 	
--6)reverse
rev [] = []
rev (x:xs) = rev2 (x:xs) [] 
             where
             	rev2 (x:xs) as = if (xs == []) then x:as else rev2 (xs) (x:as)
--7) length
len [] = 0
len (x:xs) = len (x:xs) 1
 where 
 	len (x:xs) n = if (xs == []) then n else len (xs) (n+1)  
--8) app 
app (x:xs) n = rev (n:rev(x:xs))
--9) con 
con::[a]->[a]->[a]
con (x:[]) (ys) = x:ys
con (x:xs) (ys) = x:(con (xs) (ys))
--10) Drop
dro::Integer -> [a] -> [a]
dro 0 xs = xs 
dro n (x:xs) = dro (n-1) (xs) 
--11) take n xs
take1::Integer->[a]->[a]
ys = []
take1 1 (x:xs) = x:ys
take1 n (x:xs) = x:(take1 (n-1) (xs) )
--12)
split::Integer->[a]->([a],[a]) 
split n [] = ([],[])
split n (x:xs) = (take1 (n-1) (x:xs) , dro (n-1) (x:xs))
--13)
nul::[a]->Bool
nul [] =  True
nul xs = False
--14)Elem
el:: Eq a=>a->[a]->Bool
el n [] = False
el n (x:xs) = if (x == n) then True else el n (xs)
--15) filter test xs 
fil::(a->Bool)->[a]->[a]
fil test [] = []
fil test (x:xs) = if (test (x) == True) then x:(fil test (xs)) else fil test (xs)
--16)
mapp::(a->b)->[a]->[b]
mapp f [] = []
mapp f (x:xs) = f x:(mapp f xs)
--17) zip xs xy
zipp::[a]->[b]->[(a,b)]
zipp [] [] = []
zipp [] xs = []
zipp ys [] = []
zipp (x:xs) (y:ys) = (x,y):(zipp (xs) (ys)) 
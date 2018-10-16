--1
get [] n = []
get (x:xs) n = if (n == 0) then  x else  get (xs) (n-1) 

--2
hea [] = error "EMPTY LIST"
hea  (x:xs) = x

--3
tail' [] = error "EMPTY LIST"
tail' (x:xs) = xs

--4
last' [] = error "EMPTY LIST"
last' (x:[]) = x
last' (x:xs) = last'(xs)  

--5
rev [] = []
rev (x:xs) = rev2 (x:xs) [] 
             where
             	rev2 (x:[]) as = x:as 
                rev2 (x:xs) as = rev2 xs (x:as)

--6
len []  = 0
len (x:xs) = len' (x:xs) 0
len' [] n = n
len' (x:xs) n = len' (xs) (n+1)  

--7
app (x:xs) n = app' (x:xs) n 
 where
   app' (x:[]) n = x:(n:[])                
   app' (x:xs) n = x:(app (xs) n)  

--8
con::[a]->[a]->[a]
con (x:[]) (ys) = x:ys
con [] [] = error "EMPTY LIST"
con (x:xs) (ys) = x:(con (xs) (ys))

--9
dro::Integer -> [a] -> [a]
dro  _ [] = []
dro 0 xs = xs 
dro n (x:xs) = dro (n-1) (xs) 

--10
take1::Integer->[a]->[a]
take1 _ [] = []
take1 1 (x:xs) = x:[]
take1 n (x:xs) = x:(take1 (n-1) (xs))

--11
split::Integer->[a]->([a],[a]) 
split n [] = ([],[])
split n xs = (take1 (n-1) (xs) , dro (n-1) (xs))


--12
nul::[a]->Bool
nul [] =  True
nul xs = False

--13
el:: Eq a=>a->[a]->Bool
el n [] = False
el n (x:xs) = if (x == n) then True else el n (xs)

--14

fil::(a->Bool)->[a]->[a]
fil test [] = []
fil test (x:xs) = if (test (x) == True) then x:(fil test (xs)) else fil test (xs)

--15
mapp::(a->b)->[a]->[b]
mapp f [] = []
mapp f (x:xs) = f x:(mapp f xs)

--16
zipp::[a]->[b]->[(a,b)]
zipp [] [] = []
zipp [] xs = []
zipp ys [] = []
zipp (x:xs) (y:ys) = (x,y):(zipp (xs) (ys)) 

--17

init' [] = error "ERROR"
init' (x:[]) = []
init' (x:xs) =  x:(init xs) 

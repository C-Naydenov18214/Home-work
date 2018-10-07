mapp::(a->b)->[a]->[b]
mapp f xs = foldr (\a as -> f (a):as) [] xs 

mapp2::(a->b)->[a]->[b]
mapp2 f xs = foldl (\a x -> a ++ [f x]) [] xs

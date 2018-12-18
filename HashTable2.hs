import Data.Char
import System.IO
import System.Environment
import Control.Monad
import Data.List
--data Maybee a = Nothing_Nothing | Just_Just a
data HashTable k v = Table [[(k,v)]] Integer Integer deriving Show
--  Возвращает пустую хэштаблицу
defaultHashTable::Integer -> HashTable k v
defaultHashTable newlen = Table (replicate (fromInteger newlen) []) newlen newlen


hashFunc::String->Int->Int
hashFunc [] 0 = 0
hashFunc (x:xs) len = (ord x)*(len `mod` (ord x)) `mod` (abs(len - ord x)) + (hashFunc xs (len-1))

fromList::(Show k, Eq k)=>[(k,v)]->HashTable k v 
fromList xs = inputElems (defaultHashTable (toInteger 5)) xs
 where
		inputElems::(Show k, Eq k)=>HashTable k v->[(k,v)]->HashTable k v
		inputElems (Table xs free len) [] = Table xs free len
		inputElems (Table xs free len) ((k,v):ys) = inputElems (Table ((take ((hashFunc (show k) (length $ show k)) `mod` (fromInteger len)) xs) ++ [(k,v):(xs !! ((hashFunc (show k) (length $ show k)) `mod` (fromInteger len)))] ++  (drop (((hashFunc (show k) (length $ show k)) `mod` (fromInteger len))+1) xs))  (free-1) len) ys

-- -- // Удаляет элемент по заданному ключу.
erase::(Show k, Eq k)=>HashTable k v->k->HashTable k v
erase (Table list free len) find = Table ((take ((hashFunc (show find) (length $ show find)) `mod` (fromInteger len))  list) ++ [clearV find (list !! ((hashFunc (show find) (length $ show find)) `mod` (fromInteger len)))] ++ (drop (((hashFunc (show find) (length $ show find)) `mod` (fromInteger len))+1) list)) (free+(checkLen find (list !! ((hashFunc (show find) (length $ show find)) `mod` (fromInteger len))))) len
	where
		clearV::(Eq k)=>k->[(k,v)]->[(k,v)]
		clearV find [] = []
		clearV find ((k,y):xs) = if find == k then clearV find xs else (k,y):(clearV find xs)  
		checkLen::(Eq k)=>k->[(k,v)]->Integer
		checkLen find [] = 0
		checkLen find ((k,v):xs) = if find == k then (checkLen find xs) + 1 else checkLen find xs 

insertT::(Show k, Eq k) => HashTable k v -> k -> v -> HashTable k v
insertT (Table list free len) key val |free > 0 = (Table ((take ((hashFunc (show key) (length $ show key)) `mod` (fromInteger len))  list) ++ [(key,val):(list !! ((hashFunc (show key) (length $ show key)) `mod` (fromInteger len)))] ++ (drop (((hashFunc (show key) (length $ show key)) `mod` (fromInteger len))+1) list)) (free-1) len)  
                                      |otherwise = insertT (rehash (Table list free len)) key val
                                        where
                                         rehash::(Show k, Eq k) => HashTable k v -> HashTable k v 
                                         rehash (Table list free len) = (newTable list (defaultHashTable (len*2)))
                                         newTable::(Show k, Eq k) => [[(k,v)]] -> HashTable k v -> HashTable k v
                                         newTable [] (Table newlist free len) = (Table newlist free len)
                                         newTable (xs:list) (Table newlist free len) = newTable list (inputElems1 (Table newlist free len) xs ) 
                                         inputElems1::(Show k, Eq k)=>HashTable k v->[(k,v)]->HashTable k v
                                         inputElems1 (Table xs free len) [] = (Table xs free len)
                                         inputElems1 (Table xs free len) ((k,v):ys) = inputElems1 (Table ((take ((hashFunc (show k) (length $ show k)) `mod` (fromInteger len)) xs) ++ [(k,v):(xs !! ((hashFunc (show k) (length $ show k)) `mod` (fromInteger len)))] ++  (drop (((hashFunc (show k) (length $ show k)) `mod` (fromInteger len))+1) xs))  (free-1) len) ys

-- -- // Проверка наличия значения по заданному ключу.
contains::(Show k, Eq k)=>HashTable k v->k->Bool
contains (Table xs free len) key = check (xs !! ((hashFunc (show key) (length $ show key)) `mod` (fromInteger len))) key 
                                    where 
                                     	check::(Show k, Eq k) => [(k,v)]-> k -> Bool
                                     	check [] key = False
                                     	check ((k,v):xs) key | k == key = True
                                     	                     | otherwise = check xs key  
 
  

-- -- // Возвращает значение по ключу. Бросает исключение при неудаче.
at::(Show k, Eq k) => HashTable k v -> k -> v
at (Table xs free len) k = checkVal (xs !! ((hashFunc (show k) (length $ show k)) `mod` (fromInteger len))) k
  where
   checkVal::(Show k,Eq k) => [(k,v)] -> k -> v
   checkVal [] key = error "Empty :("
   checkVal ((k,v) : xs) key | k == key = v
                             | otherwise = checkVal xs key

size::(Show k, Ord k)=>HashTable k v -> Integer
size (Table xs free len) = len - free

empty::(Show k, Ord k)=>HashTable k v -> Bool
empty (Table xs free len) |len == 0 = True
                          |otherwise = False



-- main = do        
--        handle <- openFile "input.txt" ReadMode
--        contents <- hGetContents handle
--        handle2 <- openFile "output.txt" WriteMode
--        fromList contents 
--        hClose handle2
--        hClose handle 



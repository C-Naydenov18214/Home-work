import Data.Char
import Data.List

data Elem k v = Same | Elem (k,v) [Elem k v] deriving (Show, Eq, Ord)

data HashTable k v = Table [Elem k v] Integer deriving Show
--  Возвращает пустую хэштаблицу
defaultHashTable:: HashTable k v
defaultHashTable = Table [] 0

hashFunc::String -> Int -> Integer
hashFunc [] _ = 0
hashFunc (x:xs) len = (toInteger(ord x)) * (toInteger(len + (2* len-1)))  + (hashFunc xs (length xs))

--  конструирует хэштаблицу из списка пар, удовлетворяющим всем условиям.
--Это единственные доступные для пользователя конструкторы. Все остальные вариации получаются путем добавления\удаления элеменьов из уже существующих таблиc 
fromList::(Show k, Ord k)=>[(k,v)]->HashTable k v 
fromList xs = Table (inputElems xs) (toInteger(length xs))
	where
		inputElems::(Show k, Ord k)=>[(k,v)]->[Elem k v]
		inputElems [] = []
		inputElems ((k,v):xs) = (Elem (k,v) []):(inputElems xs)

-- // Очищает контейнер.
clear::HashTable k v -> HashTable k v;
clear a = Table [] 0

-- // Удаляет элемент по заданному ключу.
erase::(Show k, Ord k)=>HashTable k v->k->HashTable k v
erase (Table xs len) k = Table (delete xs k) (len-1)
	where
		delete::(Show k,Ord k)=>[Elem k v]->k->[Elem k v]
		delete [] _ = []
		delete ((Elem (k,v) next):xs) find = if hashFunc (show $ k) (length $ show $ k) == hashFunc (show $ find) (length $ show $ find) then xs else (Elem (k, v) next):(delete xs find)

-- // Вставка в контейнер.
insertT::(Show k, Ord k)=>HashTable k v->k->v->HashTable k v
insertT (Table xs len) k v = Table (input xs k v) (len+1)
	where
		input::(Show k, Ord k)=>[Elem k v]->k->v->[Elem k v]
		input [] find value = [Elem (find, value) []] 
		input ((Elem (k,v) next):xs) find value | hashFunc (show $ k) (length $ show $ k) == hashFunc (show $ find) (length $ show $ find) && k == find = (Elem (k,v) ((Elem (find,value) [Same]):next)):xs
		                                        | hashFunc (show $ k) (length $ show $ k) < hashFunc (show $ find) (length $ show $ find) && k < find = (Elem (k,v) next):(input xs find value)
		                                        | hashFunc (show $ k) (length $ show $ k) > hashFunc (show $ find) (length $ show $ find) && k > find = (Elem (find, value) []):(Elem (k,v) next):xs


-- // Проверка наличия значения по заданному ключу.
contains::(Show k, Ord k) => HashTable k v->k->Bool
contains (Table xs len) k = search xs k
	where
		search::(Show k, Ord k) => [Elem k v]->k->Bool
		search [] _ = False
 		search ((Elem (k,v) next):xs) find | hashFunc (show $ k) (length $ show $ k) == hashFunc (show $ find) (length $ show $ find) && k == find = True
 										   | otherwise = search xs find


  

-- // Возвращает значение по ключу. Бросает исключение при неудаче.
at::(Show k, Ord k)=>HashTable k v -> k ->v
at (Table xs len) k = val xs k
	where
		val::(Show k, Ord k)=>[Elem k v]->k->v
		val [] _ = error "Empty :("
		val ((Elem (k,v) next):xs) find | hashFunc (show $ k) (length $ show $ k) == hashFunc (show $ find) (length $ show $ find) = v 
		                                | otherwise = val xs find
  
size::(Show k, Ord k)=>HashTable k v -> Integer
size (Table xs len) = len

empty::(Show k, Ord k)=>HashTable k v -> Bool
empty (Table xs len) = if len == 0 then True else False

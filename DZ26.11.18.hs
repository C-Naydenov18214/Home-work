import MyHeap
import Data.List
main = do 
 text <- getLine
 if text /= "" then printf $ heapsort $ words (text) else main
    	where
    		printf::[String]-> IO ()
    		printf (x:xs) | xs == [] = putStrLn (show x)
    		              | otherwise = (putStrLn $ show x) >> printf xs
import MyHeap
import Data.List
import Control.Monad
import Control.Exception
import System.IO
import Data.Char 
main = do
       handle <- openFile "input.txt" ReadMode
       contents <- hGetContents handle
       handle2 <- openFile "output.txt" WriteMode
       let contents2 = sort contents
       let contents = dropWhile (\x -> x == ' ') contents2
       let count a = if (a == []) then [] else (take 1 a ++ " - " ++ (show (length (takeWhile (== head a) a)))):count(dropWhile (== head a) a)
       mapM (hPutStrLn handle2) (count contents)
       hClose handle
       hClose handle2
             


        
  
 
       
    






import Data.Char

sumList list = foldr (+) 0 list

main = do
    line <- getLine 
    putStrLn (show (div (sumList (map (digitToInt) line)) (length line)))



minFromList list = foldl min (head list) list

main2 = do
    line <- getLine 
    putStrLn (show (minFromList (map (digitToInt) line)))
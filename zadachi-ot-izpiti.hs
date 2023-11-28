import Data.Char

sumList list = foldr (+) 0 list

main = do
    line <- getLine
    putStrLn (show (div (sumList (map (digitToInt) line)) (length line)))

minFromList list = foldl min (head list) list

main2 = do
    line <- getLine
    putStrLn(show (minFromList (map (digitToInt) line)))

main3 = do
    firstLine <- getLine
    secondLine <- getLine
    let first = read firstLine :: Integer
    let second = read secondLine :: Integer
    let fibX = fibonacci first
    let facY = factorial second
    print (max fibX facY)

factorial x =
    if x == 1 then 1
    else x * factorial (x-1)

fibonacci x =
    if x == 1 || x == 2
        then 1
        else fibonacci (x-1) + fibonacci (x-2)

main4 = do
    line <- getLine
    let n = read(line)
    symbol <- getLine
    triangle n symbol

draw n line symbol =
    if n <= 0
        then line
        else draw (n-1) (line ++ symbol) symbol

triangle n symbol =
    if (n > 0)
        then do
            let str = ""
            let result = draw n str symbol
            putStrLn(result)
            triangle (n-1) symbol
        else
            return()

main5 = do
    line1 <- getLine
    line2 <- getLine
    line3 <- getLine
    line4 <- getLine
    let a1 = read line1 :: Integer
    let a2 = read line2 :: Integer
    let a3 = read line3 :: Integer
    let a4 = read line4 :: Integer
    print (max (max a1 a2) (max a3 a4) - min (min a1 a2) (min a3 a4))
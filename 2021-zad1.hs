main = do
    firstLine <- getLine
    secondLine <- getLine
    let first = read firstLine :: Integer
    let second = read secondLine :: Integer
    let fibX = fibonacci1 first
    let facY = factorial1 second
    print (max fibX facY)
factorial1 x = 
    if x == 1 then 1
    else x * factorial1 (x-1)

fibonacci1 x = 
    if x == 1 || x == 2
        then 1
        else fibonacci1 (x-1) + fibonacci1 (x-2)



createTrigLine n char = replicate n char

printTrig n char = 
    if n > 0
    then do
        putStrLn (createTrigLine n char)
        printTrig (n-1) char
    else return ()

main2 = do
    inputN <- getLine
    char <- getChar
    let n = read inputN :: Int
    printTrig n char



type TriangleMaker = Char -> Int -> [String]

topLeftTriangle :: TriangleMaker
topLeftTriangle c n = [replicate i c | i <- [n, n-1 .. 1]]

main :: IO ()
main2b = do
  size <- getLine
  symbol <- getChar
  putStr $ unlines $ topLeftTriangle symbol (read size :: Int)



main2a = do
    line <- getLine
    let n = read(line)
    symbol <- getLine
    triangle n symbol

draw n line symbol =
    if n <= 0
        then line
    else
        draw (n - 1) (line ++ symbol) symbol

triangle n symbol =
    if (n > 0)
        then do
        let str = ""
        let result = draw n str symbol
        putStrLn(result)
        triangle (n - 1) symbol
        else return ()





fac x
    | x > 0 = x * fac (x-1)
    | otherwise = 1
fib x
    | x <= 2 = 1
    | x > 1 = fib (x - 1) + fib (x - 2)
main1 = do
    inputX <- getLine
    inputY <- getLine
    let x = read inputX  :: Integer
    let y = read inputY  :: Integer
    let fibX = fib x
    let facY = fac y
    print (max fibX facY)



main31 = do
    input1 <- getLine
    input2 <- getLine
    input3 <- getLine
    input4 <- getLine
    let a1 = read input1  :: Integer
    let a2 = read input2  :: Integer
    let a3 = read input3  :: Integer
    let a4 = read input4  :: Integer
    print (max (max a1 a2) (max a3 a4) - min (min a1 a2) (min a3 a4))


main32 = do
    line <- getLine
    let num = read line :: Integer
    print(sumNumbersLoop 0 sumOfDigit (absoluteValue num))

absoluteValue :: Integer -> Integer
absoluteValue n | n >= 0 = n
                | otherwise  = -n

sumOfDigit num =
    if num == 0
        then num
        else (mod num 10 + sumOfDigit (div num 10))

sumNumbersLoop sum index = 
    if index < 10
    then index
    else 0 + (sumNumbersLoop 0 sumOfDigit (index))
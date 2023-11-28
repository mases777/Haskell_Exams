import System.Exit

main11 = do
    putStrLn "Hello World"

main = do
    firstLine <- getLine
    secondLine <- getLine
    putStrLn(firstLine ++ " " ++ secondLine)

dummyGetLine :: IO String
dummyGetLine =
    return "I'm not really doing anything"

repeatString str n = 
    if n == 0
    then ""
    else str ++ (repeatString str (n-1))

sumNumbers = sumNumbersLoop 0 1

sumNumbersLoop sum index = 
    if index > 10
    then sum
    else (sum + index) + (sumNumbersLoop sum (index + 1))



simpleFunction a = 
    if a == 5
    then "It's five :)"
    else if a == 6 
        then "It's six :)"
        else "It's neither 5 nor 6 :("

main2 = do
    firstLine <- getLine
    secondLine <- getLine
    let result = read firstLine * read secondLine
    print(result)

main3 = do
    radius <- getLine
    let area = pi * read radius * read radius
    print(area)

main4 = do
    x1 <- getLine
    y1 <- getLine
    x2 <- getLine
    y2 <- getLine
    x <- getLine
    y <- getLine
    let x1Point = read x1 :: Integer
    let x2Point = read x2 :: Integer
    let y1Point = read y1 :: Integer
    let y2Point = read y2 :: Integer
    let xPoint = read x :: Integer
    let yPoint = read y :: Integer
    if (xPoint >= x1Point && xPoint <= x2Point)
        then if (yPoint >= y1Point && yPoint <= y2Point)
            then putStrLn "INSIDE"
            else putStrLn "OUTSIDE"
        else putStrLn "OUTSIDE"

doubleVal x = 2 * x

isEven x = do
    if (x `mod` 2 == 0)
        then True
        else False

biggestOf3 a b c = do
    if (a > b)
        then if (a > c)
            then a
            else c
        else if (b > c)
            then b
            else c

add1 x = x + 1
remove1 x = x - 1
execute f a = f a

factorial1 x = do
    if x == 1 then 1
    else x * factorial1 (x-1)

fibonacci1 x = do
    if x == 1 || x == 2
        then 1
        else fibonacci1 (x-1) + fibonacci1 (x-2)


reserveList [] = []
reserveList (x:xs) = reserveList xs ++ [x]

listLength [] = 0
listLength list = findLength 1 list
findLength length list =
    if null list
        then (length - 1)
        else findLength (length + 1) (tail list)

duplicate [] = []
duplicate (x:xs) = x:x:duplicate xs

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery list count = (take (count - 1) list) ++ dropEvery (drop count list) count

fibonacci n = findFibonacci n 1 0 1 []
findFibonacci n initialValue prevValue index resultList =
    if index > n
        then resultList
        else findFibonacci n (initialValue + prevValue) initialValue (index + 1) (resultList ++ [initialValue])

factorial n = findFactorial n 1 1 []
findFactorial n initialValue index resultList =
    if index > n
        then resultList
        else findFactorial n (initialValue * index) (index + 1) (resultList ++ [initialValue])

main5 = do
    numberToWord

numberToWord = do
    line <- getLine
    if line == "End"
        then exitWith ExitSuccess
        else do
            let number = read line :: Integer
            printNumber number
            numberToWord

printNumber number
    | number == 0 = do
        putStrLn "Zero"
    | number == 1 = do
        putStrLn "One"
    | number == 2 = do
        putStrLn "Two"
    | number == 3 = do
        putStrLn "Three"
    | number == 4 = do
        putStrLn "Four"
    | number == 5 = do
        putStrLn "Five"
    | number == 6 = do
        putStrLn "Six"
    | number == 7 = do
        putStrLn "Seven"
    | number == 8 = do
        putStrLn "Eight"
    | number == 9 = do
        putStrLn "Nine"
    | otherwise = do
        putStrLn "Please only enter single digit positive numbers"

main6 = do
    sumirane

sumirane = do
    line <- getLine
    let num = read line :: Integer
    putStrLn(show (sumOfDigit num))

sumOfDigit num =
    if num == 0
        then num
        else (mod num 10 + sumOfDigit (div num 10))

main7 = do
    line <- getLine
    let listOfNumber = read line :: Integer
    line <- getLine
    let count = read line :: Integer
    putStrLn (show (rotate (create listOfNumber) count))

create num = 
    if num == 0
        then []
        else create (div num 10) ++ [mod num 10]

rotate list n = rotateHelp list n 0

rotateHelp list n i = 
    if i == n
        then list
        else rotateHelp (tail list ++ [head list]) n (i+1)

main8 = do
    line <- getLine
    let point = read line :: Float
    putStrLn(show (point + (bonus point)))

bonus point =
    if point <= 100
        then point * 0.1
        else if point < 250
            then 50
            else if point < 1000
                then point * 0.4
                else point * 0.5

log2 1 = 0
log2 n = 1 + log2 (n `div` 2)

printTriangle 0 = return ()
printTriangle n = 
    do
        putStrLn (asterixStringRow n)
        printTriangle (n - 1)

asterixStringRow n = replicate n '*'


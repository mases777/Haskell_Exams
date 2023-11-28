main = do
    line <- getLine
    let num = read line :: Integer
    guessingGame num

guessingGame num = do
    line <- getLine
    let guessedNum = read line :: Integer
    if guessedNum > num
    then do 
        putStrLn "Too high!"
        guessingGame num
    else if guessedNum < num
    then do
        putStrLn "Too low!"
        guessingGame num
    else putStrLn "You win!"

doubleVal x = 2 * x

isEven x = do
    if x `mod` 2 == 0
        then True
        else False

biggestOf3 a b c = do
    if a > b
        then if a > c
            then a
            else c
        else if b > c
            then b
            else c

add1 x = x + 1
remove1 x = x - 1
execute f a = f a

factorial x = do
    if x == 1 then 1
    else x * factorial (x-1)

fibonacci x = do
    if x == 1 || x == 2
        then 1
        else fibonacci (x-1) + fibonacci (x-2)

fibonacci2 n = findFibonacci n 1 0 1
findFibonacci n initialValue prevValue index =
    if index >= n 
    then initialValue
    else findFibonacci n (initialValue + prevValue) initialValue (index + 1)

printTriangle 0 = return ()
printTriangle n = 
    do
        putStrLn (asterixStringRow n)
        printTriangle (n - 1)

asterixStringRow n = replicate n '*'
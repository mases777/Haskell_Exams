import Data.Char

tribonacci x =
    if x == 0 || x == 1
        then 0
        else if x == 2
            then 1
            else tribonacci (x-3) + tribonacci (x-2) + tribonacci (x-1)

main = do
    line <- getLine
    let x = read line :: Integer
    putStrLn(show(tribonacci x))


trib n=
    if n==0 || n==1
        then 0
        else if n==2
            then 1
        else trib(n-1)+trib(n-2)+trib(n-3)

main2=do
    firstLine<-getLine
    let value=read firstLine::Integer
    putStrLn(show(fib value))

fib 0 = 0
fib 1 = 0
fib 2 = 1
fib x = fib (x-1) + fib (x-2) + fib (x-3)

main3 = do
 line <- getLine
 let num = read line :: Integer
 print(fib num)




countBig::[Char]->Int

countBig [] = 0
countBig (ch:str) = if ((ord ch)>=65 && (ord ch)<=90)
                     then 1 + (countBig str)
                     else (countBig str)

countSmall::[Char]->Int
countSmall [] = 0
countSmall (ch:str) = if ((ord ch)>=97 && (ord ch)<=122)
                     then 1 + (countSmall str)
                     else (countSmall str)

countOther::[Char]->Int
countOther [] = 0
countOther (ch:str) = if ((ord ch)<65 || ((ord ch)>90 && (ord ch)<97) || (ord ch)>122)
                     then 1 + (countOther str)
                     else (countOther str)

main4 :: IO()
main4 = do
    line <- getLine    
    print (countSmall line)
    print (countBig line)
    print (countOther line)





getUpperLetters :: String -> Int
getLowerLetters :: String -> Int
getSymbolsLetters :: String -> Int


getUpperLetters text = length (filter (isUpper) text)
getLowerLetters text = length (filter (isLower) text)
getSymbolsLetters text = length text -  (getUpperLetters text + getLowerLetters text)

main5 = do
    line <- getLine
    let lowerCase = getLowerLetters line
    let upperCase = getUpperLetters line
    let symbols = getSymbolsLetters line
    print (show lowerCase++ " " ++ show upperCase ++ " " ++ show symbols)
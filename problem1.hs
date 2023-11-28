main = do
    line <- getLine
    let dalgina = (length line)
    let number = read line :: Int
    let sumD = (sumOfDigit number) 
    let result = (sumD `div` dalgina)
    print(result)

sumOfDigit num =
    if num == 0
        then num
        else (mod num 10 + sumOfDigit (div num 10))

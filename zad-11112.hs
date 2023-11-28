main = do
    line <- getLine
    let num = read line :: Int
    putStrLn(show (fromIntegral (sumOfDigit num) / fromIntegral (count num)))

sumOfDigit num =
    if num == 0
        then num
        else (mod num 10 + sumOfDigit (div num 10))
    
count num =
    if num == 0
        then 0
        else (1 + count (div num 10))

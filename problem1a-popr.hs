main = do
    line1 <- getLine
    line2 <- getLine
    let num = read line1 
    let m = read line2 
    putStrLn (show (sumOfDigit num m))

sumOfDigit 0 m = 0

sumOfDigit num m =
    if (num `mod` 10 > m)
        then (num `mod` 10 + sumOfDigit (num `div` 10) m)
        else sumOfDigit (num `div` 10) m

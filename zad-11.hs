findAverage ::IO Double
findAverage =do 
    n <- getLine
    s <- readAndSum n
    return$ (fromIntegral s) / (fromIntegraln)

readAndSum ::Int->IO Int
readAndSum 0 =return0
readAndSum n =do
    x <- getLine
    s <- readAndSum $ n - 1
    return$ x + s

sumOfDigit num =
    if num == 0
        then num
        else (mod num 10 + sumOfDigit (div num 10))

main =do
    avg <- findAverage
    putStrLn$"Среднотоаритметичное: "++show avg
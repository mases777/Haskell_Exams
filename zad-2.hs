main = do
    line <- getLine
    let num = read line :: Integer
    putStrLn(show (minimum num (mod num 10) 9))

minimum num x y =
    if x < y
        then minimum (div num 10) (mod num 10) x
        else (minimum (div num 10) (mod num 10) y)

mal y z =
    if y < z
        then y
        else z

main = do
    line <- getLine
    let number = read line :: Integer
    print(foldr1 min (digits number))

digits :: Integer -> [Int]
digits n = map (\x -> read [x] :: Int) (show n)
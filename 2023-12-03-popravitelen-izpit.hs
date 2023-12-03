repeatChars :: String -> Int -> String
repeatChars str n = concatMap (\c -> replicate n c) str

main :: IO ()
main = do
    inputStr <- getLine
    inputN <- getLine
    let n = read inputN :: Int
    let result = repeatChars inputStr n
    putStrLn result
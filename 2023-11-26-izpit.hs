import Data.Char
main :: IO ()
main = do
    size <- readLn
    mapM_ putStrLn (generateTreeLines size)

generateTreeLines :: Int -> [String]
generateTreeLines size = map (generateTreeLine size) [1..size+1]

generateTreeLine :: Int -> Int -> String
generateTreeLine totalSize currentSize =
    spaces ++ stars ++ "|" ++ stars
    where
        spaces = replicate (totalSize - currentSize+1) ' '
        stars = replicate (currentSize - 1) '*'
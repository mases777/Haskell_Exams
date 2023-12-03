drawTriangle :: Int -> IO ()
drawTriangle height
    | height <= 0 = putStrLn "Invalid value!"
    | otherwise = mapM_ putStrLn (generateTriangle height)

generateTriangle :: Int -> [String]
generateTriangle height = [line i | i <- [1..height]]
  where
    line i = spaces ++ replicateStars i
      where
        spaces = replicate (height - i) ' '
        replicateStars n = replicate (2 * n - 1) '$'

main :: IO ()
main = do
    inputHeight <- getLine
    let height = read inputHeight :: Int
    drawTriangle height
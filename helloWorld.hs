main = do
    putStrLn "Hello World"
    dummyGetLine

dummyGetLine :: IO String
dummyGetLine =
    return "I'm not really doing anything"

simpleFunction a = 
    if a == 5
    then "It's five :)"
    else if a == 6 
        then "It's six :)"
        else "It's neither 5 nor 6 :("

simpleFunction' a 
    | a == 5 = "It's five :)"
    | a == 6 = "It's six :)"
    | otherwise = "It's neither 5 nor 6 :("

simpleFunction'' a = case a of
    5 -> "It's five :)"
    6 -> "It's six :)"
    _  -> "It's neither 5 nor 6 :("
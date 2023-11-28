main = do
    print("Hello")

plusFive n = 
    if n < 0
    then (-n) + 5
    else n + 5

pass3 f = f 3
add1 x = x + 1 

compose f g x = f (g x)
add2 x = x + 1
mult2 x = 2 * x

repeatString str n = 
    if n == 0
    then ""
    else str ++ (repeatString str (n-1))

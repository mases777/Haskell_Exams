import Data.Char
countBig::[Char]->Int

countBig [] = 0
countBig (ch:str) = if ((ord ch)>=65 && (ord ch)<=90)
                     then 1 + (countBig str)
                     else (countBig str)

countSmall::[Char]->Int
countSmall [] = 0
countSmall (ch:str) = if ((ord ch)>=97 && (ord ch)<=122)
                     then 1 + (countSmall str)
                     else (countSmall str)

countOther::[Char]->Int
countOther [] = 0
countOther (ch:str) = if ((ord ch)<65 || ((ord ch)>90 && (ord ch)<97) || (ord ch)>122)
                     then 1 + (countOther str)
                     else (countOther str)

main :: IO()
main = do
    line <- getLine    
    print (countSmall line)
    print (countBig line)
    print (countOther line)
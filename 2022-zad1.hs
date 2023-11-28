import Data.Char

-- function declaration for countVowels
countVowels::[Char]->Int

-- function definition for countVowels
-- base case
countVowels [] = 0
countVowels (ch:str) = if (ch=='a' || ch=='e' || ch=='i' || ch=='o' || ch=='u')
                     then 1 + (countVowels str)
                     else (countVowels str)
                     
-- function declaration for countConsonants
countConsonants::[Char]->Int

-- function definition for countConsonants
-- base case
countConsonants [] = 0
countConsonants (ch:str) = if (((ord ch)>=97 && (ord ch)<=122) && not (ch=='a' || ch=='e' || ch=='i' || ch=='o' || ch=='u'))
                        then 1 + (countConsonants str)
                        else (countConsonants str)

countBig [] = 0
countBig (ch:str) = if ((ord ch)>=65 && (ord ch)<=90)
                     then 1 + (countBig str)
                     else (countBig str)

countSmall [] = 0
countSmall (ch:str) = if ((ord ch)>=97 && (ord ch)<=122)
                     then 1 + (countSmall str)
                     else (countSmall str)

countOther [] = 0
countOther (ch:str) = if ((ord ch)<65 || ((ord ch)>90 && (ord ch)<97) || (ord ch)>122)
                     then 1 + (countOther str)
                     else (countOther str)

main :: IO()
main = do
-- declaring and initializing the variable for the sentence
   let input = "Thank you for visiting Tutorialspoint ="
-- case conversion of the input string to lowercase
   let sentence = map toLower input
-- invoking the function countVowels with the argument sentence and printing the returned result
   print ("The number of Big in the sentence: "++ input)
   print (countBig input)
   print ("The number of Small in the sentence: "++ input)
   print (countSmall input)
   print ("The number of Other in the sentence: "++ input)
   print (countOther input)
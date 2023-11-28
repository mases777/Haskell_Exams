import Data.List

avg :: (Integral a, Fractional b) => [a] -> b
avg xs = g $ foldl" c (0,0) xs
where
c (!a,!n) x = (a+x,n+1)
g (a,n) = fromIntegral a / fromIntegral n
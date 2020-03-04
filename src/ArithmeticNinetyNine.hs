module ArithmeticNinetyNine
    ( 
        isPrime
    ) where

import ListNinetyNine    

isPrime :: Int -> Bool
isPrime x
    | x < 2 = False
    | otherwise = not (any (\y -> (x `mod` y) == 0) (range 2 (x - 1)))
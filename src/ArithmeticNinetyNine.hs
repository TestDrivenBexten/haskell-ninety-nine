module ArithmeticNinetyNine
    ( 
        isPrime,
        myGcd,
        coprime,
        totient,
        primeFactors
    ) where

import ListNinetyNine
import Data.List 

isPrime :: Int -> Bool
isPrime x
    | x < 2 = False
    | otherwise = not (any (\y -> (x `mod` y) == 0) (range 2 (x - 1)))

myGcd :: Int -> Int -> Int
myGcd x y
    | y == 0 = x
    | x == 0 = y
    | x == y = x
    | x > y = myGcd y (x `mod` y)
    | x < y = myGcd x (y `mod` x)

coprime :: Int -> Int -> Bool
coprime x y = myGcd x y == 1

totient :: Int -> Int
totient x = length (filter (\y -> coprime x y) (range 1 (x - 1)))

primeFactors :: Int -> [Int]
primeFactors x
    | isPrime x = [x]
    | otherwise = [firstDivisor] ++ primeFactors (div x firstDivisor)
    where
        firstDivisor = head divisors
        divisors = filter (\y -> x `mod` y == 0) (range 2 x)
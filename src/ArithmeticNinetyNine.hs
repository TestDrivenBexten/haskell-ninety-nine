module ArithmeticNinetyNine
    ( 
        isPrime,
        myGcd,
        coprime,
        totient,
        primeFactors,
        primeFactorsMult,
        eulerTotientImproved,
        primeRange,
        goldbach,
        goldbachList,
        goldbachListMin
    ) where

import ListNinetyNine
import Data.List 

isPrime :: Int -> Bool
isPrime x
    | x < 2 = False
    | otherwise = not $ any (\y -> (x `mod` y) == 0) $ range 2 (x - 1)

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
totient x = 
    let numRange = range 1 (x - 1)
        in length [ y | y <- numRange, coprime x y]

primeFactors :: Int -> [Int]
primeFactors x
    | isPrime x = [x]
    | otherwise = [firstDivisor] ++ primeFactors (div x firstDivisor)
    where
        firstDivisor = head divisors
        divisors = filter (\y -> x `mod` y == 0) (range 2 x)

primeFactorsMult :: Int -> [(Int,Int)]
primeFactorsMult x = map (\x -> (snd x,fst x)) (encode (primeFactors x))

eulerTotientImproved :: Int -> Int
eulerTotientImproved x = foldl (*) 1 phiFactorList
    where
        phiFactorList = map (\y -> (fst y - 1) * (fst y ^ (snd y - 1))) primeFactorList
        primeFactorList = primeFactorsMult x

primeRange :: Int -> Int -> [Int]
primeRange x y = filter isPrime $ range x y

goldbach :: Int -> (Int, Int)
goldbach x = head bothPrimeList
    where
        bothPrimeList = [ (y,z) | (y,z) <- primePairList, isPrime y && isPrime z ]
        primePairList = map (\y -> (y,x - y)) primeList
        primeList = primeRange 2 x

goldbachList :: Int -> Int -> [(Int,Int)]
goldbachList start end = map goldbach evenList
    where
        evenList = [ x | x <- [start..end], x `mod` 2 == 0]

goldbachListMin :: Int -> Int -> Int -> [(Int,Int)]
goldbachListMin start end min = filter (\pair -> (fst pair > min) && (snd pair > min)) (goldbachList start end)
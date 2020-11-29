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
import Data.Tuple (swap)

isPrime :: Int -> Bool
isPrime x
    | x < 2 = False
    | otherwise = not $ any (isDivisible x) $ range 2 (x - 1)

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
        divisors = filter (isDivisible x) (range 2 x)

isDivisible :: Int -> Int -> Bool
isDivisible x y = x `mod` y == 0

primeFactorsMult :: Int -> [(Int,Int)]
primeFactorsMult x = map swap (encode (primeFactors x))

eulerTotientImproved :: Int -> Int
eulerTotientImproved x = foldl (*) 1 phiFactorList
    where
        phiFactorList = map (\y -> (fst y - 1) * (fst y ^ (snd y - 1))) primeFactorList
        primeFactorList = primeFactorsMult x

primeRange :: Int -> Int -> [Int]
primeRange x y = filter isPrime $ range x y

goldbach :: Int -> (Int, Int)
goldbach x = head [ (a,b) | (a,b) <- pairList, isPrime a && isPrime b]
    where
        pairList = map (\y -> (y,x - y)) $ primeRange 2 x

goldbachList :: Int -> Int -> [(Int,Int)]
goldbachList start end = map goldbach evenList
    where
        evenList = [ x | x <- [start..end], x `mod` 2 == 0]

goldbachListMin :: Int -> Int -> Int -> [(Int,Int)]
goldbachListMin start end min = 
    let aboveMin = (\x -> x > min)
        in [ (a,b) | (a,b) <- goldList, aboveMin a && aboveMin b]
    where
        goldList = goldbachList start end

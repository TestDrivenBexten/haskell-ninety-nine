module ArithmeticSpec (spec) where

import Test.Hspec
import ArithmeticNinetyNine

spec :: Spec
spec = do
    describe "When checking whether a number is prime" $ do
        it "Zero is not prime" $ do
            isPrime 0 `shouldBe` False

        it "One is not prime" $ do
            isPrime 1 `shouldBe` False

        it "Two is prime" $ do
            isPrime 2 `shouldBe` True
        
        it "Twelve is not prime" $ do
            isPrime 12 `shouldBe` False
            
        it "Thirteen is prime" $ do
            isPrime 13 `shouldBe` True

    describe "When finding the greatest common divisor of two numbers" $ do
        it "GCD of 3 and 6 should be 3" $ do
            myGcd 3 6 `shouldBe` 3

        it "GCD of 36 and 63 should be 9" $ do
            myGcd 36 63 `shouldBe` 9
        
        it "GCD of 63 and 36 should be 9" $ do
            myGcd 63 36 `shouldBe` 9
        
        it "GCD of 4 and 4 should be 4" $ do
            myGcd 4 4 `shouldBe` 4

    describe "When checking two numbers as coprimes" $ do
        it "35 and 64 are coprime" $ do
            coprime 35 64 `shouldBe` True

        it "36 and 63 are not coprime" $ do
            coprime 36 63 `shouldBe` False

    describe "When counting coprime numbers" $ do
        -- 10 : 1, 3, 7, 9
        it "10 should have 4 coprimes" $ do
            totient 10 `shouldBe` 4

        it "11 should have 10 coprimes" $ do
            totient 11 `shouldBe` 10

    describe "When finding prime factors" $ do
        it "5 should be [5]" $ do
            primeFactors 5 `shouldBe` [5]
            
        it "15 should be [3,5]" $ do
            primeFactors 15 `shouldBe` [3,5]
            
        it "315 should be [3,3,5,7]" $ do
            primeFactors 315 `shouldBe` [3,3,5,7]

    describe "When finding frequency of prime numbers" $ do
        it "5 should be [5]" $ do
            primeFactorsMult 5 `shouldBe` [(5,1)]
            
        it "15 should be [3,5]" $ do
            primeFactorsMult 15 `shouldBe` [(3,1),(5,1)]
            
        it "315 should be [3,3,5,7]" $ do
            primeFactorsMult 315 `shouldBe` [(3,2),(5,1),(7,1)]

    describe "When counting coprime numbers" $ do
        -- 10 : 1, 3, 7, 9
        it "10 should have 4 coprimes" $ do
            eulerTotientImproved 10 `shouldBe` 4

        it "11 should have 10 coprimes" $ do
            eulerTotientImproved 11 `shouldBe` 10

    -- Skip problem 38

    describe "When building a list of prime numbers" $ do
        it "The range 10 to 20 should give [11,13,17,19]" $ do
            primeRange 10 20 `shouldBe` [11,13,17,19]

        it "The range 3 to 9 should give [3,5,7]" $ do
            primeRange 3 9 `shouldBe` [3,5,7]

    describe "When working with Goldbach's conjecture" $ do
        it "The number 28 should give (5, 23)" $ do
            goldbach 28 `shouldBe` (5,23)

        it "The number 10 should give (3, 7)" $ do
            goldbach 10 `shouldBe` (3,7)

    describe "When working with Goldbach lists" $ do
        it "Should have 4 pairs from 9 to 16" $ do
            goldbachList 9 16 `shouldBe` [(3,7),(5,7),(3,11),(3,13)]

        it "Should have 4 pairs for 4 to 2000 with a minimum of 50" $ do
            goldbachListMin 4 2000 50 `shouldBe` [(73,919),(61,1321),(67,1789),(61,1867)]
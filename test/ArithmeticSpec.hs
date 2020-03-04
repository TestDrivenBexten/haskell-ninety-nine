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
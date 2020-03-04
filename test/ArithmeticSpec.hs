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
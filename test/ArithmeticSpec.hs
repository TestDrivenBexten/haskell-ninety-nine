module ArithmeticSpec (spec) where

import Test.Hspec
import ArithmeticNinetyNine

spec :: Spec
spec = hspec $ do
    describe "When checking whether a number is prime" $ do
        it "Zero is not prime" $ do
            isPrime 0 `shouldBe` False
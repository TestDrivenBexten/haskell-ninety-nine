module LogicSpec (spec) where

import Test.Hspec
import LogicNinetyNine

spec :: Spec
spec = do
    describe "Given the AND operator" $ do
        it "T T Should return T" $ do
            myAnd True True `shouldBe` True
        it "T F Should return F" $ do
            myAnd True False `shouldBe` False
        it "F T Should return F" $ do
            myAnd False True `shouldBe` False
        it "F F Should return F" $ do
            myAnd False False `shouldBe` False
            
    describe "Given the OR operator" $ do
        it "T T Should return T" $ do
            myOr True True `shouldBe` True
        it "T F Should return T" $ do
            myOr True False `shouldBe` True
        it "F T Should return T" $ do
            myOr False True `shouldBe` True
        it "F F Should return F" $ do
            myOr False False `shouldBe` False
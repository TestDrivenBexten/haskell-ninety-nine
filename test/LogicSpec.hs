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
                        
    describe "Given the NAND operator" $ do
        it "T T Should return F" $ do
            myNand True True `shouldBe` False
        it "T F Should return T" $ do
            myNand True False `shouldBe` True
        it "F T Should return T" $ do
            myNand False True `shouldBe` True
        it "F F Should return T" $ do
            myNand False False `shouldBe` True
                        
    describe "Given the NOR operator" $ do
        it "T T Should return F" $ do
            myNor True True `shouldBe` False
        it "T F Should return F" $ do
            myNor True False `shouldBe` False
        it "F T Should return F" $ do
            myNor False True `shouldBe` False
        it "F F Should return T" $ do
            myNor False False `shouldBe` True
                
    describe "Given the XOR operator" $ do
        it "T T Should return T" $ do
            myXor True True `shouldBe` True
        it "T F Should return F" $ do
            myXor True False `shouldBe` False
        it "F T Should return F" $ do
            myXor False True `shouldBe` False
        it "F F Should return F" $ do
            myXor False False `shouldBe` False
    
    describe "Given the IMPLIES operator" $ do
        it "T T Should return T" $ do
            myImplies True True `shouldBe` True
        it "T F Should return F" $ do
            myImplies True False `shouldBe` False
        it "F T Should return T" $ do
            myImplies False True `shouldBe` True
        it "F F Should return T" $ do
            myImplies False False `shouldBe` True
                
    describe "Given the EQUAL operator" $ do
        it "T T Should return T" $ do
            myEqual True True `shouldBe` True
        it "T F Should return F" $ do
            myEqual True False `shouldBe` False
        it "F T Should return F" $ do
            myEqual False True `shouldBe` False
        it "F F Should return T" $ do
            myEqual False False `shouldBe` True

    describe "When finding gray codes" $ do
        it "C=1 should give [0,1]" $ do
            gray 1 `shouldBe` ["0","1"]
            
        it "C=3 should give [000,001,010,011,100,101,110,111]" $ do
            gray 3 `shouldBe` ["000","001","010","011","100","101","110","111"]

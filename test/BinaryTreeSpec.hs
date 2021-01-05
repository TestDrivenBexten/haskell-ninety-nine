module BinaryTreeSpec (spec) where

import Test.Hspec
import BinaryTree

spec :: Spec
spec = do
    describe "P55. Construct balanced binary trees" $ do
        it "Balanced tree with 1 node should have one permutation" $ do
            cBal 1 `shouldBe` [Branch 'x' Empty Empty]

        it "Balanced tree with 2 nodes should have two permutations" $ do
            cBal 2 `shouldBe` [Branch 'x' (Branch 'x' Empty Empty) Empty,
                Branch 'x' Empty (Branch 'x' Empty Empty)]

        it "Balanced tree with 3 nodes should have one permutation" $ do
            cBal 3 `shouldBe` [Branch 'x' (Branch 'x' Empty Empty) 
                                            (Branch 'x' Empty Empty)]
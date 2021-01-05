module BinaryTreeSpec (spec) where

import Test.Hspec
import BinaryTree

spec :: Spec
spec = do
    describe "P55. Construct balanced binary trees" $ do
        let baseNode = Branch 'x' Empty Empty
        it "Balanced tree with 1 node should have one permutation" $ do
            cBal 1 `shouldBe` [baseNode]

        it "Balanced tree with 2 nodes should have two permutations" $ do
            cBal 2 `shouldBe` [Branch 'x' baseNode Empty,
                Branch 'x' Empty baseNode]

        it "Balanced tree with 3 nodes should have one permutation" $ do
            cBal 3 `shouldBe` [Branch 'x' baseNode baseNode]

        it "Balanced tree with 4 nodes should have four permutations" $ do
            cBal 4 `shouldBe` [Branch 'x' (Branch 'x' baseNode Empty) baseNode,
                Branch 'x' (Branch 'x' Empty baseNode) baseNode,
                Branch 'x' baseNode (Branch 'x' baseNode Empty),
                Branch 'x' baseNode (Branch 'x' Empty baseNode)]

        it "Balanced tree with 16 nodes should have 16 permutations" $ do
            length (cBal 16) `shouldBe` 16
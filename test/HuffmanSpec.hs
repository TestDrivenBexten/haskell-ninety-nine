module HuffmanSpec (spec) where

import Test.Hspec
import Huffman

spec :: Spec
spec = do
    let frequencyList = [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
    describe "P50. Determine Huffman Coding for Character Frequency" $ do
        it "Sample with six character frequencies" $ do
            huffman frequencyList `shouldBe`
                [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]

    describe "Frequency List to Tree" $ do
        it "A single frequency should build a leaf node" $ do
            buildHuffmanTree [('e',52)] `shouldBe` Leaf ('e',52)

        describe "Given frequency d = 10 and c = 5" $ do
            let huffmanTree = buildHuffmanTree [('d',10),('c',5)]
            it "Then the parent node should have a value of 15" $ do
                treeTotal huffmanTree `shouldBe` 15

        describe "Given huffman tree for sample frequencies" $ do
            let huffmanTree = buildHuffmanTree frequencyList
            it "Then the tree should have total of 58" $ do
                treeTotal huffmanTree `shouldBe` 100
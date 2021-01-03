module HuffmanSpec (spec) where

import Test.Hspec
import Huffman

spec :: Spec
spec = do
    describe "P50. Determine Huffman Coding for Character Frequency" $ do
        it "Sample with six character frequencies" $ do
            huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
                `shouldBe` [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]
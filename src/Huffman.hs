module Huffman (
    huffman,
    buildHuffmanTree,
    Tree(Leaf,Branch),
    treeTotal
)  where

type CharFrequency = (Char,Int)
data Tree = Leaf { value :: CharFrequency } |
    Branch { leftChild :: Tree, rightChild :: Tree }

huffman :: [(Char,Int)] -> [(Char,String)]
huffman xs = [('a',"01")]

buildHuffmanTree :: [CharFrequency] -> Tree
buildHuffmanTree xs = Leaf ('a',1)

treeTotal :: Tree -> Int
treeTotal (Leaf x) = 5
treeTotal (Branch left right) = 5
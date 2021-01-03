module Huffman (
    huffman,
    buildHuffmanTree,
    Tree(Leaf,Branch),
    treeTotal
)  where

type CharFrequency = (Char,Int)
data Tree = Leaf CharFrequency |
    Branch { leftChild :: Tree, rightChild :: Tree }

huffman :: [(Char,Int)] -> [(Char,String)]
huffman xs = [('a',"01")]

buildHuffmanTree :: [CharFrequency] -> Tree
buildHuffmanTree [x,y] = Branch (Leaf x) (Leaf y)

treeTotal :: Tree -> Int
treeTotal (Leaf x) = snd x
treeTotal (Branch left right) = treeTotal left + treeTotal right
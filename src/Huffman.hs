module Huffman (
    huffman,
    buildHuffmanTree,
    Tree(Leaf,Branch),
    treeTotal
)  where

type CharFrequency = (Char,Int)
data Tree = Empty | Leaf CharFrequency |
    Branch { leftChild :: Tree, rightChild :: Tree } deriving (Eq, Show)

huffman :: [(Char,Int)] -> [(Char,String)]
huffman xs = [('a',"01")]

buildHuffmanTree :: [CharFrequency] -> Tree
buildHuffmanTree xs = combineHuffmanTree [ Leaf x | x <- xs ]

treeTotal :: Tree -> Int
treeTotal (Leaf x) = snd x
treeTotal (Branch left right) = treeTotal left + treeTotal right

combineHuffmanTree :: [Tree] -> Tree
combineHuffmanTree [x] = x
combineHuffmanTree xs = combineHuffmanTree (tail xs)
module Huffman (
    huffman,
    buildHuffmanTree,
    Tree(Leaf,Branch),
    treeTotal
)  where

import Data.List (sortBy)

type CharFrequency = (Char,Int)
data Tree = Empty | Leaf CharFrequency |
    Branch { leftChild :: Tree, rightChild :: Tree } deriving (Eq, Show)

huffman :: [(Char,Int)] -> [(Char,String)]
huffman xs = [('a',"01")]

buildHuffmanTree :: [CharFrequency] -> Tree
buildHuffmanTree xs =
    let sortedLeafList = sortBy compareTree [ Leaf x | x <- xs ]
        in combineHuffmanTree sortedLeafList

treeTotal :: Tree -> Int
treeTotal (Leaf x) = snd x
treeTotal (Branch left right) = treeTotal left + treeTotal right

combineHuffmanTree :: [Tree] -> Tree
combineHuffmanTree [x] = x
combineHuffmanTree xs = combineHuffmanTree (tail xs)

compareTree :: Tree -> Tree -> Ordering
compareTree x y
    | treeTotal x < treeTotal y = LT
    | otherwise = GT
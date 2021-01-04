module Huffman (
    huffman,
    buildHuffmanTree,
    Tree(Leaf,Branch),
    treeTotal
)  where

import Data.List (sortBy)

type CharFrequency = (Char,Int)
type CharEncoding = (Char,String)
data Tree = Leaf CharFrequency |
    Branch { leftChild :: Tree, rightChild :: Tree } deriving (Eq, Show)

huffman :: [CharFrequency] -> [CharEncoding]
huffman xs = sortBy compareCharEncoding $ encodeHuffmanTree "" $ buildHuffmanTree xs

compareCharEncoding :: CharEncoding -> CharEncoding -> Ordering
compareCharEncoding (char1,_) (char2,_)
    | char1 < char2 = LT
    | otherwise = GT

encodeHuffmanTree :: String -> Tree -> [(Char, String)]
encodeHuffmanTree encoding (Leaf x) = [(fst x, encoding)]
encodeHuffmanTree encoding (Branch left right) =
    encodeHuffmanTree (encoding ++ "0") left ++ encodeHuffmanTree (encoding ++ "1") right

buildHuffmanTree :: [CharFrequency] -> Tree
buildHuffmanTree xs =
    let sortedLeafList = sortBy compareTree [ Leaf x | x <- xs ]
        in combineHuffmanTree sortedLeafList

treeTotal :: Tree -> Int
treeTotal (Leaf x) = snd x
treeTotal (Branch left right) = treeTotal left + treeTotal right

combineHuffmanTree :: [Tree] -> Tree
combineHuffmanTree [x] = x
combineHuffmanTree [x,y] = Branch x y
combineHuffmanTree (x:y:xs) =
    let newBranch = combineHuffmanTree [x,y]
        sortedTreeList = sortBy compareTree (newBranch:xs)
        in combineHuffmanTree sortedTreeList

compareTree :: Tree -> Tree -> Ordering
compareTree x y
    | treeTotal x < treeTotal y = LT
    | otherwise = GT
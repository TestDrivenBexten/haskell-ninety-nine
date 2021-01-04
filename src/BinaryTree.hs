module BinaryTree (
    Tree(Empty,Branch),
    cBal
) where

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Eq, Show)

cBal :: Int -> [Tree Char]
cBal nodeCount = [Branch 'x' Empty Empty]
module BinaryTree (
    Tree(Empty,Branch),
    cBal
) where

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Eq, Show)

cBal :: Int -> [Tree Char]
cBal 1 = [Branch 'x' Empty Empty]
cBal 2 = [Branch 'x' (Branch 'x' Empty Empty) Empty,
            Branch 'x' Empty (Branch 'x' Empty Empty)]
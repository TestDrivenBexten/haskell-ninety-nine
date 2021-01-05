module BinaryTree (
    Tree(Empty,Branch),
    cBal
) where

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Eq, Show)

xNode = Branch 'x' Empty Empty

cBal :: Int -> [Tree Char]
cBal 1 = [xNode]
cBal 2 = [Branch 'x' xNode Empty, Branch 'x' Empty xNode]
cBal 3 = [Branch 'x' xNode xNode]
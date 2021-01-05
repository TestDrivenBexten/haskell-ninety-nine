module BinaryTree (
    Tree(Empty,Branch),
    cBal
) where

data Tree a = Empty |
    Branch { nodeValue :: a, leftChild :: (Tree a), rightChild :: (Tree a) }
    deriving (Eq, Show)

xNode = Branch 'x' Empty Empty

cBal :: Int -> [Tree Char]
cBal 1 = [xNode]
cBal 2 = [Branch 'x' xNode Empty, Branch 'x' Empty xNode]
cBal 3 = [Branch 'x' xNode xNode]
cBal 4 = [ treeBase | treeBase <- (cBal 3), newLeaf <- (cBal 2)]
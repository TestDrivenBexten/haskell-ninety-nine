module ListNinetyNine
    ( lastInList,
      penultimateInList,
      kthInList,
      numElements,
      reverseList,
      isPalindrome,
      trimEnds,
      flattenList,
      Tree(Node,Branch)
    ) where

lastInList :: [a] -> a
lastInList [] = error "Empty list"
lastInList [x] = x
lastInList (_:xs) = lastInList xs

penultimateInList :: [a] -> a
penultimateInList [] = error "Empty list"
penultimateInList [x] = error "List requires two or more elements"
penultimateInList [x,y] = x
penultimateInList (_:xs) = penultimateInList xs

kthInList :: [a] -> Int -> a
kthInList [] x = error "Empty list"
kthInList xs x = xs !! (x - 1)

numElements :: [a] -> Int
numElements [] = 0
numElements [x] = 1
numElements (_:xs) = 1 + numElements xs

reverseList :: [a] -> [a]
reverseList [] = []
reverseList [x] = [x]
reverseList [x,y] = [y,x]
reverseList (x:xs) = (reverseList xs) ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome (xs) = (head xs == lastInList xs) && isPalindrome (trimEnds xs)

trimEnds :: [a] -> [a]
trimEnds x = drop 1 (take (numElements x - 1) x)

-- A nested list pretty much boils down to a tree
data Tree a = Node a | Branch [Tree a]
flattenList :: Tree a -> [a]
flattenList (Node x) = [x]
flattenList (Branch []) = []
flattenList (Branch [x]) = flattenList x
flattenList (Branch (x:xs)) = flattenList x